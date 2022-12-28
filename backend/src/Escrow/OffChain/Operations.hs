{-|
Module      : Escrow.OffChain.Actions
Description : OffChain actions for Escrow Contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define here the interface for accessing the contract functions through
an Schema. The main offchain functions implement all the logic for
building unbalanced transactions.
-}

module Escrow.OffChain.Operations
    (-- * Escrow schema
      EscrowSchema
    -- * Endpoints
    , endpoints
    )
where

-- Non-IOG imports
import Control.Lens  ( (^.) )
import Control.Monad ( forever, unless )
import Data.Map      ( singleton, toList )
import Data.Text     ( Text )
import Data.Monoid   ( Last(..) )

-- IOG imports
import Ledger             ( ChainIndexTxOut, TxOutRef, Datum, DatumHash
                          , ciTxOutValue, getDatum, ciTxOutDatum
                          , unPaymentPubKeyHash
                          )
import Ledger.Constraints ( mintingPolicy, mustBeSignedBy, mustMintValue
                          , mustPayToPubKey, mustPayToTheScript
                          , mustSpendScriptOutput, otherScript
                          , typedValidatorLookups, unspentOutputs
                          )
import Ledger.Value       ( assetClass, assetClassValue )
import Plutus.Contract    ( Contract, Promise, awaitPromise, endpoint
                          , handleError, logError, logInfo, mkTxConstraints
                          , select, tell, throwError
                          , utxosAt, yieldUnbalancedTx
                          )
import PlutusTx           ( fromBuiltinData )

-- Escrow imports
import Escrow.OffChain.Interface ( StartParams(..), CancelParams(..)
                                 , ResolveParams(..), UtxoEscrowInfo
                                 , EscrowSchema
                                 , mkUtxoEscrowInfo
                                 )
import Escrow.Business  ( EscrowInfo(..)
                        , mkSenderAddress, mkReceiverAddress
                        , eInfoSenderAddr, valueToSender, signerIsSender
                        )
import Escrow.Validator ( Escrowing
                        , escrowAddress, escrowValidator, escrowInst
                        , controlTokenCurrency, controlTokenMP
                        )
import Escrow.Types   ( eInfo, cTokenName, mkEscrowDatum
                      , cancelRedeemer, resolveRedeemer
                      )
import Utils.OffChain ( getPpkhFromAddress
                      , lookupScriptUtxos, getDatumWithError
                      )
import Utils.OnChain  ( minAda )
import Utils.WalletAddress ( WalletAddress
                           , waPaymentPubKeyHash
                           )

endpoints
    :: WalletAddress
    -> Contract (Last [UtxoEscrowInfo]) EscrowSchema Text ()
endpoints raddr = forever $ handleError logError $ awaitPromise $
                  startEp `select` cancelEp `select` resolveEp `select` reloadEp
  where
    startEp :: Promise (Last [UtxoEscrowInfo]) EscrowSchema Text ()
    startEp = endpoint @"start" $ startOp raddr

    cancelEp :: Promise (Last [UtxoEscrowInfo]) EscrowSchema Text ()
    cancelEp = endpoint @"cancel" $ cancelOp raddr

    resolveEp :: Promise (Last [UtxoEscrowInfo]) EscrowSchema Text ()
    resolveEp = endpoint @"resolve" $ resolveOp raddr

    reloadEp :: Promise (Last [UtxoEscrowInfo]) EscrowSchema Text ()
    reloadEp = endpoint @"reload" $ const $ reloadOp raddr

{- | A user, using its `addr`, locks the tokens they want to exchange and
     specifies the tokens they want to receive and from whom, all these
     information is contained on `sParams`. The control Token is minted.
-}
startOp
    :: forall w s
    .  WalletAddress
    -> StartParams
    -> Contract w s Text ()
startOp addr StartParams{..} = do

    let senderPpkh      = waPaymentPubKeyHash addr
        contractAddress = escrowAddress receiverAddress
        validator       = escrowValidator receiverAddress

        cTokenCurrency = controlTokenCurrency contractAddress
        cTokenAsset    = assetClass cTokenCurrency cTokenName
        cTokenVal      = assetClassValue cTokenAsset 1

        senderVal = assetClassValue sendAssetClass sendAmount
        val       = minAda <> cTokenVal <> senderVal
        datum     = mkEscrowDatum (mkSenderAddress addr)
                                  receiveAmount
                                  receiveAssetClass
                                  cTokenAsset

        lkp = mconcat
              [ typedValidatorLookups (escrowInst receiverAddress)
              , otherScript validator
              , mintingPolicy (controlTokenMP contractAddress)
              ]
        tx  = mconcat
              [ mustPayToTheScript datum val
              , mustMintValue cTokenVal
              , mustBeSignedBy senderPpkh
              ]

    mkTxConstraints lkp tx >>= yieldUnbalancedTx
    logInfo @String "Escrow started"
    logInfo @String $ "Escrow Address: " ++ show contractAddress
    logInfo @String $ "Control Token Currency Symbol: " ++ show cTokenCurrency

{- | The user, using its `addr`, cancels the escrow placed on the utxo
     referenced on `cParams`, to receive the locked tokens back.
     The control Token is burned.
-}
cancelOp
    :: forall w s
    .  WalletAddress
    -> CancelParams
    -> Contract w s Text ()
cancelOp addr CancelParams{..} = do

    let senderPpkh      = waPaymentPubKeyHash addr
        contractAddress = escrowAddress cpReceiverAddress
        validator       = escrowValidator cpReceiverAddress

        cTokenCurrency = controlTokenCurrency contractAddress
        cTokenAsset    = assetClass cTokenCurrency cTokenName
        cTokenVal      = assetClassValue cTokenAsset (-1)

    utxos       <- lookupScriptUtxos contractAddress cTokenAsset
    (ref, utxo) <- findEscrowUtxo cpTxOutRef utxos
    eInfo       <- getEscrowInfo utxo

    unless (signerIsSender (unPaymentPubKeyHash senderPpkh) (sender eInfo))
           (throwError "Sender address invalid")

    let lkp = mconcat
            [ otherScript validator
            , unspentOutputs (singleton ref utxo)
            , mintingPolicy (controlTokenMP contractAddress)
            ]
        tx = mconcat
            [ mustSpendScriptOutput ref cancelRedeemer
            , mustMintValue cTokenVal
            , mustBeSignedBy senderPpkh
            ]

    mkTxConstraints @Escrowing lkp tx >>= yieldUnbalancedTx
    logInfo @String "Escrow canceled"
    logInfo @String $ "Escrow Address: " ++ show contractAddress
    logInfo @String $ "Control Token Currency Symbol: " ++ show cTokenCurrency

{- | The user, using its `addr`, resolves the escrow placed on the utxo
     referenced on `rParams`, to pay the corresponding tokens to the other user
     and to receive the locked tokens.
-}
resolveOp
    :: forall w s
    .  WalletAddress
    -> ResolveParams
    -> Contract w s Text ()
resolveOp addr ResolveParams{..} = do

    let receiverPpkh    = waPaymentPubKeyHash addr
        contractAddress = escrowAddress (mkReceiverAddress addr)
        validator       = escrowValidator (mkReceiverAddress addr)

        cTokenCurrency = controlTokenCurrency contractAddress
        cTokenAsset    = assetClass cTokenCurrency cTokenName
        cTokenVal      = assetClassValue cTokenAsset (-1)

    utxos       <- lookupScriptUtxos contractAddress cTokenAsset
    (ref, utxo) <- findEscrowUtxo rpTxOutRef utxos
    eInfo       <- getEscrowInfo utxo
    senderPpkh  <- getPpkhFromAddress (eInfoSenderAddr eInfo)

    let senderPayment = valueToSender eInfo <> minAda

        lkp = mconcat
            [ otherScript validator
            , unspentOutputs (singleton ref utxo)
            , mintingPolicy (controlTokenMP contractAddress)
            ]
        tx = mconcat
            [ mustSpendScriptOutput ref resolveRedeemer
            , mustMintValue cTokenVal
            , mustBeSignedBy receiverPpkh
            , mustPayToPubKey senderPpkh senderPayment
            ]

    mkTxConstraints @Escrowing lkp tx >>= yieldUnbalancedTx
    logInfo @String "Escrow resolved"
    logInfo @String $ "Escrow Address: " ++ show contractAddress
    logInfo @String $ "Control Token Currency Symbol: " ++ show cTokenCurrency

{- | The user, using its `addr`, obtains all the escrows that can resolve. That
     is the complete escrow info, together with the UTxO reference.
-}
reloadOp
    :: forall s
    .  WalletAddress
    -> Contract (Last [UtxoEscrowInfo]) s Text ()
reloadOp addr = do
    let contractAddress = escrowAddress $ mkReceiverAddress addr

    utxos      <- utxosAt contractAddress
    utxosEInfo <- mapM mkEscrowInfo $ toList utxos

    tell $ Last $ Just utxosEInfo
  where
     mkEscrowInfo
         :: forall w
         .  (TxOutRef, ChainIndexTxOut)
         -> Contract w s Text UtxoEscrowInfo
     mkEscrowInfo (utxoRef, citxout) =
         mkUtxoEscrowInfo utxoRef (citxout ^. ciTxOutValue)
         <$> getEscrowInfo citxout

{- | Off-chain function for getting the specific UTxO from a list of UTxOs by
     its TxOutRef.
-}
findEscrowUtxo
    :: forall w s
    .  TxOutRef
    -> [(TxOutRef, ChainIndexTxOut)]
    -> Contract w s Text (TxOutRef, ChainIndexTxOut)
findEscrowUtxo ref utxos =
    case filter ((==) ref . fst) utxos of
        [(oref, o)] -> (oref,) <$> ciTxOutDatum loadDatum o
        _           -> throwError "Specified Utxo not found"
  where
    loadDatum
        :: Either DatumHash Datum
        -> Contract w s Text (Either DatumHash Datum)
    loadDatum lhd@(Left dh) = maybe lhd Right <$> datumFromHash dh
    loadDatum d = return d

{- | Off-chain function for getting the Typed Datum (EscrowInfo) from a
     ChainIndexTxOut.
-}
getEscrowInfo
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text EscrowInfo
getEscrowInfo txOut = getDatumWithError txOut >>=
                      maybe (throwError "Datum format invalid")
                            (pure . eInfo) . (fromBuiltinData . getDatum)
