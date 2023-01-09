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
import Ledger             ( DecoratedTxOut, TxOutRef
                          , decoratedTxOutValue, getDatum
                          , unPaymentPubKeyHash
                          )
import Ledger.Constraints ( plutusV1MintingPolicy, mustBeSignedBy
                          , mustMintValue
                          , mustPayToTheScriptWithDatumInTx
                          , mustSpendScriptOutput
                          , plutusV1OtherScript, typedValidatorLookups
                          , unspentOutputs
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
                        , eInfoSenderWallAddr, valueToSender, signerIsSender
                        )
import Escrow.Validator ( Escrowing
                        , escrowAddress, escrowValidator, escrowInst
                        , controlTokenCurrency, controlTokenMP
                        )
import Escrow.Types   ( eInfo, cTokenName, mkEscrowDatum
                      , cancelRedeemer, resolveRedeemer
                      )
import Utils.OffChain ( lookupScriptUtxos, findMUtxo, getDatumWithError )
import Utils.OnChain  ( minAda )
import Utils.WalletAddress ( WalletAddress
                           , waPaymentPubKeyHash
                           , mustPayToWalletAddress
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
              , plutusV1OtherScript validator
              , plutusV1MintingPolicy (controlTokenMP contractAddress)
              ]
        tx  = mconcat
              [ mustPayToTheScriptWithDatumInTx datum val
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

    utxos <- lookupScriptUtxos contractAddress cTokenAsset
    utxo  <- findMUtxo cpTxOutRef utxos
    eInfo <- getEscrowInfo utxo

    unless (signerIsSender (unPaymentPubKeyHash senderPpkh) (sender eInfo))
           (throwError "Sender address invalid")

    let lkp = mconcat
            [ plutusV1OtherScript validator
            , unspentOutputs (singleton cpTxOutRef utxo)
            , plutusV1MintingPolicy (controlTokenMP contractAddress)
            ]
        tx = mconcat
            [ mustSpendScriptOutput cpTxOutRef cancelRedeemer
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

    utxos <- lookupScriptUtxos contractAddress cTokenAsset
    utxo  <- findMUtxo rpTxOutRef utxos
    eInfo <- getEscrowInfo utxo

    let senderWallAddr = eInfoSenderWallAddr eInfo
        senderPayment  = valueToSender eInfo <> minAda

        lkp = mconcat
            [ plutusV1OtherScript validator
            , unspentOutputs (singleton rpTxOutRef utxo)
            , plutusV1MintingPolicy (controlTokenMP contractAddress)
            ]
        tx = mconcat
            [ mustSpendScriptOutput rpTxOutRef resolveRedeemer
            , mustMintValue cTokenVal
            , mustBeSignedBy receiverPpkh
            , mustPayToWalletAddress senderWallAddr senderPayment
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
         .  (TxOutRef, DecoratedTxOut)
         -> Contract w s Text UtxoEscrowInfo
     mkEscrowInfo (utxoRef, dtxout) =
         mkUtxoEscrowInfo utxoRef (dtxout ^. decoratedTxOutValue)
         <$> getEscrowInfo dtxout

{- | Off-chain function for getting the Typed Datum (EscrowInfo) from a
     DecoratedTxOut.
-}
getEscrowInfo
    :: forall w s
    .  DecoratedTxOut
    -> Contract w s Text EscrowInfo
getEscrowInfo txOut = getDatumWithError txOut >>=
                      maybe (throwError "Datum format invalid")
                            (pure . eInfo) . (fromBuiltinData . getDatum)
