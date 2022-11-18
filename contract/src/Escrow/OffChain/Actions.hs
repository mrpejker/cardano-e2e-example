{-# LANGUAGE TupleSections #-}
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

module Escrow.OffChain.Actions
    (-- | Escrow schema
      EscrowSchema
    -- | Endpoints
    , endpoints
    )
where

-- Non-IOG imports
import Control.Lens  ( (^.) )
import Data.Map      ( singleton, toList )
import Data.Text     ( Text )
import Data.Monoid   ( Last(..) )
import Control.Monad ( forever, unless )

-- IOG imports
import Ledger             ( Address, ChainIndexTxOut, getDatum, TxOutRef
                          , ciTxOutValue
                          )
import Ledger.Constraints ( mintingPolicy, mustBeSignedBy, mustMintValue
                          , mustPayToPubKey, mustPayToTheScript
                          , mustSpendScriptOutput, otherScript
                          , typedValidatorLookups, unspentOutputs
                          )
import Ledger.Value       ( assetClass, assetClassValue )
import Plutus.Contract    ( awaitPromise, Contract, Endpoint, endpoint
                          , handleError, logError, logInfo, mkTxConstraints
                          , Promise, select, tell, throwError, type (.\/)
                          , utxosAt, yieldUnbalancedTx
                          )
import PlutusTx           ( fromBuiltinData )

-- Escrow imports
import Escrow.Business            ( EscrowInfo(..), mkSenderAddress
                                  , mkReceiverAddress, eInfoSenderAddr
                                  )
import Escrow.Validator           ( Escrowing
                                  , escrowAddress, escrowValidator, escrowInst
                                  , controlTokenCurrency, controlTokenMP
                                  )
import Escrow.OffChain.Parameters ( StartParams(..), CancelParams(..)
                                  , ResolveParams(..), ObservableState(..)
                                  )
import Escrow.Types               ( eInfo, cTokenName, mkEscrowDatum
                                  , cancelRedeemer, resolveRedeemer
                                  )
import Utils.OffChain             ( getPpkhFromAddress
                                  , lookupScriptUtxos, getDatumWithError
                                  )
import Utils.OnChain              ( minAda )

-- | Escrow Schema
type EscrowSchema = Endpoint "start"   StartParams
                .\/ Endpoint "cancel"  CancelParams
                .\/ Endpoint "resolve" ResolveParams
                .\/ Endpoint "reload" ()

endpoints
    :: Address
    -> Contract (Last ObservableState) EscrowSchema Text ()
endpoints raddr = forever $ handleError logError $ awaitPromise $
                  startEp `select` cancelEp `select` resolveEp `select` reloadEp
  where
    startEp :: Promise (Last ObservableState) EscrowSchema Text ()
    startEp = endpoint @"start" $ startOp raddr

    cancelEp :: Promise (Last ObservableState) EscrowSchema Text ()
    cancelEp = endpoint @"cancel" $ cancelOp raddr

    resolveEp :: Promise (Last ObservableState) EscrowSchema Text ()
    resolveEp = endpoint @"resolve" $ resolveOp raddr

    reloadEp :: Promise (Last ObservableState) EscrowSchema Text ()
    reloadEp = endpoint @"reload" $ const $ reloadOp raddr

reloadOp
    :: Address
    -> Contract (Last ObservableState) EscrowSchema Text ()
reloadOp addr = do
    let contractAddress = escrowAddress $ mkReceiverAddress addr

    utxos <- utxosAt contractAddress
    utxosWithEInfo <- mapM (\(ref, citxout) -> (ref, ,citxout ^. ciTxOutValue)
                                                <$> getEscrowInfo citxout)
                           (toList utxos)
    tell $ Last $ Just $ ObservableState utxosWithEInfo


{- | A user, using its `addr`, locks the tokens they want to exchange and
     specifies the tokens they want to receive and from whom, all these
     information is contained on `sParams`. The control Token is minted.
-}
startOp
    :: Address
    -> StartParams
    -> Contract (Last ObservableState) EscrowSchema Text ()
startOp addr StartParams{..} = do
    senderPpkh <- getPpkhFromAddress addr

    let contractAddress = escrowAddress receiverAddress
        validator       = escrowValidator receiverAddress

        cTokenCurrency  = controlTokenCurrency contractAddress
        cTokenAsset     = assetClass cTokenCurrency cTokenName
        cTokenVal       = assetClassValue cTokenAsset 1

        senderVal       = assetClassValue sendAssetClass sendAmount
        val             = minAda <> cTokenVal <> senderVal
        datum           = mkEscrowDatum (mkSenderAddress addr)
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
    :: Address
    -> CancelParams
    -> Contract (Last ObservableState) EscrowSchema Text ()
cancelOp addr CancelParams{..} = do
    senderPpkh   <- getPpkhFromAddress addr

    let contractAddress = escrowAddress (mkReceiverAddress cpReceiverAddress)
        validator       = escrowValidator (mkReceiverAddress cpReceiverAddress)

        cTokenCurrency  = controlTokenCurrency contractAddress
        cTokenAsset     = assetClass cTokenCurrency cTokenName
        cTokenVal       = assetClassValue cTokenAsset (-1)

    utxos        <- lookupScriptUtxos contractAddress cTokenAsset
    (ref, utxo)  <- findEscrowUtxo cpTxOutRef utxos
    eInfo        <- getEscrowInfo utxo

    unless (eInfoSenderAddr eInfo == addr)
           (throwError "Sender address invalid")

    let lkp = mconcat
            [ otherScript validator
            , unspentOutputs (singleton cpTxOutRef utxo)
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
    :: Address
    -> ResolveParams
    -> Contract (Last ObservableState) EscrowSchema Text ()
resolveOp addr ResolveParams{..} = do
    receiverPpkh <- getPpkhFromAddress addr

    let contractAddress = escrowAddress (mkReceiverAddress addr)
        validator       = escrowValidator (mkReceiverAddress addr)

        cTokenCurrency  = controlTokenCurrency contractAddress
        cTokenAsset     = assetClass cTokenCurrency cTokenName
        cTokenVal       = assetClassValue cTokenAsset (-1)

    utxos        <- lookupScriptUtxos contractAddress cTokenAsset
    (ref, utxo)  <- findEscrowUtxo rpTxOutRef utxos
    eInfo        <- getEscrowInfo utxo
    senderPpkh   <- getPpkhFromAddress (eInfoSenderAddr eInfo)

    let senderPayment = assetClassValue (rAssetClass eInfo) (rAmount eInfo)
                        <> minAda

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

{- | Off-chain function for getting the specific UTxO from a list of UTxOs by
     its TxOutRef.
-}
findEscrowUtxo
    :: TxOutRef
    -> [(TxOutRef, ChainIndexTxOut)]
    -> Contract (Last ObservableState) EscrowSchema Text (TxOutRef, ChainIndexTxOut)
findEscrowUtxo ref utxos =
    case filter ((==) ref . fst) utxos of
        [utxo] -> pure utxo
        _      -> throwError "Specified Utxo not found"

{- | Off-chain function for getting the Typed Datum (EscrowInfo) from a
     ChainIndexTxOut.
-}
getEscrowInfo
    :: ChainIndexTxOut
    -> Contract (Last ObservableState) EscrowSchema Text EscrowInfo
getEscrowInfo txOut = getDatumWithError txOut >>=
                      maybe (throwError "Datum format invalid")
                            (pure . eInfo) . (fromBuiltinData . getDatum)
