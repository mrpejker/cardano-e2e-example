{-|
Module      : Escrow.OffChain
Description : OffChain code for Escrow Contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define here the interface for accessing the contract functions through
an Schema. The main offchain functions implement all the logic for
building unbalanced transactions.
-}

module Escrow.OffChain where

-- Non-IOG imports
import Data.Aeson    (FromJSON, ToJSON)
import Data.Map      qualified as Map
import Data.Text     (Text)
import Control.Monad (forever)
import GHC.Generics  (Generic)

-- IOG imports
import Ledger
import Ledger.Constraints as Constraints
import Ledger.Value
import Plutus.Contract
import PlutusTx (fromBuiltinData)

import Escrow.Business
import Escrow.Validator
import Escrow.Types
import Utils.OffChain

-- | Contract Schema
type EscrowSchema = Endpoint "start"   StartParams
                .\/ Endpoint "cancel"  CancelParams
                .\/ Endpoint "resolve" ResolveParams

data StartParams   = StartParams
                     { receiverAddress   :: ReceiverAddress
                     , sendAmount        :: Integer
                     , sendAssetClass    :: AssetClass
                     , receiveAmount     :: Integer
                     , receiveAssetClass :: AssetClass
                     }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype CancelParams  = CancelParams  { cpTxOutRef :: TxOutRef }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ResolveParams = ResolveParams { rpTxOutRef :: TxOutRef }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

endpoints :: Address
          -> Contract () EscrowSchema Text ()
endpoints raddr = forever $ handleError logError $ awaitPromise $
                  startEp `select` cancelEp `select` resolveEp
  where
    startEp :: Promise () EscrowSchema Text ()
    startEp = endpoint @"start" $ startOp raddr

    cancelEp :: Promise () EscrowSchema Text ()
    cancelEp = endpoint @"cancel" $ cancelOp raddr

    resolveEp :: Promise () EscrowSchema Text ()
    resolveEp = endpoint @"resolve" $ resolveOp raddr

{- | A user, using its `addr`, locks the tokens they want to exchange and
     specifies the tokens they want to receive and from whom, all these
     information is contained on `sParams`. The control Token is minted.
-}
startOp :: Address
        -> StartParams
        -> Contract () EscrowSchema Text ()
startOp addr StartParams{..} = do
    let contractAddress = escrowAddress receiverAddress
        cTokenCurrency  = controlTokenCurrency contractAddress
        cTokenAsset     = assetClass cTokenCurrency cTokenName
        cTokenVal       = assetClassValue cTokenAsset 1
        receiveVal      = assetClassValue receiveAssetClass receiveAmount
        val             = minAda <> cTokenVal <> receiveVal
        datum           = mkEscrowDatum (SenderAddress { sAddr = addr }) receiveAmount receiveAssetClass

        lkp = mconcat
              [ Constraints.typedValidatorLookups (escrowInst receiverAddress)
              , Constraints.otherScript (escrowValidator receiverAddress)
              , Constraints.mintingPolicy (controlTokenMP contractAddress)
              ]
        tx  = mconcat
              [ Constraints.mustPayToTheScript datum val
              , Constraints.mustMintValue cTokenVal
              ]

    mkTxConstraints lkp tx >>= yieldUnbalancedTx
    logInfo @String $ "Addr: " ++ show (escrowAddress receiverAddress)
    logInfo @String "Contract started"

{- | The user, using its `addr`, cancels the escrow placed on the utxo referenced
     on `cParams`, to receive the locked tokens back. The control Token is burned.
-}
cancelOp :: Address
         -> CancelParams
         -> Contract () EscrowSchema Text ()
cancelOp addr CancelParams{..} = do
    utxo       <- findUtxoFromRef cpTxOutRef
    validator  <- loadValidatorWithError utxo
    datum      <- loadDatumWithError utxo
    senderPpkh <- getPpkhFromAddress addr
    validateSenderAddr addr datum
    let
        contractAddress = _ciTxOutAddress utxo
        cTokenCurrency  = controlTokenCurrency contractAddress
        cTokenAsset     = assetClass cTokenCurrency cTokenName
        cTokenVal       = assetClassValue cTokenAsset (-1)

        lkp = mconcat
            [ Constraints.otherScript validator
            , Constraints.unspentOutputs (Map.singleton cpTxOutRef utxo)
            , Constraints.mintingPolicy (controlTokenMP contractAddress)
            ]
        tx = mconcat
            [ Constraints.mustSpendScriptOutput cpTxOutRef cancelRedeemer
            , Constraints.mustMintValue cTokenVal
            , Constraints.mustBeSignedBy senderPpkh
            ]

    mkTxConstraints @Escrowing lkp tx >>= yieldUnbalancedTx
    logInfo @String "Contract canceled"
  where
    validateSenderAddr :: Address
                       -> Datum
                       -> Contract () EscrowSchema Text ()
    validateSenderAddr senderAddr dat =
        getEscrowInfoFromDatum dat >>= \eInfo ->
        if (sAddr . sender) eInfo /= senderAddr
        then throwError "Sender address invalid"
        else pure ()

{- | The user, using its `addr`, resolves the escrow placed on the utxo referenced
     on `rParams`, to pay the corresponding tokens to the other user and to receive
     the locked tokens.
-}
resolveOp :: Address
          -> ResolveParams
          -> Contract () EscrowSchema Text ()
resolveOp addr ResolveParams{..} = do
    let
        contractAddress = escrowAddress (ReceiverAddress { rAddr = addr })
        validator       = escrowValidator (ReceiverAddress { rAddr = addr })
        cTokenCurrency  = controlTokenCurrency contractAddress
        cTokenAsset     = assetClass cTokenCurrency cTokenName
        cTokenVal       = assetClassValue cTokenAsset (-1)

    utxos <- lookupScriptUtxos contractAddress cTokenAsset
    (ref, utxo) <- findContractUtxo rpTxOutRef utxos
    datum <- loadDatumWithError utxo
    escrowInfo <- getEscrowInfoFromDatum datum
    receiverPpkh <- getPpkhFromAddress addr
    senderPpkh <- getPpkhFromAddress (sAddr $ sender escrowInfo)

    let senderPayment = assetClassValue (rAssetClass escrowInfo) (rAmount escrowInfo)

        lkp = mconcat
            [ Constraints.otherScript validator
            , Constraints.unspentOutputs (Map.singleton ref utxo)
            , Constraints.mintingPolicy (controlTokenMP contractAddress)
            ]
        tx = mconcat
            [ Constraints.mustSpendScriptOutput ref resolveRedeemer
            , Constraints.mustMintValue cTokenVal
            , Constraints.mustBeSignedBy receiverPpkh
            , Constraints.mustPayToPubKey senderPpkh senderPayment
            ]

    mkTxConstraints @Escrowing lkp tx >>= yieldUnbalancedTx
    logInfo @String "Contract resolved"

  where
    findContractUtxo :: TxOutRef -> [(TxOutRef, ChainIndexTxOut)] -> Contract () EscrowSchema Text (TxOutRef, ChainIndexTxOut)
    findContractUtxo ref utxos = case filter ((==) ref . fst) utxos of
        [utxo] -> pure utxo
        _      -> throwError "Specified Utxo not found"

getEscrowInfoFromDatum :: Datum
                       -> Contract () EscrowSchema Text EscrowInfo
getEscrowInfoFromDatum dat = case fromBuiltinData (getDatum dat) of
        Nothing -> throwError "Datum format invalid"
        Just d  -> pure $ eInfo d

cTokenName :: TokenName
cTokenName = "controlToken"
