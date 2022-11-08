{-|
Module      : Utils.OffChain
Description : Utils functions for OffChain code.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.OffChain where

-- Non-IOG imports
import Data.Map qualified as Map
import Control.Lens
import Data.Text

-- IOG imports
import Ledger
import Ledger.Value
import Plutus.Contract


{- | Off-chain function for getting a list of utxos for the given address that
     contains the given Token.
-}
lookupScriptUtxos
    :: forall w s
    .  Address
    -> AssetClass
    -> Contract w s Text [(TxOutRef, ChainIndexTxOut)]
lookupScriptUtxos addr token =
    Map.toList . Map.filter (checkTxHasToken token . (^. ciTxOutValue))
        <$> utxosAt addr
  where
    checkTxHasToken :: AssetClass -> Value -> Bool
    checkTxHasToken asc v = assetClassValueOf v asc == 1

loadValidatorWithError
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text Validator
loadValidatorWithError txOut =
    loadValidator txOut >>= maybe (throwError "Validator not Found") pure

loadValidator
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text (Maybe Validator)
loadValidator txOut = case _ciTxOutValidator txOut of
    Left vh -> validatorFromHash vh
    Right v -> return $ Just v

loadDatumWithError
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text Datum
loadDatumWithError txOut =
    loadDatum txOut >>= maybe (throwError "Datum not Found") pure

loadDatum
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text (Maybe Datum)
loadDatum txOut = case _ciTxOutDatum txOut of
    Left dh -> datumFromHash dh
    Right d -> return $ Just d

findUtxoFromRef
    :: forall w s
    .  TxOutRef
    -> Contract w s Text ChainIndexTxOut
findUtxoFromRef ref = do
    mUtxo <- unspentTxOutFromRef ref
    case mUtxo of
        Nothing -> throwError "Utxo not found"
        Just utxo -> pure utxo

getPpkhFromAddress
    :: forall w s
    .  Address
    -> Contract w s Text PaymentPubKeyHash
getPpkhFromAddress addr = case toPubKeyHash addr of
    Nothing   -> throwError "The address should be a wallet address"
    Just pkh -> pure (PaymentPubKeyHash { unPaymentPubKeyHash = pkh })