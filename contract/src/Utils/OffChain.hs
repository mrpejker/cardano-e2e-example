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
    Map.toList . Map.filter (txOutHasToken . (^. ciTxOutValue))
        <$> utxosAt addr
  where
    txOutHasToken :: Value -> Bool
    txOutHasToken v = assetClassValueOf v token == 1

{- | Off-chain function for getting the validator from a ChainIndexTxOut.
     Calls throwError if it can't find it.
-}
loadValidatorWithError
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text Validator
loadValidatorWithError txOut =
    loadValidator txOut >>= maybe (throwError "Validator not Found") pure

{- | Off-chain function for getting the validator from a ChainIndexTxOut.
     returns Maybe if it can't find it.
-}
loadValidator
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text (Maybe Validator)
loadValidator txOut =
    either validatorFromHash (return . Just) (_ciTxOutValidator txOut)

{- | Off-chain function for getting the datum from a ChainIndexTxOut.
     Calls throwError if it can't find it.
-}
loadDatumWithError
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text Datum
loadDatumWithError txOut =
    loadDatum txOut >>= maybe (throwError "Datum not Found") pure

{- | Off-chain function for getting the datum from a ChainIndexTxOut.
     returns Maybe if it can't find it.
-}
loadDatum
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text (Maybe Datum)
loadDatum txOut =
    either datumFromHash (return . Just) (_ciTxOutDatum txOut)

{- | Off-chain function for getting the ChainIndexTxOut for the given TxOutRef
-}
findUtxoFromRef
    :: forall w s
    .  TxOutRef
    -> Contract w s Text ChainIndexTxOut
findUtxoFromRef ref =
    unspentTxOutFromRef ref >>= maybe (throwError "Utxo not found") pure

{- | Off-chain function for getting the PaymentPubKeyHash for the given Address
-}
getPpkhFromAddress
    :: forall w s
    .  Address
    -> Contract w s Text PaymentPubKeyHash
getPpkhFromAddress addr =
    maybe (throwError "The address should be a wallet address")
     (pure . PaymentPubKeyHash) (toPubKeyHash addr)
