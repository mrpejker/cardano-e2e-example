{-|
Module      : Utils.OffChain
Description : Utils functions for OffChain code.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.OffChain where

-- Non-IOG imports
import Control.Monad ( unless )
import Control.Lens  ( (^.), matching )
import Data.Text     ( Text )
import Data.Map qualified as Map ( toList, filter )

-- IOG imports
import Ledger ( DecoratedTxOut(..), Address, TxOutRef, Datum, DatumHash
              , DatumFromQuery(..), PaymentPubKeyHash(..)
              , decoratedTxOutDatum, decoratedTxOutAddress
              , decoratedTxOutValue, _ScriptDecoratedTxOut
              , toPubKeyHash
              )
import Ledger.Value    ( Value, AssetClass, assetClassValueOf )
import Plutus.Contract ( Contract, unspentTxOutFromRef
                       , utxosAt, throwError, datumFromHash
                       )

-- | Gets a list of utxos for the given address that contains the given Token.
lookupScriptUtxos
    :: forall w s
    .  Address
    -> AssetClass
    -> Contract w s Text [(TxOutRef, DecoratedTxOut)]
lookupScriptUtxos addr ac =
    Map.toList . Map.filter (valueHasOneToken ac . (^. decoratedTxOutValue))
    <$> utxosAt addr

-- | Checks if the value has exactly one token of the given asset class.
valueHasOneToken :: AssetClass -> Value -> Bool
valueHasOneToken ac v = assetClassValueOf v ac == 1

-- | Gets the Datum from a DecoratedTxOut. Throws an error if it can't find it.
getDatumWithError
    :: forall w s
    .  DecoratedTxOut
    -> Contract w s Text Datum
getDatumWithError txOut =
    getDatumTxOut txOut >>= maybe (throwError "Datum not Found") pure

-- | Gets the Maybe Datum from a DecoratedTxOut.
getDatumTxOut
    :: forall w s
    .  DecoratedTxOut
    -> Contract w s Text (Maybe Datum)
getDatumTxOut txOut =
    case matching _ScriptDecoratedTxOut txOut of
        Right (_,_,_,(_,DatumInline d),_,_) -> return (Just d)
        Right (_,_,_,(_,DatumInBody d),_,_) -> return (Just d)
        Right (_,_,_,(dh,DatumUnknown),_,_) -> datumFromHash dh
        _ -> return Nothing

{- | Finds the unique utxo from the given TxOutRef and loads the datum on the
     DecoratedTxOut.
-}
findUtxoFromRefWithDatum
    :: forall w s
    .  TxOutRef
    -> Contract w s Text DecoratedTxOut
findUtxoFromRefWithDatum ref =
    unspentTxOutFromRef ref >>=
    maybe (throwError "Utxo not found") (decoratedTxOutDatum loadDatum)
  where
    loadDatum
        :: (DatumHash, DatumFromQuery)
        -> Contract w s Text (DatumHash, DatumFromQuery)
    loadDatum d@(dh, DatumUnknown) =
        maybe d ((dh,) . DatumInBody) <$> datumFromHash dh
    loadDatum d = return d

{- | Finds the unique utxo (loading the datum if needed) from the given TxOutRef.
     The function fails if the utxo address doesn't match the given address or
     the value doesn't have the given token asset class.
-}
findValidUtxoFromRef
    :: forall w s
    .  TxOutRef
    -> Address
    -> AssetClass
    -> Contract w s Text DecoratedTxOut
findValidUtxoFromRef ref addr ac = do
    utxo <- findUtxoFromRefWithDatum ref

    unless (utxo ^. decoratedTxOutAddress == addr) $
        throwError "Wrong escrow address on the utxo"

    unless (valueHasOneToken ac (utxo ^. decoratedTxOutValue)) $
        throwError "The utxo doesn't have the token"

    return utxo

-- | Gets the PaymentPubKeyHash for the given Address
getPpkhFromAddress
    :: forall w s
    .  Address
    -> Contract w s Text PaymentPubKeyHash
getPpkhFromAddress = maybe (throwError "The address should be a wallet address")
                           (return . PaymentPubKeyHash) . toPubKeyHash
