{-|
Module      : Utils.OffChain
Description : Utils functions for OffChain code.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.OffChain where

-- Non-IOG imports
import Control.Lens ( (^.), matching )
import Data.Text    ( Text )
import Data.Map qualified as Map ( toList, filter )

-- IOG imports
import Ledger.Constraints ( TxConstraints, mustPayToPubKey
                          , mustPayToPubKeyAddress
                          )
import Ledger ( DecoratedTxOut(..)
              , DatumFromQuery(..), PaymentPubKeyHash(..)
              , StakePubKeyHash(..)
              , decoratedTxOutDatum
              , decoratedTxOutValue, _ScriptDecoratedTxOut
              , toPubKeyHash, stakePubKeyHashCredential
              )
import Ledger.Value    ( AssetClass, assetClassValueOf )
import Plutus.Contract ( Contract, unspentTxOutFromRef
                        , utxosAt, throwError, datumFromHash
                       )
import Plutus.V1.Ledger.Api

-- Escrow imports
import Utils.WalletAddress ( WalletAddress(..) )

-- | Gets a list of utxos for the given address that contains the given Token.
lookupScriptUtxos
    :: forall w s
    .  Address
    -> AssetClass
    -> Contract w s Text [(TxOutRef, DecoratedTxOut)]
lookupScriptUtxos addr token =
    Map.toList . Map.filter (txOutHasToken . (^. decoratedTxOutValue))
    <$> utxosAt addr
  where
    txOutHasToken :: Value -> Bool
    txOutHasToken v = assetClassValueOf v token == 1

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
--    either datumFromHash (return . Just) (decoratedTxOutDatum txOut)
    case matching _ScriptDecoratedTxOut txOut of
        Right (_,_,_,(_,DatumInline d),_,_) -> return (Just d)
        Right (_,_,_,(_,DatumInBody d),_,_) -> return (Just d)
        Right (_,_,_,(dh,DatumUnknown),_,_) -> datumFromHash dh
        _ -> return Nothing

-- | Finds the DecoratedTxOut from the given TxOutRef.
findUtxoFromRef
    :: forall w s
    .  TxOutRef
    -> Contract w s Text DecoratedTxOut
findUtxoFromRef ref =
    unspentTxOutFromRef ref >>= maybe (throwError "Utxo not found") pure

{- | Finds the unique utxo from the given TxOutRef and loads the datum on the
     DecoratedTxOut, fails otherwise.
-}
filterMUtxo
    :: forall w s
    .  TxOutRef
    -> [(TxOutRef, DecoratedTxOut)]
    -> Contract w s Text (TxOutRef, DecoratedTxOut)
filterMUtxo ref utxos =
    case filter ((==) ref . fst) utxos of
        [(oref, o)] -> (oref,) <$> decoratedTxOutDatum loadDatum o
        _           -> throwError "Specified Utxo not found"
  where
    loadDatum
        :: (DatumHash, DatumFromQuery)
        -> Contract w s Text (DatumHash, DatumFromQuery)
    loadDatum d@(dh, DatumUnknown) =
        maybe d ((dh,) . DatumInBody) <$> datumFromHash dh
    loadDatum d = return d

-- | Gets the PaymentPubKeyHash for the given Address
getPpkhFromAddress
    :: forall w s
    .  Address
    -> Contract w s Text PaymentPubKeyHash
getPpkhFromAddress = maybe (throwError "The address should be a wallet address")
                           (return . PaymentPubKeyHash) . toPubKeyHash

{- | To avoid making case-pattern matching inside off-chain functions when
     the staking pkh of the wallet address is Nothing.
-}
mustPayToWalletAddress :: forall i o
                       .  WalletAddress
                       -> Value
                       -> TxConstraints i o
mustPayToWalletAddress WalletAddress{..} val =
  case waStaking of
    Just staking -> mustPayToPubKeyAddress ppkh
                    (stakePubKeyHashCredential $ StakePubKeyHash staking) val
    Nothing      -> mustPayToPubKey ppkh val
  where
    ppkh :: PaymentPubKeyHash
    ppkh = PaymentPubKeyHash waPayment

