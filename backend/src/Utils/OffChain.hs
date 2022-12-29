{-|
Module      : Utils.OffChain
Description : Utils functions for OffChain code.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.OffChain where

-- Non-IOG imports
import Control.Lens ( (^.) )
import Data.Text    ( Text )
import Data.Map qualified as Map ( toList, filter )

-- IOG imports
import Ledger ( Address, TxOutRef, ChainIndexTxOut(..)
              , Datum, PaymentPubKeyHash(..), StakePubKeyHash(..)
              , ciTxOutValue, _ciTxOutDatum
              , toPubKeyHash
              )
import Ledger.Constraints ( TxConstraints, mustPayToPubKey
                          , mustPayToPubKeyAddress
                          )
import Ledger.Value    ( AssetClass, Value, assetClassValueOf )
import Plutus.Contract ( Contract, unspentTxOutFromRef
                        , utxosAt, throwError, datumFromHash
                       )

-- Escrow imports
import Utils.WalletAddress ( WalletAddress(..) )

-- | Gets a list of utxos for the given address that contains the given Token.
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

-- | Gets the Datum from a ChainIndexTxOut. Throws an error if it can't find it.
getDatumWithError
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text Datum
getDatumWithError txOut =
    getDatumTxOut txOut >>= maybe (throwError "Datum not Found") pure

-- | Gets the Maybe Datum from a ChainIndexTxOut.
getDatumTxOut
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text (Maybe Datum)
getDatumTxOut txOut =
    either datumFromHash (return . Just) (_ciTxOutDatum txOut)

-- | Finds the ChainIndexTxOut from the given TxOutRef.
findUtxoFromRef
    :: forall w s
    .  TxOutRef
    -> Contract w s Text ChainIndexTxOut
findUtxoFromRef ref =
    unspentTxOutFromRef ref >>= maybe (throwError "Utxo not found") pure

-- | Gets the PaymentPubKeyHash for the given Address
getPpkhFromAddress
    :: forall w s
    .  Address
    -> Contract w s Text PaymentPubKeyHash
getPpkhFromAddress = maybe (throwError "The address should be a wallet address")
                           (return . PaymentPubKeyHash) . toPubKeyHash

-- | To avoid making case-pattern matching inside off-chain functions when
--   the staking pkh of the wallet address is Nothing.
mustPayToWalletAddress :: forall i o
                       .  WalletAddress
                       -> Value
                       -> TxConstraints i o
mustPayToWalletAddress WalletAddress{..} val = case waStaking of
    Just staking ->
        let ppkh = PaymentPubKeyHash waPayment
            spkh = StakePubKeyHash staking
        in mustPayToPubKeyAddress ppkh spkh val
    Nothing -> mustPayToPubKey (PaymentPubKeyHash waPayment) val
