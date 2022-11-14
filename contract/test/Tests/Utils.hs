{-# LANGUAGE NumericUnderscores #-}

{-|
Module      : Tests.Utils
Description : Util functions for the Tests.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

In this module we define a variety of functions and constants to be used on the
traces and other testing modules.
-}

module Tests.Utils where

-- Non-IOG imports
import           Control.Lens
import           Data.Default                 (def)
import           Data.Maybe                   (fromJust, isJust)
import qualified Data.Map                     as Map
import           Text.Hex                     (decodeHex)

-- IOG imports
import           Plutus.Trace.Emulator        ( chainNewestFirst, chainState
                                              , EmulatorConfig (EmulatorConfig)
                                              , EmulatorTrace
                                              )
import           Plutus.Contract.Test         ( w1, w2 )
import           PlutusTx.Builtins.Class      ( toBuiltin )
import           Ledger                       ( Address, PaymentPubKey, TxOut
                                              , txOutDatumHash, TxOutRef
                                              , unspentOutputs
                                              )
import           Ledger.Ada                   ( lovelaceValueOf )
import           Ledger.Value                 as Value
import           Wallet.Emulator.Types        ( Wallet (..) )
import           Wallet.Emulator.Wallet       ( mockWalletAddress
                                              , mockWalletPaymentPubKey
                                              )

tokenA :: TokenName
tokenA = tokenName "A"

tokenB :: TokenName
tokenB = tokenName "B"

tokenACurrencySymbol :: CurrencySymbol
tokenACurrencySymbol = CurrencySymbol $ toBuiltin $ fromJust $
    decodeHex "246ea4f1fd944bc8b0957050a31ab0487016be233725c9f931b1aaaa"

tokenBCurrencySymbol :: CurrencySymbol
tokenBCurrencySymbol = CurrencySymbol $ toBuiltin $ fromJust $
    decodeHex "0b1e203c7e13914e095bf462441205c1b377e978718fcb93fd44bbbb"

paymentA :: Integer -> Value
paymentA = singleton tokenACurrencySymbol tokenA

paymentB :: Integer -> Value
paymentB = singleton tokenBCurrencySymbol tokenB

wallets :: [(Wallet,Value)]
wallets = [ (senderWallet,   v <> paymentA 100)
          , (receiverWallet, v <> paymentB 100)
          ]
  where
    v :: Value
    v = lovelaceValueOf 100_000_000

senderWallet :: Wallet
senderWallet = w1

receiverWallet :: Wallet
receiverWallet = w2

senderAddr :: Address
senderAddr = mockWalletAddress senderWallet

receiverAddr :: Address
receiverAddr = mockWalletAddress receiverWallet

senderPpk :: PaymentPubKey
senderPpk = mockWalletPaymentPubKey senderWallet

receiverPpk :: PaymentPubKey
receiverPpk = mockWalletPaymentPubKey receiverWallet

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def

{- | Temporary solution to get TxOutRef to build the cancel and
     resolve parameters.
-}
utxosMap :: EmulatorTrace (Map.Map TxOutRef TxOut)
utxosMap =  chainState >>=
  \chain -> pure $ unspentOutputs (chain ^. chainNewestFirst)

findScriptTxOutRef :: Map.Map TxOutRef TxOut -> TxOutRef
findScriptTxOutRef = head . Map.keys . Map.filter (isJust . txOutDatumHash)
