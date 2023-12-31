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
import Control.Monad   ( void )
import Data.Default    ( def )
import Data.Map as Map ( fromList )
import Data.Maybe      ( fromJust )
import Data.Monoid     ( Last(..) )
import Data.Text       ( Text )

-- IOG imports
import Control.Monad.Freer.Extras ( logInfo )
import Plutus.Trace.Emulator      ( ContractHandle
                                  , EmulatorConfig (EmulatorConfig)
                                  , EmulatorTrace, observableState, waitNSlots
                                  )
import Plutus.Contract.Test       ( w1, w2, w3, w4
                                  , mockWalletPaymentPubKeyHash
                                  )
import Ledger                     ( Address, PaymentPubKey, PaymentPubKeyHash
                                  , pubKeyHashAddress
                                  )
import Ledger.Ada                 ( lovelaceValueOf )
import Ledger.Value               as Value
import Wallet.Emulator.Types      ( Wallet (..) )
import Wallet.Emulator.Wallet     ( mockWalletAddress
                                  , mockWalletPaymentPubKey
                                  )

-- Escrow imports
import Escrow ( EscrowSchema, UtxoEscrowInfo, ObservableState(info) )
import Utils.WalletAddress ( WalletAddress, fromAddress )

walletsWithValue :: [(Wallet,Value)]
walletsWithValue = [ (w, v <> valueA 1_000_000 <> valueB 1_000_000)
                   | w <- wallets
                   ]
  where
    v :: Value
    v = lovelaceValueOf 100_000_000

wallet1, wallet2, wallet3, wallet4 :: Wallet
wallet1 = w1
wallet2 = w2
wallet3 = w3
wallet4 = w4

-- | Wallets that will be used to test the endpoints
wallets :: [Wallet]
wallets = [wallet1, wallet2, wallet3, wallet4]

mockPKH :: Wallet -> PaymentPubKeyHash
mockPKH = mockWalletPaymentPubKeyHash

mockAddress :: Wallet -> Address
mockAddress = flip pubKeyHashAddress Nothing . mockPKH

mockWAddress :: Wallet -> WalletAddress
mockWAddress =  fromJust . fromAddress . mockAddress

tokenAName, tokenBName :: TokenName
tokenAName = "A"
tokenBName = "B"

tokenACurrencySymbol, tokenBCurrencySymbol :: CurrencySymbol
tokenACurrencySymbol =
    "246ea4f1fd944bc8b0957050a31ab0487016be233725c9f931b1aaaa"
tokenBCurrencySymbol =
    "0b1e203c7e13914e095bf462441205c1b377e978718fcb93fd44bbbb"

valueA, valueB :: Integer -> Value
valueA = singleton tokenACurrencySymbol tokenAName
valueB = singleton tokenBCurrencySymbol tokenBName

wallet1Addr, wallet2Addr, wallet3Addr, wallet4Addr :: WalletAddress
wallet1Addr = fromJust $ fromAddress $ mockWalletAddress wallet1
wallet2Addr = fromJust $ fromAddress $ mockWalletAddress wallet2
wallet3Addr = fromJust $ fromAddress $ mockWalletAddress wallet3
wallet4Addr = fromJust $ fromAddress $ mockWalletAddress wallet4

wallet1Ppk, wallet2Ppk, wallet3Ppk, wallet4Ppk :: PaymentPubKey
wallet1Ppk = mockWalletPaymentPubKey wallet1
wallet2Ppk = mockWalletPaymentPubKey wallet2
wallet3Ppk = mockWalletPaymentPubKey wallet3
wallet4Ppk = mockWalletPaymentPubKey wallet4

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ fromList walletsWithValue) def

mockReloadFlag :: Integer
mockReloadFlag = 0

{- | Polls the Emulator until it finds an ObservableState and return the info
     field.
-}
getEscrowInfoList :: ContractHandle (Last ObservableState) EscrowSchema Text
                  -> EmulatorTrace [UtxoEscrowInfo]
getEscrowInfoList h = info <$> getObservableState h

-- | Polls the Emulator until it finds an ObservableState.
getObservableState
    :: ContractHandle (Last ObservableState) EscrowSchema Text
    -> EmulatorTrace ObservableState
getObservableState h = do
    void $ waitNSlots 1
    l <- observableState h
    case l of
        Last (Just observable) ->
            logInfo (show observable) >> return observable
        Last _ ->
            waitNSlots 1 >> getObservableState h
