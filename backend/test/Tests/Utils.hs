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
import PlutusTx.Prelude           ( traceError )
import Ledger                     ( Address, PaymentPubKey, PaymentPubKeyHash
                                  , PubKey, CardanoTx, Signature
                                  , pubKeyHashAddress, onCardanoTx
                                  , txSignatures
                                  )
import Ledger.Ada                 ( lovelaceValueOf )
import Ledger.Value               as Value
import Wallet.Emulator.Types      ( Wallet (..) )
import Wallet.Emulator.Wallet     ( mockWalletAddress
                                  , mockWalletPaymentPubKey
                                  )

-- Escrow imports
import Escrow ( EscrowSchema, UtxoEscrowInfo )
import Utils.WalletAddress ( WalletAddress, toWalletAddress )

walletsWithValue :: [(Wallet,Value)]
walletsWithValue = [(w, v <> paymentA 1000 <> paymentB 1000)
                   | w <- [senderWallet,receiverWallet,w3,w4]
                   ]
  where
    v :: Value
    v = lovelaceValueOf 100_000_000

-- | Wallets that will be used to test the endpoints
wallets :: [Wallet]
wallets = [w1, w2, w3, w4]

mockPKH :: Wallet -> PaymentPubKeyHash
mockPKH = mockWalletPaymentPubKeyHash

mockAddress :: Wallet -> Address
mockAddress = flip pubKeyHashAddress Nothing . mockPKH

mockWAddress :: Wallet -> WalletAddress
mockWAddress =  fromJust . toWalletAddress . mockAddress

tokenA, tokenB :: TokenName
tokenA = "A"
tokenB = "B"

tokenACurrencySymbol, tokenBCurrencySymbol :: CurrencySymbol
tokenACurrencySymbol =
    "246ea4f1fd944bc8b0957050a31ab0487016be233725c9f931b1aaaa"
tokenBCurrencySymbol =
    "0b1e203c7e13914e095bf462441205c1b377e978718fcb93fd44bbbb"

paymentA, paymentB :: Integer -> Value
paymentA = singleton tokenACurrencySymbol tokenA
paymentB = singleton tokenBCurrencySymbol tokenB

senderWallet, receiverWallet :: Wallet
senderWallet   = w1
receiverWallet = w2

senderAddr, receiverAddr :: WalletAddress
senderAddr   = fromJust $ toWalletAddress $ mockWalletAddress senderWallet
receiverAddr = fromJust $ toWalletAddress $ mockWalletAddress receiverWallet

senderPpk, receiverPpk :: PaymentPubKey
senderPpk   = mockWalletPaymentPubKey senderWallet
receiverPpk = mockWalletPaymentPubKey receiverWallet

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ fromList walletsWithValue) def

-- | Polls the Emulator until it finds an ObservableState
getObservableState
    :: ContractHandle (Last [UtxoEscrowInfo]) EscrowSchema Text
    -> EmulatorTrace [UtxoEscrowInfo]
getObservableState h = do
    void $ waitNSlots 1
    l <- observableState h
    case l of
        Last (Just observable) ->
            logInfo (show observable) >> return observable
        Last _ ->
            waitNSlots 1 >> getObservableState h
