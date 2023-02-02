{- HLINT ignore "Use head" -}
{-|
Module      : Tests.OffChain.Trace3
Description : Trace3 for unit testing the Escrow contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

In this trace, multiple Senders start escrows with the same wallet as
the Receiver.
Then the Receiver resolves all of them, receiving the agreed amount.

Trace execution description:
  1. Wallet 1 starts an escrow with Wallet 2 as the receiver, depositing the
     payment
  2. Wallet 3 starts an escrow with Wallet 2 as the receiver
  3. Wallet 4 starts an escrow with Wallet 2 as the receiver
  4. The Receiver (Wallet 2) resolves the first escrow, paying their part and
     receiving the agreed amount
  5. The Receiver (Wallet 2) resolves the second escrow
  6. The Receiver (Wallet 2) resolves the third escrow
-}

module Tests.OffChain.Trace3 where

-- Non-IOG imports
import Control.Lens  ( (.~), (&) )
import Control.Monad ( void )
import Data.Default  ( Default (..) )
import Test.Tasty    ( TestTree )

-- IOG imports
import Ledger.Value           ( assetClass )
import Plutus.Trace.Emulator  ( activateContractWallet, callEndpoint
                              , EmulatorTrace, runEmulatorTraceIO', waitNSlots
                              )
import Plutus.Contract.Test   ( (.&&.)
                              , checkPredicateOptions, defaultCheckOptions
                              , emulatorConfig, walletFundsChange, w3, w4
                              )

-- Escrow imports
import Escrow        ( mkStartParams, mkResolveParams
                     , mkReceiverAddress, endpoints, escrowUtxo
                     )
import Tests.Utils   ( emConfig, senderWallet, receiverWallet
                     , receiverAddr, senderAddr
                     , paymentA, paymentB
                     , tokenACurrencySymbol, tokenAName
                     , tokenBCurrencySymbol, tokenBName
                     , getEscrowInfoList, mockWAddress, mockReloadFlag
                     )

testMsg :: String
testMsg = "Starting and resolving 3 escrows with same receiver"

test :: TestTree
test = checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emConfig)
        testMsg
        (walletFundsChange receiverWallet   (paymentA 300    <> paymentB (-30))
        .&&. walletFundsChange senderWallet (paymentA (-100) <> paymentB 10)
        .&&. walletFundsChange w3           (paymentA (-100) <> paymentB 10)
        .&&. walletFundsChange w4           (paymentA (-100) <> paymentB 10))
        trace

trace :: EmulatorTrace ()
trace = do
    let startParams = mkStartParams
                        (mkReceiverAddress receiverAddr)
                        100
                        (assetClass tokenACurrencySymbol tokenAName)
                        10
                        (assetClass tokenBCurrencySymbol tokenBName)

    h1 <- activateContractWallet senderWallet $ endpoints senderAddr
    h2 <- activateContractWallet w3 $ endpoints $ mockWAddress w3
    h3 <- activateContractWallet w4 $ endpoints $ mockWAddress w4
    callEndpoint @"start" h1 startParams
    void $ waitNSlots 10
    callEndpoint @"start" h2 startParams
    void $ waitNSlots 10
    callEndpoint @"start" h3 startParams
    void $ waitNSlots 10

    h4 <- activateContractWallet receiverWallet $ endpoints receiverAddr
    callEndpoint @"reload" h4 mockReloadFlag
    utxos <- getEscrowInfoList h4
    let resolveParams1 = mkResolveParams $ escrowUtxo $ utxos !! 0
        resolveParams2 = mkResolveParams $ escrowUtxo $ utxos !! 1
        resolveParams3 = mkResolveParams $ escrowUtxo $ utxos !! 2

    callEndpoint @"resolve" h4 resolveParams1
    void $ waitNSlots 10
    callEndpoint @"resolve" h4 resolveParams2
    void $ waitNSlots 10
    callEndpoint @"resolve" h4 resolveParams3
    void $ waitNSlots 10

-- | For running the trace from the repl
runTrace :: IO ()
runTrace = do
  putStrLn $ "\n" ++ testMsg ++ ".\n"
  runEmulatorTraceIO' def emConfig trace
