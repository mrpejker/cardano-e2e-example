{-|
Module      : Tests.OffChain.Trace2
Description : Trace2 for unit testing the Escrow contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

In this trace, the Sender starts the contracts locking the payment in the script
and then the Receiver resolves the contract paying their part to the sender,
and receiving the agreed amount.

Trace execution description:
  1. The sender (Wallet 1) starts the escrow and deposits the payment, blocking
     it inside the script
  2. The Receiver resolve the escrow, paying their part and receiving the agreed
     amount
-}

module Tests.OffChain.Trace2 where

-- Non-IOG imports
import Control.Lens  ( (.~), (&) )
import Control.Monad ( void )
import Data.Default  ( Default (..) )
import Test.Tasty    ( TestTree )

-- IOG imports
import Ledger.Value          ( assetClass )
import Plutus.Trace.Emulator ( activateContractWallet, callEndpoint
                             , EmulatorTrace, runEmulatorTraceIO', waitNSlots
                             )
import Plutus.Contract.Test  ( (.&&.), checkPredicateOptions
                             , defaultCheckOptions, emulatorConfig
                             , walletFundsChange
                             )

-- Escrow imports
import Escrow        ( mkStartParams, mkResolveParams
                     , mkReceiverAddress, endpoints, escrowUtxo
                     )
import Tests.Utils   ( emConfig
                     , wallet1, wallet2
                     , wallet1Addr, wallet2Addr
                     , valueA, valueB
                     , tokenACurrencySymbol, tokenAName
                     , tokenBCurrencySymbol, tokenBName
                     , getEscrowInfoList, mockReloadFlag
                     )
testMsg :: String
testMsg = "Starting and resolving the escrow"

test :: TestTree
test = checkPredicateOptions
       (defaultCheckOptions & emulatorConfig .~ emConfig)
       testMsg
       (    walletFundsChange wallet1 (valueA (-50) <> valueB 100)
       .&&. walletFundsChange wallet2 (valueB (-100) <> valueA 50)
       )
       trace

trace :: EmulatorTrace ()
trace = do
    let startParams = mkStartParams
                      (mkReceiverAddress wallet2Addr)
                      50
                      (assetClass tokenACurrencySymbol tokenAName)
                      100
                      (assetClass tokenBCurrencySymbol tokenBName)

    h1 <- activateContractWallet wallet1 $ endpoints wallet1Addr
    callEndpoint @"start" h1 startParams
    void $ waitNSlots 10
    h2 <- activateContractWallet wallet2 $ endpoints wallet2Addr
    callEndpoint @"reload" h2 mockReloadFlag
    utxos <- getEscrowInfoList h2

    let resolveParams = mkResolveParams $ escrowUtxo $ head utxos
    callEndpoint @"resolve" h2 resolveParams
    void $ waitNSlots 10

-- | For running the trace from the repl
runTrace :: IO ()
runTrace = do
  putStrLn $ "\n" ++ testMsg ++ ".\n"
  runEmulatorTraceIO' def emConfig trace
