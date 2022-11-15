{-|
Module      : Tests.OffChain.Trace2
Description : Trace2 for unit testing the Escrow contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

In this trace, the Sender starts the contracts locking the payment in the script
and then the Receiver resolves the contract paying their part to the sender,
and receiving the agreed amount.
1. The sender (Wallet 1) starts the escrow and deposits the payment, blocking
it inside the script
2. The Receiver resolve the escrow, paying their part and receiving the agreed
amount
-}

module Tests.OffChain.Trace2 where

-- Non-IOG imports
import Control.Lens          ( (.~), (&) )
import Control.Monad         ( void )
import Data.Default          ( Default (..) )
import Data.Map              qualified as Map
import Data.Maybe            ( isJust )
import Test.Tasty            ( TestTree )

-- IOG imports
import Ledger                ( Block, OnChainTx(Valid), txSignatures
                             , unPaymentPubKey
                             )
import Ledger.Value          ( assetClass )
import Plutus.Trace.Emulator ( activateContractWallet, callEndpoint
                             , EmulatorTrace, runEmulatorTraceIO', waitNSlots
                             )
import Plutus.Contract.Test  ( (.&&.), assertBlockchain, checkPredicateOptions
                             , defaultCheckOptions, emulatorConfig
                             , walletFundsChange
                             )

import Escrow.Business
import Escrow.OffChain
import Tests.Utils

testMsg :: String
testMsg = "Starting and resolving the escrow"

test :: TestTree
test = checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emConfig)
        testMsg
        (walletFundsChange senderWallet (paymentA (-50) <> paymentB 100)
        .&&. walletFundsChange receiverWallet (paymentB (-100) <> paymentA 50)
        .&&. assertBlockchain bcCheck)
        trace
  where
    bcCheck :: [Block] -> Bool
    bcCheck b = bcCheckAux blocks
      where
        blocks :: [Block]
        blocks = Prelude.reverse . Prelude.filter (/= []) $ b

        bcCheckAux :: [Block] -> Bool
        bcCheckAux [ [Valid _]
                   , [Valid start]
                   , [Valid resolve]
                   ] =
               isJust (Map.lookup (unPaymentPubKey senderPpk)
                          (txSignatures start))
            && isJust (Map.lookup (unPaymentPubKey receiverPpk)
                          (txSignatures resolve))
        bcCheckAux _                = False

trace :: EmulatorTrace ()
trace =
    let startParams = StartParams
            { receiverAddress   = mkReceiverAddress receiverAddr
            , sendAmount        = 50
            , sendAssetClass    = assetClass tokenACurrencySymbol tokenA
            , receiveAmount     = 100
            , receiveAssetClass = assetClass tokenBCurrencySymbol tokenB
            }
    in do
    h1 <- activateContractWallet senderWallet $ endpoints senderAddr
    callEndpoint @"start" h1 startParams
    void $ waitNSlots 10
    h2 <- activateContractWallet receiverWallet $ endpoints receiverAddr
    utxos <- utxosMap
    let scriptUtxos   = findScriptTxOutRef utxos
        resolveParams = ResolveParams { rpTxOutRef = head scriptUtxos }
    callEndpoint @"resolve" h2 resolveParams
    void $ waitNSlots 10

-- | For running the trace from the repl
runTrace :: IO ()
runTrace = do
  putStrLn $ "\n" ++ testMsg ++ ".\n"
  runEmulatorTraceIO' def emConfig trace