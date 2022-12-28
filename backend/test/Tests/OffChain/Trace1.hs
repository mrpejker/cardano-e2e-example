{-|
Module      : Tests.OffChain.Trace1
Description : Trace1 for unit testing the Escrow contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

In this trace, the Sender starts the contract locking the payment in the script
and then cancels it getting back it funds.
1. The sender (Wallet 1) starts the escrow and deposits the payment, blocking it
   inside the script
2. The sender cancels the escrow, recovering the locked payment
-}

module Tests.OffChain.Trace1 where

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

-- Escrow imports
import Escrow
import Tests.Utils

testMsg :: String
testMsg = "Starting and cancelling the escrow"

test :: TestTree
test = checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emConfig)
        testMsg
        (walletFundsChange senderWallet mempty
        .&&. walletFundsChange receiverWallet mempty
        .&&. assertBlockchain bcCheck)
        trace
  where
    bcCheck :: [Block] -> Bool
    bcCheck b = bcCheckAux blocks
      where
        blocks :: [Block]
        blocks = Prelude.reverse . Prelude.filter (/= []) $ b

        bcCheckAux :: [Block] -> Bool
        bcCheckAux [[ Valid cancel
                    , Valid start
                    , Valid _
                    ]] =
               isJust (Map.lookup (unPaymentPubKey senderPpk)
                          (txSignatures start))
            && isJust (Map.lookup (unPaymentPubKey senderPpk)
                          (txSignatures cancel))
        bcCheckAux _                = False

trace :: EmulatorTrace ()
trace =
    let recAddr = mkReceiverAddress receiverAddr
        startParams = mkStartParams
                        recAddr
                        100
                        (assetClass tokenACurrencySymbol tokenA)
                        100
                        (assetClass tokenBCurrencySymbol tokenB)
    in do
    h1 <- activateContractWallet senderWallet $ endpoints senderAddr
    callEndpoint @"start" h1 startParams
    void $ waitNSlots 10

    h2 <- activateContractWallet receiverWallet $ endpoints receiverAddr
    callEndpoint @"reload" h2 ()
    utxos <- getObservableState h2

    let cancelParams = mkCancelParams (escrowUtxo $ head utxos) recAddr
    callEndpoint @"cancel" h1 cancelParams
    void $ waitNSlots 10

-- | For running the trace from the repl
runTrace :: IO ()
runTrace = do
  putStrLn $ "\n" ++ testMsg ++ ".\n"
  runEmulatorTraceIO' def emConfig trace
