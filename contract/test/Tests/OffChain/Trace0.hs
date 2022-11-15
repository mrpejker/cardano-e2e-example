{-|
Module      : Tests.OffChain.Trace0
Description : Trace0 for unit testing the Escrow contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

In this trace, we only test that the escrow has been properly started, and
that the payment has been locked in the script.
Trace execution description:
1. The sender (Wallet 1) starts the escrow and deposits the payment,
blocking it inside the script.
-}

module Tests.OffChain.Trace0 where

-- Non-IOG imports
import Control.Lens          ( (.~), (&) )
import Control.Monad         ( void )
import Data.Default          ( Default (..) )
import Data.Map              qualified as Map
import Data.Maybe            ( isJust )
import Test.Tasty            ( TestTree )

-- IOG imports
import PlutusTx.Numeric      qualified as PNum ((-))
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
import Utils.OnChain
import Tests.Utils

testMsg :: String
testMsg = "Only starting the escrow"

test :: TestTree
test = checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emConfig)
        testMsg
        (walletFundsChange senderWallet (paymentA (-100) PNum.- minAda)
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
        bcCheckAux [ [Valid _]
                   , [Valid start]
                   ] =
           isJust (Map.lookup (unPaymentPubKey senderPpk) (txSignatures start))
        bcCheckAux _                = False

trace :: EmulatorTrace ()
trace =
    let startParams = StartParams
            { receiverAddress   = mkReceiverAddress receiverAddr
            , sendAmount        = 100
            , sendAssetClass    = assetClass tokenACurrencySymbol tokenA
            , receiveAmount     = 100
            , receiveAssetClass = assetClass tokenBCurrencySymbol tokenB
            }
    in do
    h1 <- activateContractWallet senderWallet $ endpoints senderAddr
    callEndpoint @"start" h1 startParams
    void $ waitNSlots 10

-- | For running the trace from the repl
runTrace :: IO ()
runTrace = do
  putStrLn $ "\n" ++ testMsg ++ ".\n"
  runEmulatorTraceIO' def emConfig trace
