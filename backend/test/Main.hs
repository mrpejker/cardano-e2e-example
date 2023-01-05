{-|
Module      : Main
Description : Main Testing module
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Main ( main ) where

-- Non-IOG imports
import Test.Tasty
-- QuickCheck tests are disabled
-- import Test.QuickCheck

-- import Tests.Prop.Escrow
import Tests.OffChain.Trace0 qualified
import Tests.OffChain.Trace1 qualified
import Tests.OffChain.Trace2 qualified
import Tests.OffChain.Trace3 qualified

main :: IO ()
main = do
    -- QuickCheck tests are disabled
    -- quickCheck propEscrow
    defaultMain tests

tests :: TestTree
tests = testGroup "Project tests"
         [ offChainTests ]

offChainTests :: TestTree
offChainTests = testGroup "OffChain Tests"
                [ Tests.OffChain.Trace0.test
                , Tests.OffChain.Trace1.test
                , Tests.OffChain.Trace2.test
                , Tests.OffChain.Trace3.test
                ]
