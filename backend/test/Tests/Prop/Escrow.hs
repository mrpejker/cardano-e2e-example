{-|
Module      : Tests.Prop.Escrow
Description : Escrow dApp property checks.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

Property Base Testing module. This approach helps us to auto-generate the
contract action traces that can be run by the emulator and then check the result
of every run fits with some specification of the contract.
-}

module Tests.Prop.Escrow where

-- Non-IOG imports
import Control.Lens    ( (.~), (&), over )
import Control.Arrow   ( second )
import Data.Default    ( def )
import Data.Maybe      ( fromJust )
import Data.Map as Map ( lookup, member, fromList, toList )
import Test.QuickCheck ( Property )

-- IOG imports
import Plutus.Contract.Test               ( CheckOptions, Wallet
                                          , defaultCheckOptions, emulatorConfig
                                          )
import Plutus.Contract.Test.ContractModel ( DL
                                          , Actions, NoLockedFundsProof(..)
                                          , action, defaultNLFP
                                          , checkNoLockedFundsProofWithOptions
                                          , defaultCoverageOptions
                                          , propRunActionsWithOptions
                                          , viewContractState
                                          )
import Plutus.Trace.Emulator              ( EmulatorConfig (EmulatorConfig)
                                          , params
                                          )
import Ledger                             ( Params(..), protocolParamsL )
import Cardano.Api.Shelley                ( ProtocolParameters(..) )

-- Escrow imports
import Tests.Prop.EscrowModel ( EscrowModel, Action(..), toResolve )
import Tests.Prop.Gen         ( walletsWithValue )
import Tests.Prop.Extra       ( tiSenderWallet )
import Tests.Utils            ( wallets )

-- | Basic property testing.
propBasic :: Actions EscrowModel -> Property
propBasic = propRunActionsWithOptions
            (options & increaseMaxCollateral)
            defaultCoverageOptions
            (const $ pure True)

-- | No locked funds property testing.
propNoLockedFunds :: Property
propNoLockedFunds = checkNoLockedFundsProofWithOptions
                    (options & increaseMaxCollateral) noLockProof

-- | No locked funds proofs.
noLockProof :: NoLockedFundsProof EscrowModel
noLockProof = defaultNLFP
              { nlfpMainStrategy   = finishingMainStrategy
              , nlfpWalletStrategy = finishingWalletStrategy
              }

-- | Main Strategy to return all the funds locked by the contract
--  at any reachable state
finishingMainStrategy :: DL EscrowModel ()
finishingMainStrategy = do
    resolveMap <- viewContractState toResolve
    sequence_ [ action (Cancel w tInfo)
              | w <- wallets
              , w `Map.member` resolveMap
              , tInfo <- fromJust $ Map.lookup w resolveMap
              ]

-- | Strategy for the wallets to recover all their funds as
--   if the main strategy was not performed
finishingWalletStrategy :: Wallet -> DL EscrowModel ()
finishingWalletStrategy w = do
    resolveMap <- viewContractState toResolve
    sequence_ [ action (Cancel resW tInfo)
              | (resW, tInfos) <-
                      map (second (filter ((w==) . tiSenderWallet)))
                          (Map.toList resolveMap)
              , tInfo <- tInfos
              ]

-- | Emulator configuration.
emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ fromList walletsWithValue) def

-- | Config the checkOptions to use the emulator config from the Offchain traces.
options :: CheckOptions
options = defaultCheckOptions & emulatorConfig .~ emConfig

{- Increasing max amount of collateral inputs, otherwise property tests fail due
   to tooManyCollateralInputs error.
-}
increaseMaxCollateral:: CheckOptions -> CheckOptions
increaseMaxCollateral = over (emulatorConfig . params) increaseMaxCollIn
    where
      increaseMaxCollIn :: Params -> Params
      increaseMaxCollIn = over protocolParamsL $
          \pp -> pp { protocolParamMaxCollateralInputs = Just 200 }
