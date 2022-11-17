{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE LambdaCase          #-}

{-|
Module      : Tests.Prop.Escrow
Description : OffChain code for Escrow Contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

Property Base Testing module. This approach helps us to auto-generate the contract
action traces that can be run by the emulator and then check the result of every
run fits with some specification of the contract.
-}

module Tests.Prop.Escrow where

import Ledger.Value qualified as Value
import Plutus.Contract
import Plutus.Contract.Test ( CheckOptions, defaultCheckOptions, emulatorConfig
                            , mockWalletPaymentPubKeyHash, w1, w2, w3, w4
                            , Wallet, TracePredicate, assertBlockchain
                            )
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value (assetClassValue, singleton, assetClass, geq, valueOf)
import Ledger hiding (singleton)
import Control.Monad
import Control.Lens hiding (elements)
import Data.Data
import Data.List (sort, delete)
import Data.Maybe (fromJust, isJust)
import PlutusTx.List qualified as PTx
import PlutusTx.AssocMap qualified as PTx
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text hiding (null, last, filter, singleton, head, length)

import Test.QuickCheck (Gen, Property, shrink, oneof, elements, chooseInteger, tabulate)

import Escrow
import Utils.OffChain
import Utils.OnChain (minAda)
import Tests.Utils hiding (wallets)

import qualified PlutusTx.AssocMap as AM
import Escrow.Business()
import Escrow.Types()
import PlutusTx hiding (Data)
import Ledger.Scripts

-- | Config the checkOptions to use the same emulator config as the Offchain traces.
options :: CheckOptions
options = defaultCheckOptions & emulatorConfig .~ emConfig

-- | Wallets that will be used to test the endpoints
wallets :: [Wallet]
wallets = [w1, w2, w3, w4]

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

mockPKH :: Wallet -> PaymentPubKeyHash
mockPKH = mockWalletPaymentPubKeyHash

mockAddress :: Wallet -> Address
mockAddress = flip pubKeyHashAddress Nothing . mockPKH

data TransferInfo = TransferInfo
                    { tiSenderWallet      :: Wallet
                    , tiSendAmount        :: Integer
                    , tiSendAssetClass    :: AssetClass
                    , tiReceiveAmount     :: Integer
                    , tiReceiveAssetClass :: AssetClass
                    }
    deriving (Show, Eq, Data)

newtype EscrowModel = EscrowModel
                      { _toResolve :: Map.Map Wallet [TransferInfo] }
    deriving (Show, Eq, Data)

makeLenses 'EscrowModel

deriving instance Eq   (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

instance CM.ContractModel EscrowModel where

    data Action EscrowModel =
          Start Wallet Wallet (AssetClass, Integer) (AssetClass, Integer)
        | Resolve Wallet TransferInfo
        deriving (Eq, Show, Data)

    data ContractInstanceKey EscrowModel w s e params where
        UserH :: Wallet
              -> CM.ContractInstanceKey EscrowModel () EscrowSchema Text ()

    initialInstances = []

    initialState = EscrowModel { _toResolve = Map.empty }

    startInstances _ (Start sw _ _ _) = [CM.StartContract (UserH sw) ()]
    startInstances _ (Resolve rw _)   = [CM.StartContract (UserH rw) ()]

    instanceWallet (UserH w) = w

    instanceContract _ (UserH w) _ = endpoints $ mockAddress w

    arbitraryAction s = do
        connWallet <- genWallet
        let toRes = Map.lookup connWallet (s ^. CM.contractState . toResolve)
        oneof $
            genStart connWallet :
            [ genResolve connWallet (fromJust toRes)
            | not (null $ fromJust toRes) && isJust toRes
            ]
      where
        genWallet :: Gen Wallet
        genWallet = elements wallets
        genPayment :: Gen Integer
        genPayment = chooseInteger (1, 50)

        genStart :: Wallet -> Gen (CM.Action EscrowModel)
        genStart connWallet = do
            resW <- elements (delete connWallet wallets)
            p1 <- genPayment
            p2 <- genPayment
            return $ Start connWallet resW
                           (assetClass tokenACurrencySymbol tokenA, p1)
                           (assetClass tokenBCurrencySymbol tokenB, p2)

        genResolve :: Wallet -> [TransferInfo] -> Gen (CM.Action EscrowModel)
        genResolve connWallet toRes = Resolve connWallet <$> elements toRes

    precondition _ Start{} =
        -- Must check the wallet has enough funds.
        True
    precondition s (Resolve rw ti) =
        -- Must check the wallet has enough funds.
        ti `elem` (s ^. CM.contractState . toResolve) Map.! rw

    nextState (Start sendW resW (acA,aA) (acB,aB)) = do
        CM.withdraw sendW (minAda <> singleton tokenACurrencySymbol tokenA aA)
        toResolve %= Map.insertWith (++) resW [TransferInfo sendW aA acA aB acB]
        CM.wait 2
    nextState (Resolve connW TransferInfo{..}) = do
        CM.withdraw connW (assetClassValue tiSendAssetClass tiSendAmount)
        CM.deposit connW (assetClassValue tiReceiveAssetClass tiReceiveAmount)
        CM.deposit tiSenderWallet (assetClassValue tiSendAssetClass tiSendAmount)
        CM.withdraw tiSenderWallet (assetClassValue tiReceiveAssetClass tiReceiveAmount)
        CM.wait 2

    perform h _ _ (Start sendW resW (acA,aA) (acB,aB)) = do
        Trace.callEndpoint @"start" (h $ UserH sendW) $ StartParams
            { receiverAddress   = mkReceiverAddress $ mockAddress resW
            , sendAmount        = aA
            , sendAssetClass    = acA
            , receiveAmount     = aB
            , receiveAssetClass = acB
            }
        CM.delay 2
    perform h _ _ (Resolve connW TransferInfo{..}) = do
        utxos <- Trace.chainState <&> unspentOutputs . (^. Trace.chainNewestFirst)
        let scriptUtxos   = findScriptTxOutRef utxos
            resolveParams = ResolveParams { rpTxOutRef = head scriptUtxos }
        Trace.callEndpoint @"resolve" (h $ UserH connW) resolveParams
        CM.delay 2

    shrinkAction _ _ = []

    monitoring _ _ = id

propEscrow :: CM.Actions EscrowModel -> Property
propEscrow = CM.propRunActionsWithOptions options CM.defaultCoverageOptions
             (\ _ -> pure True)
