{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}

{-|
Module      : Tests.Prop.Escrow
Description : OffChain code for Escrow Contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

Property Base Testing module. This approach helps us to auto-generate the
contract action traces that can be run by the emulator and then check the result
of every run fits with some specification of the contract.
-}

module Tests.Prop.Escrow where

-- Non-IOG imports
import Control.Lens    ( (^.), (.~), (&), makeLenses )
import Data.Data       ( Data )
import Data.List       ( delete, find )
import Data.Maybe      ( fromJust, isJust )
import Data.Monoid     ( Last (..) )
import Data.Text       ( Text )
import Data.Map as Map ( (!), Map, empty, lookup, member, insertWith, adjust )

import Test.QuickCheck ( Gen, Property, oneof, elements, chooseInteger )

-- IOG imports
import Plutus.Contract.Test               ( CheckOptions, Wallet
                                          , defaultCheckOptions, emulatorConfig
                                          )
import Plutus.Contract.Test.ContractModel ( ($~), ContractInstanceKey
                                          , StartContract(..), ContractModel(..)
                                          , Action, Actions, contractState
                                          , defaultCoverageOptions, delay
                                          , deposit, propRunActionsWithOptions
                                          , wait, withdraw
                                          )
import Plutus.Trace.Emulator              ( callEndpoint, observableState )
import Plutus.V1.Ledger.Value             ( assetClassValue, assetClassValueOf
                                          , singleton, assetClass
                                          )
import Ledger                             ( AssetClass )

-- Escrow imports
import Escrow.Business                 ( EscrowInfo, mkReceiverAddress
                                       , mkEscrowInfo, mkSenderAddress
                                       )
import Escrow.OffChain.Actions         ( EscrowSchema, endpoints )
import Escrow.OffChain.Parameters      ( mkResolveParams, mkStartParams
                                       , mkCancelParams
                                       )
import Escrow.OffChain.ObservableState ( UtxoEscrowInfo(..) )
import Utils.OnChain                   ( minAda )
import Tests.Utils                     ( emConfig, tokenA, tokenACurrencySymbol
                                       , tokenB, tokenBCurrencySymbol, wallets
                                       , mockAddress
                                       )

-- | Config the checkOptions to use the emulator config from the Offchain traces
options :: CheckOptions
options = defaultCheckOptions & emulatorConfig .~ emConfig

{- | The representation of the EscrowInfo plus the Value contained in script
     Utxo.
-}
data TransferInfo = TransferInfo
                    { tiSenderWallet      :: Wallet
                    , tiSendAmount        :: Integer
                    , tiSendAssetClass    :: AssetClass
                    , tiReceiveAmount     :: Integer
                    , tiReceiveAssetClass :: AssetClass
                    }
    deriving (Show, Eq, Data)

-- | This type represent all the Escrows a Wallet can resolve
newtype EscrowModel = EscrowModel
                      { _toResolve :: Map Wallet [TransferInfo] }
    deriving (Show, Eq, Data)

makeLenses 'EscrowModel

deriving instance Eq   (ContractInstanceKey EscrowModel w s e params)
deriving instance Show (ContractInstanceKey EscrowModel w s e params)

instance ContractModel EscrowModel where

{- | Actions that can be done using the contract.
   Start: Starts a new Escrow with the given parameters.
     - Parameters:
        Wallet: The sender wallet
        Wallet: The receiver wallet
        (AssetClass, Integer): The AssetClass and amount of the send Asset
        (AssetClass, Integer): The AssetClass and amount of the receive Asset

   Resolve: Resolve the specific Escrow given the TransferInfo
     - Parameters:
        Wallet: The receiver wallet
        TransferInfo: The Information about the Utxo

   Cancel: Cancels an existing Escrow
     - Parameters:
        Wallet: The receiver wallet
        TransferInfo: The information about the Utxo
-}
    data Action EscrowModel =
          Start Wallet Wallet (AssetClass, Integer) (AssetClass, Integer)
        | Resolve Wallet TransferInfo
        | Cancel Wallet TransferInfo
        deriving (Eq, Show, Data)

    data ContractInstanceKey EscrowModel w s e params where
        UserH :: Wallet
              -> ContractInstanceKey EscrowModel (Last [UtxoEscrowInfo])
                                        EscrowSchema Text ()

    initialInstances = []

    initialState = EscrowModel { _toResolve = empty }

    startInstances _ (Start sw _ _ _) = [ StartContract (UserH sw) () ]
    startInstances _ (Resolve rw _)   = [ StartContract (UserH rw) () ]
    startInstances _ (Cancel rw ti)   = [ StartContract (UserH rw) ()
                                        , StartContract (UserH sw) ()
                                        ]
      where
        sw = tiSenderWallet ti

    instanceWallet (UserH w) = w

    instanceContract _ (UserH w) _ = endpoints $ mockAddress w

    arbitraryAction s = do
        connWallet <- genWallet
        let toRes = Map.lookup connWallet (s ^. contractState . toResolve)
        oneof $
            genStart connWallet :
            [ genResolve connWallet (fromJust toRes)
            | isJust toRes && not (null $ fromJust toRes)
            ] ++
            [ genCancel connWallet (fromJust toRes)
            | isJust toRes && not (null $ fromJust toRes)
            ]
      where
        genWallet :: Gen Wallet
        genWallet = elements wallets
        genPayment :: Gen Integer
        genPayment = chooseInteger (1, 50)

        genStart :: Wallet -> Gen (Action EscrowModel)
        genStart connWallet = do
            resW <- elements (delete connWallet wallets)
            p1 <- genPayment
            p2 <- genPayment
            return $ Start connWallet resW
                           (assetClass tokenACurrencySymbol tokenA, p1)
                           (assetClass tokenBCurrencySymbol tokenB, p2)

        genResolve :: Wallet -> [TransferInfo] -> Gen (Action EscrowModel)
        genResolve connWallet toRes = Resolve connWallet <$> elements toRes

        genCancel :: Wallet -> [TransferInfo] -> Gen (Action EscrowModel)
        genCancel resWallet toCancel = Cancel resWallet <$> elements toCancel

    precondition _ Start{} =
        -- Must check the wallet has enough funds.
        True
    precondition s (Resolve rw ti) =
        -- Must check the wallet has enough funds.
        member rw (s ^. contractState . toResolve) &&
            ti `elem` (s ^. contractState . toResolve) ! rw
    precondition s (Cancel rw ti) =
        -- Must check the wallet has enough funds.
        member rw (s ^. contractState . toResolve) &&
            ti `elem` (s ^. contractState . toResolve) ! rw

    nextState (Start sendW resW (acA,aA) (acB,aB)) = do
        withdraw sendW (minAda <> singleton tokenACurrencySymbol tokenA aA)
        toResolve $~ insertWith (++) resW [TransferInfo sendW aA acA aB acB]
        wait 2
    nextState (Resolve resW ti@TransferInfo{..}) = do
        let rVal = assetClassValue tiReceiveAssetClass tiReceiveAmount
            sVal = assetClassValue tiSendAssetClass tiSendAmount
        deposit resW sVal
        deposit tiSenderWallet (minAda <> rVal)
        withdraw resW rVal
        toResolve $~ adjust (delete ti) resW
        wait 8
    nextState (Cancel resW ti@TransferInfo{..}) = do
        let sVal = assetClassValue tiSendAssetClass tiSendAmount
        deposit tiSenderWallet (minAda <> sVal)
        toResolve $~ adjust (delete ti) resW
        wait 7

    perform h _ _ (Start sendW resW (acA,aA) (acB,aB)) = do
        callEndpoint @"start" (h $ UserH sendW) $
            mkStartParams (mkReceiverAddress $ mockAddress resW) aA acA aB acB
        delay 2
    perform h _ _ (Resolve resW ti) = do
        callEndpoint @"reload" (h $ UserH resW) ()
        delay 5
        Last obsState <- observableState $ h $ UserH resW
        let utxoEscrowInfo = fromJust $ findEscrowUtxo ti (fromJust obsState)
        callEndpoint @"resolve" (h $ UserH resW) $
            mkResolveParams (escrowUtxo utxoEscrowInfo)
        delay 2
    perform h _ _ (Cancel resW ti) = do
        callEndpoint @"reload" (h $ UserH resW) ()
        delay 5
        Last obsState <- observableState $ h $ UserH resW
        let utxoEscrowInfo = fromJust $ findEscrowUtxo ti (fromJust obsState)
        callEndpoint @"cancel" (h $ UserH (tiSenderWallet ti)) $
            mkCancelParams (escrowUtxo utxoEscrowInfo) (mockAddress resW)
        delay 2

    shrinkAction _ _ = []

    monitoring _ _ = id

-- | Finds an specific UtxoEscrowInfo from a list using the TransferInfo
findEscrowUtxo :: TransferInfo -> [UtxoEscrowInfo] -> Maybe UtxoEscrowInfo
findEscrowUtxo TransferInfo{..} =
    find (\utxoInfo -> escrowInfo utxoInfo == eInfo
                    && sendA utxoInfo == tiSendAmount)
  where
    eInfo :: EscrowInfo
    eInfo = mkEscrowInfo (mkSenderAddress $ mockAddress tiSenderWallet)
                         tiReceiveAmount
                         tiReceiveAssetClass

    sendA :: UtxoEscrowInfo -> Integer
    sendA uInfo = assetClassValueOf (escrowValue uInfo) tiSendAssetClass

propEscrow :: Actions EscrowModel -> Property
propEscrow = propRunActionsWithOptions options defaultCoverageOptions
             (\ _ -> pure True)
