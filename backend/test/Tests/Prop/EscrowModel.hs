{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}

{-|
Module      : Tests.Prop.EscrowModel
Description : EscrowModel instance implementation.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

-}

module Tests.Prop.EscrowModel where

-- Non-IOG imports
import Control.Lens    ( (^.), makeLenses )
import Data.Data       ( Data )
import Data.List       ( delete )
import Data.Maybe      ( fromJust, isJust )
import Data.Monoid     ( Last (..) )
import Data.Text       ( Text )
import Data.Map as Map ( (!), Map, empty, lookup, member
                       , insertWith, adjust
                       )
import Test.QuickCheck ( Property, Gen, oneof, elements, tabulate )

-- IOG imports
import Plutus.Contract.Test               ( Wallet )
import Plutus.Contract.Test.ContractModel ( ($~), ContractInstanceKey
                                          , StartContract(..), ContractModel(..)
                                          , Action
                                          , ModelState, HandleFun, SymToken
                                          , SpecificationEmulatorTrace, Spec
                                          , contractState
                                          , delay, deposit
                                          , wait, withdraw
                                          )
import Plutus.Trace.Emulator              ( callEndpoint, observableState )
import Plutus.V1.Ledger.Value             ( assetClassValue )
import Ledger                             ( AssetClass )

-- Escrow imports
import Escrow.Business            ( mkReceiverAddress )
import Escrow.OffChain.Interface  ( UtxoEscrowInfo(..)
                                  , mkResolveParams, mkStartParams
                                  , mkCancelParams
                                  )
import Escrow.OffChain.Operations ( EscrowSchema, endpoints )
import Utils.OnChain              ( minAda )
import Tests.Utils                ( wallets, mockWAddress )
import Tests.Prop.Gen             ( genWallet, gen2Tokens )
import Tests.Prop.Extra           ( ExchangeInfo(..)
                                  , LookupSchema, lookupEndpoint, findEscrowUtxo
                                  )

-- | This type represent all the Escrows a Wallet can resolve
newtype EscrowModel = EscrowModel
                      { _toResolve :: Map Wallet [ExchangeInfo] }
    deriving (Show, Eq, Data)

makeLenses 'EscrowModel

emptyEscrowModel :: EscrowModel
emptyEscrowModel = EscrowModel { _toResolve = empty }

deriving instance Eq   (ContractInstanceKey EscrowModel w s e params)
deriving instance Show (ContractInstanceKey EscrowModel w s e params)

instance ContractModel EscrowModel where
    {- | Actions that can be done using the contract.
         - Start: Starts a new Escrow with the given parameters.
         - Resolve: Resolve the specific Escrow given the ExchangeInfo
         - Cancel: Cancels an existing Escrow
    -}
    data Action EscrowModel =
        Start { sWallet :: Wallet -- ^ Sender wallet
              , rWallet :: Wallet -- ^ Receiver wallet
              , sPay    :: (AssetClass, Integer)
              -- ^ AssetClass and amount of the send Asset
              , rPay    :: (AssetClass, Integer)
              -- ^ AssetClass and amount of the receive Asset
              }
        | Resolve { rWallet :: Wallet     -- ^ Receiver wallet
                  , eInfo :: ExchangeInfo -- ^ Exchange information
                  }
        | Cancel { rWallet :: Wallet     -- ^ Receiver wallet
                 , eInfo :: ExchangeInfo -- ^ Exchange information
                 }
        deriving (Eq, Show, Data)

    {- | Two kinds of handlers, the standard related to the Escrow dApp, and
         one that allows the wallet to lookup for specific escrows to cancel.
    -}
    data ContractInstanceKey EscrowModel w s e params where
        UserH :: Wallet
              -> ContractInstanceKey EscrowModel (Last [UtxoEscrowInfo])
                                     EscrowSchema Text ()
        LookupH :: Wallet
                -> ContractInstanceKey EscrowModel (Last [UtxoEscrowInfo])
                                       LookupSchema Text ()

    initialInstances = []

    instanceWallet (UserH w)   = w
    instanceWallet (LookupH w) = w

    instanceContract _ (UserH w)   _ = endpoints $ mockWAddress w
    instanceContract _ (LookupH _) _ = lookupEndpoint

    initialState   = emptyEscrowModel
    startInstances = eStartInstances

    -- | Arbitrary escrow model actions.
    arbitraryAction = eArbitraryAction

    precondition = ePrecondition

    -- | Escrow model specification.
    nextState = escrowSpecification
    -- | Escrow semantics using the emulator.
    perform   = escrowSemantics

    shrinkAction = eShrinkAction
    monitoring = eMonitoring

-- | Start new contract instances.
eStartInstances
    :: ModelState EscrowModel
    -> Action EscrowModel
    -> [StartContract EscrowModel]
eStartInstances _ Start{sWallet}   = [ StartContract (UserH sWallet) () ]
eStartInstances _ Resolve{rWallet} = [ StartContract (UserH rWallet) () ]
eStartInstances _ Cancel{eInfo}    = [ StartContract (LookupH sw) ()
                                     , StartContract (UserH sw) ()
                                     ]
  where
    sw = tiSenderWallet eInfo

-- | Arbitrary escrow model actions.
eArbitraryAction :: ModelState EscrowModel -> Gen (Action EscrowModel)
eArbitraryAction s = do
    connWallet <- genWallet
    let toRes = Map.lookup connWallet (s ^. contractState . toResolve)
    oneof $ genStart connWallet :
        [ genResolve connWallet (fromJust toRes)
        | isJust toRes && not (null $ fromJust toRes)
        ] ++
        [ genCancel connWallet (fromJust toRes)
        | isJust toRes && not (null $ fromJust toRes)
        ]
  where
      genStart :: Wallet -> Gen (Action EscrowModel)
      genStart connWallet = do
          resW <- elements (delete connWallet wallets)
          uncurry (Start connWallet resW) <$> gen2Tokens

      genResolve :: Wallet -> [ExchangeInfo] -> Gen (Action EscrowModel)
      genResolve connWallet toRes = Resolve connWallet <$> elements toRes

      genCancel :: Wallet -> [ExchangeInfo] -> Gen (Action EscrowModel)
      genCancel resWallet toCancel = Cancel resWallet <$> elements toCancel

ePrecondition :: ModelState EscrowModel -> Action EscrowModel -> Bool
ePrecondition _ Start{} = True
ePrecondition s action = member rw (s ^. contractState . toResolve)
                         &&
                         ti `elem` (s ^. contractState . toResolve) ! rw
    where
      rw = rWallet action
      ti = eInfo action

-- | Escrow model specification.
escrowSpecification :: Action EscrowModel -> Spec EscrowModel ()
escrowSpecification Start{sWallet,rWallet,sPay,rPay} = do
    let (acA, aA) = sPay
        (acB, aB) = rPay

    withdraw sWallet (minAda <> assetClassValue acA aA)

    toResolve $~ insertWith (++) rWallet [ExchangeInfo sWallet aA acA aB acB]
    wait 2
escrowSpecification Resolve{rWallet, eInfo} = do
    let ExchangeInfo{..} = eInfo
        rVal = assetClassValue tiReceiveAssetClass tiReceiveAmount
        sVal = assetClassValue tiSendAssetClass tiSendAmount

    deposit rWallet sVal
    deposit tiSenderWallet (minAda <> rVal)
    withdraw rWallet rVal

    toResolve $~ adjust (delete eInfo) rWallet
    wait 8
escrowSpecification Cancel{rWallet, eInfo} = do
    let ExchangeInfo{..} = eInfo
        sVal = assetClassValue tiSendAssetClass tiSendAmount

    deposit tiSenderWallet (minAda <> sVal)

    toResolve $~ adjust (delete eInfo) rWallet
    wait 7

-- | Escrow semantics using the emulator.
escrowSemantics
    :: HandleFun EscrowModel
    -> (SymToken -> AssetClass)
    -> ModelState EscrowModel
    -> Action EscrowModel
    -> SpecificationEmulatorTrace ()
escrowSemantics h _ _ Start{sWallet,rWallet,sPay,rPay} = do
    let (acA, aA) = sPay
        (acB, aB) = rPay

    callEndpoint @"start" (h $ UserH sWallet) $
        mkStartParams (mkReceiverAddress $ mockWAddress rWallet) aA acA aB acB
    delay 2
escrowSemantics h _ _ Resolve{rWallet, eInfo} = do
    callEndpoint @"reload" (h $ UserH rWallet) ()
    delay 5
    Last obsState <- observableState $ h $ UserH rWallet
    let utxoEscrowInfo = fromJust $ findEscrowUtxo eInfo (fromJust obsState)

    callEndpoint @"resolve" (h $ UserH rWallet) $
        mkResolveParams (escrowUtxo utxoEscrowInfo)
    delay 2
escrowSemantics h _ _ Cancel{rWallet, eInfo} = do
    let sendW = tiSenderWallet eInfo

    callEndpoint @"lookup" (h $ LookupH sendW) (mockWAddress rWallet)
    delay 5
    Last obsState <- observableState $ h $ LookupH sendW
    let utxoEscrowInfo = fromJust $ findEscrowUtxo eInfo (fromJust obsState)

    callEndpoint @"cancel" (h $ UserH sendW) $
        mkCancelParams (escrowUtxo utxoEscrowInfo)
                       (mkReceiverAddress $ mockWAddress rWallet)
    delay 2

eShrinkAction
    :: ModelState EscrowModel
    -> Action EscrowModel
    -> [Action EscrowModel]
eShrinkAction _ Start{sWallet,rWallet,sPay,rPay} =
    [Start sWallet' rWallet sPay rPay | sWallet' <- shrinkWallet sWallet]
    ++
    [Start sWallet rWallet' sPay rPay | rWallet' <- shrinkWallet rWallet]
eShrinkAction _ Resolve{rWallet, eInfo} =
    [Resolve rWallet' eInfo | rWallet' <- shrinkWallet rWallet]
eShrinkAction _ Cancel{rWallet, eInfo} =
    [Cancel rWallet' eInfo | rWallet' <- shrinkWallet rWallet]

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

eMonitoring
    :: (ModelState EscrowModel, ModelState EscrowModel)
    -> Action EscrowModel
    -> Property
    -> Property
eMonitoring _ Start{sWallet}   = tabulate "Starting escrow" [show sWallet]
eMonitoring _ Resolve{rWallet} = tabulate "Reslving escrow" [show rWallet]
eMonitoring _ Cancel{rWallet}  = tabulate "Cancelling escrow" [show rWallet]
