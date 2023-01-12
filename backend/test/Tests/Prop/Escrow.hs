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
import Control.Lens    ( (^.), (.~), (&), makeLenses, over )
import Data.Data       ( Data )
import Data.Default    ( def )
import Data.List       ( delete, find )
import Data.Maybe      ( fromJust, isJust )
import Data.Monoid     ( Last (..) )
import Data.Text       ( Text , pack , append )
import Data.Map as Map ( (!), Map, empty, lookup, member
                       , insertWith, adjust, fromList
                       )
import Data.ByteString.UTF8 ( fromString )
import Text.Hex        ( decodeHex)

import Test.QuickCheck ( Gen, Property
                       , oneof, elements, chooseInteger, tabulate
                       , suchThat
                       )

-- IOG imports
import Plutus.Contract.Test               ( CheckOptions, Wallet
                                          , defaultCheckOptions, emulatorConfig
                                          )
import Plutus.Contract.Test.ContractModel ( ($~), ContractInstanceKey
                                          , StartContract(..), ContractModel(..)
                                          , Action, Actions
                                          , contractState
                                          , defaultCoverageOptions, delay
                                          , deposit, propRunActionsWithOptions
                                          , wait, withdraw
                                          )
import Plutus.Trace.Emulator              ( EmulatorConfig (EmulatorConfig)
                                          , callEndpoint, observableState
                                          , params
                                          )
import Plutus.V1.Ledger.Value             ( Value, CurrencySymbol, TokenName
                                          , assetClassValue, assetClass
                                          , tokenName, currencySymbol
                                          )
import Ledger                             ( AssetClass, Params(..)
                                          , protocolParamsL )
import Ledger.Ada                         ( lovelaceValueOf )
import Cardano.Api.Shelley                (ProtocolParameters(..))

-- Escrow imports
import Escrow.Business            ( EscrowInfo, mkReceiverAddress
                                  , mkEscrowInfo, mkSenderAddress
                                  )
import Escrow.OffChain.Interface  ( UtxoEscrowInfo(..)
                                  , mkResolveParams, mkStartParams
                                  , mkCancelParams
                                  )
import Escrow.OffChain.Operations ( EscrowSchema, endpoints )
import Utils.OnChain              ( minAda )
import Tests.Utils                ( wallets, mockWAddress
                                  )

mkTokenName :: String -> TokenName
mkTokenName = tokenName . fromString

mkCurrencySymbol :: String -> CurrencySymbol
mkCurrencySymbol = currencySymbol . fromJust . decodeHex . pack
                 . ("246ea4f1fd944bc8b0957050a31ab0487016be233725c9f931b1" ++)

-- | List of AssetClass for emulator configuration
tokens :: [AssetClass]
tokens = [assetClass
            (mkCurrencySymbol $ replicate 4 hex )
            (mkTokenName $ replicate n hex)
         | hex <- hexChars
         , n <- [1..4]
         ]
    where
        hexChars = ['a' .. 'f']

walletsWithValue :: [(Wallet,Value)]
walletsWithValue = [(w, v <>  mconcat (map (`assetClassValue` 1_000_000) tokens))
                   | w <- wallets
                   ]
  where
    v :: Value
    v = lovelaceValueOf 100_000_000

-- | emulator configuration
emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ fromList walletsWithValue) def

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

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

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

    instanceContract _ (UserH w) _ = endpoints $ mockWAddress w

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
        genStart :: Wallet -> Gen (Action EscrowModel)
        genStart connWallet = do
            resW <- elements (delete connWallet wallets)
            uncurry (Start connWallet resW) <$> gen2Tokens

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
        withdraw sendW (minAda <> assetClassValue acA aA)
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
            mkStartParams (mkReceiverAddress $ mockWAddress resW) aA acA aB acB
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
            mkCancelParams (escrowUtxo utxoEscrowInfo)
                           (mkReceiverAddress $ mockWAddress resW)
        delay 2

    shrinkAction _ (Start sw rw sv rv) =
           [Start sw' rw sv rv | sw' <- shrinkWallet sw]
        ++ [Start sw rw' sv rv | rw' <- shrinkWallet rw]
    shrinkAction _ (Resolve w ti) =
           [Resolve w' ti | w' <- shrinkWallet w]
    shrinkAction _ (Cancel w ti) =
           [Cancel w' ti | w' <- shrinkWallet w]

    monitoring _ (Start _ _ (ac,_) _) =
        tabulate "Starting escrow" [show ac]
    monitoring _ (Resolve rw _) =
        tabulate "Reslving escrow" [show rw]
    monitoring _ (Cancel rw _) =
        tabulate "Cancelling escrow" [show rw]

-- | Finds an specific UtxoEscrowInfo from a list using the TransferInfo
findEscrowUtxo :: TransferInfo -> [UtxoEscrowInfo] -> Maybe UtxoEscrowInfo
findEscrowUtxo TransferInfo{..} =
    find (\utxoInfo -> escrowInfo utxoInfo == eInfo
                    && sendA utxoInfo == tiSendAmount)
  where
    eInfo :: EscrowInfo
    eInfo = mkEscrowInfo (mkSenderAddress $ mockWAddress tiSenderWallet)
                         tiReceiveAmount
                         tiReceiveAssetClass

    sendA :: UtxoEscrowInfo -> Integer
    sendA = snd . escrowPayment

propEscrow :: Actions EscrowModel -> Property
propEscrow = propRunActionsWithOptions
             (options & increaseMaxCollateral)
             defaultCoverageOptions
             (\ _ -> pure True)

{- increasing max amount of collateral inputs,
   otherwise property tests fail due to tooManyCollateralInputs
   error.
-}
increaseMaxCollateral:: CheckOptions -> CheckOptions
increaseMaxCollateral = over (emulatorConfig . params) increaseMaxCollIn

increaseMaxCollIn :: Params -> Params
increaseMaxCollIn = over protocolParamsL fixParams
  where
    fixParams pp = pp
      { protocolParamMaxCollateralInputs = Just 200}

genWallet :: Gen Wallet
genWallet = elements wallets

genPayment :: Gen Integer
genPayment = chooseInteger (1, 50)

genAssetClass :: Gen AssetClass
genAssetClass = elements tokens

gen2Tokens :: Gen ((AssetClass, Integer), (AssetClass,Integer))
gen2Tokens = do
    ac1 <- genAssetClass
    ac2 <- suchThat genAssetClass (/= ac1)
    p1 <- genPayment
    p2 <- genPayment
    return ((ac1, p1), (ac2, p2))
