{-# LANGUAGE DeriveDataTypeable  #-}

{-|
Module      : Tests.Prop.Extra
Description : Extra Schema and implementation for the EscrowModel definition.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

-}

module Tests.Prop.Extra where

-- Non-IOG imports
import Control.Monad ( forever )
import Data.Data     ( Data )
import Data.List     ( find )
import Data.Monoid   ( Last (..) )
import Data.Text     ( Text )

-- IOG imports
import Plutus.Contract      ( Contract, Promise, Endpoint
                            , endpoint
                            , handleError, logError, awaitPromise
                            )
import Plutus.Contract.Test ( Wallet )
import Ledger               ( AssetClass )

-- Escrow imports
import Escrow.Business            ( EscrowInfo
                                  , mkEscrowInfo, mkSenderAddress
                                  )
import Escrow.OffChain.Interface  ( ObservableState, UtxoEscrowInfo(..) )
import Escrow.OffChain.Operations ( reloadOp )
import Utils.WalletAddress        ( WalletAddress )
import Tests.Utils                ( mockReloadFlag, mockWAddress )

{- | The representation of the EscrowInfo plus the Value contained in script
     Utxo.
-}
data ExchangeInfo =
    ExchangeInfo
    { tiSenderWallet      :: Wallet
    -- ^ The wallet that starts the escrow.
    , tiSenderAmount        :: Integer
    -- ^ The amount of tokens the sender locks in the script utxo.
    , tiSenderAssetClass    :: AssetClass
    -- ^ The asset class of the tokens the sender locks in the script utxo.
    , tiReceiverAmount     :: Integer
    -- ^ The amount of tokens the receiver must send to resolve the escrow.
    , tiReceiverAssetClass :: AssetClass
    {- ^ The asset class of the tokens the receiver must send to resolve the
         escrow. -}
    }
    deriving (Show, Eq, Data)

{- | LookupSchema to let the sender wallet call the reload operation to find the
     escrow utxo.
-}
type LookupSchema = Endpoint "lookup" WalletAddress

-- | Lookup endpoint to call the reload operation.
lookupEndpoint
    :: Contract (Last ObservableState) LookupSchema Text ()
lookupEndpoint = forever $ handleError logError $ awaitPromise lookupEp
  where
    lookupEp :: Promise (Last ObservableState) LookupSchema Text ()
    lookupEp = endpoint @"lookup" $ flip reloadOp mockReloadFlag

-- | Finds an specific UtxoEscrowInfo from a list using the TransferInfo
findEscrowUtxo :: ExchangeInfo -> [UtxoEscrowInfo] -> Maybe UtxoEscrowInfo
findEscrowUtxo ExchangeInfo{..} = find $
    \utxoInfo -> escrowInfo utxoInfo == eInfo && sendA utxoInfo == tiSenderAmount
  where
    eInfo :: EscrowInfo
    eInfo = mkEscrowInfo (mkSenderAddress $ mockWAddress tiSenderWallet)
                         tiReceiverAmount
                         tiReceiverAssetClass

    sendA :: UtxoEscrowInfo -> Integer
    sendA = snd . escrowPayment
