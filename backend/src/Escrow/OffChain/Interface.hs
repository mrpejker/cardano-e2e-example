{-|
Module      : Escrow.OffChain.Interface
Description : The interface of the offchain escrow operations.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

Here we define the API of the offchain implementation.
-}

module Escrow.OffChain.Interface
    ( -- * API/Schema
      EscrowSchema
    -- * Endpoints parameters
    , StartParams(..)
    , CancelParams(..)
    , ResolveParams(..)
    -- * Observable state information
    , UtxoEscrowInfo(..)
    -- * Smart constructors
    , mkStartParams
    , mkCancelParams
    , mkResolveParams
    , mkUtxoEscrowInfo
    )
where

-- Non-IOG imports
import Data.Aeson   ( FromJSON, ToJSON )
import GHC.Generics ( Generic )

-- IOG imports
import Ledger          ( TxOutRef )
import Ledger.Value    ( AssetClass )
import Plutus.Contract ( Endpoint, type (.\/) )

-- Escrow imports
import Escrow.Types ( EscrowInfo, ReceiverAddress )

-- | Escrow Schema
type EscrowSchema = Endpoint "start"   StartParams
                .\/ Endpoint "cancel"  CancelParams
                .\/ Endpoint "resolve" ResolveParams
                .\/ Endpoint "reload"  ()

{-| The start parameter includes the Address of the receiver, that
    will be used to get the contract address and validator.
    It also contains the amount and asset class the sender wants
    to offer that will be locked in the value of the Utxo.
    Finally the amount and asset class the sender wants to receive.
    This last two parameters will be stored in the datum.
-}
data StartParams = StartParams
                   { receiverAddress   :: ReceiverAddress
                   , sendAmount        :: Integer
                   , sendAssetClass    :: AssetClass
                   , receiveAmount     :: Integer
                   , receiveAssetClass :: AssetClass
                   }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

{-| The cancel parameter includes the reference of the script Utxo
    to find the complete Utxo with it's datum. It also contains the
    receiver Address to get the contract address and validator.
-}
data CancelParams = CancelParams
                    { cpTxOutRef        :: TxOutRef
                    , cpReceiverAddress :: ReceiverAddress
                    }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

{-| The resolve parameter includes the reference of the script Utxo
    to find the complete Utxo with it's datum. The address of the user calling
    the action is used as the Receiver Address.
-}
newtype ResolveParams = ResolveParams { rpTxOutRef :: TxOutRef }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for the start param.
mkStartParams
    :: ReceiverAddress
    -> Integer
    -> AssetClass
    -> Integer
    -> AssetClass
    -> StartParams
mkStartParams rAdd sAmount sAC rAmount rAC =
    StartParams { receiverAddress   = rAdd
                , sendAmount        = sAmount
                , sendAssetClass    = sAC
                , receiveAmount     = rAmount
                , receiveAssetClass = rAC
                }

-- | Smart constructor for the cancel param.
mkCancelParams :: TxOutRef -> ReceiverAddress -> CancelParams
mkCancelParams ref rAddr = CancelParams
                           { cpTxOutRef        = ref
                           , cpReceiverAddress = rAddr
                           }

-- | Smart constructor for the resolve param.
mkResolveParams :: TxOutRef -> ResolveParams
mkResolveParams ref = ResolveParams { rpTxOutRef = ref }

{- | Enclose the complete information about a particular escrow instance:
     - The utxo reference for resolving or canceling.
     - The escrow info: Sender's address and receiver's payment info.
     - The total value locked: The Sender's payment.
-}
data UtxoEscrowInfo = UtxoEscrowInfo
                      { escrowUtxo    :: TxOutRef
                      , escrowInfo    :: EscrowInfo
                      , escrowPayment :: (AssetClass,Integer)
                      }
    deriving (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor of a UtxoscrowInfo.
mkUtxoEscrowInfo
    :: TxOutRef
    -> (AssetClass, Integer)
    -> EscrowInfo
    -> UtxoEscrowInfo
mkUtxoEscrowInfo utxoRef pay ei = UtxoEscrowInfo
                                  { escrowUtxo    = utxoRef
                                  , escrowInfo    = ei
                                  , escrowPayment = pay
                                  }
