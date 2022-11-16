{-|
Module      : Escrow.OffChain.Parameters
Description : Parameters for calling OffChain Actions.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define here the parameters that will be used to call the offchain actions.
-}

module Escrow.OffChain.Parameters
    ( -- | Endpoints parameters
      StartParams(..)
    , CancelParams(..)
    , ResolveParams(..)
    , mkStartParams
    , mkCancelParams
    , mkResolveParams
    )
where

-- Non-IOG imports
import Data.Aeson    ( FromJSON, ToJSON )
import GHC.Generics  ( Generic )

-- IOG imports
import Ledger        ( Address, TxOutRef )
import Ledger.Value  ( AssetClass )

-- Escrow imports
import Escrow.Business ( ReceiverAddress )

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
                      { cpTxOutRef :: TxOutRef
                      , cpReceiverAddress :: Address
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

mkCancelParams :: TxOutRef -> Address -> CancelParams
mkCancelParams ref rAddr =
    CancelParams { cpTxOutRef = ref
                 , cpReceiverAddress = rAddr
                 }

mkResolveParams :: TxOutRef -> ResolveParams
mkResolveParams ref = ResolveParams { rpTxOutRef = ref }
