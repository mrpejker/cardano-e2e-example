{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-|
Module      : Escrow.Business
Description : Business logic for escrow contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define in this module any business logic of the contract. This is
the core part in which any modification of the contract state is defined.
This module will be shared between offchain and onchain code, so it must use
the plutus prelude, instead of the haskell prelude.
-}

module Escrow.Business
    ( -- * Types
      SenderAddress
    , ReceiverAddress
    , EscrowInfo (..)
    -- * Smart Constructors
    , mkSenderAddress
    , mkReceiverAddress
    , mkEscrowInfo
    -- * Checks
    , singerIsSender
    , singerIsReceiver
    -- * Getters
    , eInfoSenderAddr
    )
where

-- Non-IOG imports
import Data.Aeson       ( FromJSON, ToJSON )
import Prelude          ( Eq, Show )
import GHC.Generics     ( Generic )

-- IOG imports
import Ledger           ( Address(..), AssetClass, PubKeyHash )
import PlutusTx         ( makeIsDataIndexed, makeLift )
import PlutusTx.Prelude ( Integer, (.), Bool )

-- Escrow imports
import Utils.OnChain ( pubKeyHashInAddress )

{- | A SenderAddress is just a wrapper over Address, used for not confusing
     concerns.
-}
newtype SenderAddress   = SenderAddress { sAddr :: Address }
  deriving newtype (Eq, Show, FromJSON, ToJSON)
{- | A ReceiverAddress is just a wrapper over Address, used for not confusing
     concerns.
-}
newtype ReceiverAddress = ReceiverAddress { rAddr :: Address }
  deriving newtype (Show, FromJSON, ToJSON)

-- | Smart constructor of a SenderAddress.
mkSenderAddress :: Address -> SenderAddress
mkSenderAddress addr = SenderAddress { sAddr = addr }

-- | Smart constructor of a ReceiverAddress.
mkReceiverAddress :: Address -> ReceiverAddress
mkReceiverAddress addr = ReceiverAddress { rAddr = addr }

-- | Checks is the given pubkeyhash is part of the SenderAddress.
{-# INLINABLE singerIsSender #-}
singerIsSender :: PubKeyHash -> SenderAddress -> Bool
singerIsSender pkh SenderAddress{..} = pubKeyHashInAddress pkh sAddr

-- | Checks is the given pubkeyhash is part of the ReceiverAddress.
{-# INLINABLE singerIsReceiver #-}
singerIsReceiver :: PubKeyHash -> ReceiverAddress -> Bool
singerIsReceiver pkh ReceiverAddress{..} = pubKeyHashInAddress pkh rAddr

{- | EscrowInfo

Stores most of the information of the escrow:
 - Sender’s address.
 - The amount and asset class of the receiver’s payment.
-}
data EscrowInfo = EscrowInfo
                  { sender      :: SenderAddress
                  , rAmount     :: Integer
                  , rAssetClass :: AssetClass
                  }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor of a EscrowInfo.
mkEscrowInfo :: SenderAddress -> Integer -> AssetClass -> EscrowInfo
mkEscrowInfo sAdd amount assetClass =
    EscrowInfo { sender      = sAdd
               , rAmount     = amount
               , rAssetClass = assetClass
               }

-- | Gets the sender address from the EscrowInfo.
{-# INLINABLE eInfoSenderAddr #-}
eInfoSenderAddr :: EscrowInfo -> Address
eInfoSenderAddr = sAddr . sender

-- Boilerplate for deriving the FromData and ToData instances.
makeLift ''ReceiverAddress
makeIsDataIndexed ''EscrowInfo    [ ('EscrowInfo, 0) ]
makeIsDataIndexed ''SenderAddress [ ('SenderAddress, 0) ]
makeIsDataIndexed ''ReceiverAddress [ ('ReceiverAddress, 0) ]
