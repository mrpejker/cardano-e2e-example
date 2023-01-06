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
    , signerIsSender
    , signerIsReceiver
    -- * Getters
    , eInfoSenderAddr
    , eInfoSenderWallAddr
    , valueToSender
    )
where

-- Non-IOG imports
import Data.Aeson              ( FromJSON, ToJSON )
import Prelude qualified as HP ( Eq, Show )
import GHC.Generics            ( Generic )

-- IOG imports
import Ledger           ( Address(..), AssetClass, PubKeyHash )
import PlutusTx         ( makeIsDataIndexed, makeLift )
import PlutusTx.Prelude ( Integer, (.), Eq, Bool )
import Plutus.V1.Ledger.Value ( Value, assetClassValue )

-- Escrow imports
import Utils.OnChain ( pubKeyHashInAddress )
import Utils.WalletAddress ( WalletAddress, fromWalletAddress )

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
    deriving (HP.Eq, HP.Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor of a EscrowInfo.
mkEscrowInfo :: SenderAddress -> Integer -> AssetClass -> EscrowInfo
mkEscrowInfo sAdd amount assetClass =
    EscrowInfo { sender      = sAdd
               , rAmount     = amount
               , rAssetClass = assetClass
               }

{- | A SenderAddress is just a wrapper over Address, used for not confusing
     concerns.
-}
newtype SenderAddress = SenderAddress { sAddr :: WalletAddress }
    deriving newtype (HP.Eq, HP.Show, Eq, FromJSON, ToJSON)

-- | Smart constructor of a SenderAddress.
mkSenderAddress :: WalletAddress -> SenderAddress
mkSenderAddress addr = SenderAddress { sAddr = addr }

{- | A ReceiverAddress is just a wrapper over Address, used for not confusing
     concerns.
-}
newtype ReceiverAddress = ReceiverAddress { rAddr :: WalletAddress }
    deriving newtype (HP.Show, Eq, FromJSON, ToJSON)

-- | Smart constructor of a ReceiverAddress.
mkReceiverAddress :: WalletAddress -> ReceiverAddress
mkReceiverAddress addr = ReceiverAddress { rAddr = addr }

-- | Gets the sender address from the EscrowInfo.
{-# INLINABLE eInfoSenderAddr #-}
eInfoSenderAddr :: EscrowInfo -> Address
eInfoSenderAddr = fromWalletAddress . sAddr . sender

-- | Gets the sender WalletAddress from the EscrowInfo.
{-# INLINABLE eInfoSenderWallAddr #-}
eInfoSenderWallAddr :: EscrowInfo -> WalletAddress
eInfoSenderWallAddr = sAddr . sender

-- | Given a escrow information builds the value to be paid to the sender.
{-# INLINABLE valueToSender #-}
valueToSender :: EscrowInfo -> Value
valueToSender EscrowInfo{..} = assetClassValue rAssetClass rAmount

-- | Checks is the given pubkeyhash is part of the SenderAddress.
{-# INLINABLE signerIsSender #-}
signerIsSender :: PubKeyHash -> SenderAddress -> Bool
signerIsSender pkh SenderAddress{..} =
    pubKeyHashInAddress pkh (fromWalletAddress sAddr)

-- | Checks is the given pubkeyhash is part of the ReceiverAddress.
{-# INLINABLE signerIsReceiver #-}
signerIsReceiver :: PubKeyHash -> ReceiverAddress -> Bool
signerIsReceiver pkh ReceiverAddress{..} =
    pubKeyHashInAddress pkh (fromWalletAddress rAddr)

-- | Boilerplate for deriving the FromData and ToData instances.
makeLift ''ReceiverAddress
makeIsDataIndexed ''EscrowInfo    [ ('EscrowInfo, 0) ]
makeIsDataIndexed ''SenderAddress [ ('SenderAddress, 0) ]
makeIsDataIndexed ''ReceiverAddress [ ('ReceiverAddress, 0) ]
