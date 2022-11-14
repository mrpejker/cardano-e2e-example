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

module Escrow.Business where

-- Non-IOG imports
import Data.Aeson       ( FromJSON, ToJSON )
import Prelude          ( Show )

-- IOG imports
import Ledger           ( Address, AssetClass )
import PlutusTx         ( makeIsDataIndexed, makeLift )
import PlutusTx.Prelude ( Integer, (.) )

newtype SenderAddress   = SenderAddress { sAddr :: Address }
  deriving newtype (Show, FromJSON, ToJSON)
newtype ReceiverAddress = ReceiverAddress { rAddr :: Address }
  deriving newtype (Show, FromJSON, ToJSON)

mkSenderAddress :: Address -> SenderAddress
mkSenderAddress addr = SenderAddress { sAddr = addr }

mkReceiverAddress :: Address -> ReceiverAddress
mkReceiverAddress addr = ReceiverAddress { rAddr = addr }

{- | EscrowInfo

Stores most of the information of the escrow:
 - Sender’s address.
 - The amount and asset class of the receiver’s payment.
-}
data EscrowInfo = EscrowInfo { sender      :: SenderAddress
                             , rAmount     :: Integer
                             , rAssetClass :: AssetClass
                             }
    deriving Show

mkEscrowInfo :: SenderAddress -> Integer -> AssetClass -> EscrowInfo
mkEscrowInfo sAdd amount assetClass = EscrowInfo { sender = sAdd
                                                 , rAmount = amount
                                                 , rAssetClass = assetClass
                                                 }

eInfoSenderAddr :: EscrowInfo -> Address
eInfoSenderAddr = sAddr . sender

PlutusTx.makeLift ''ReceiverAddress
PlutusTx.makeIsDataIndexed ''EscrowInfo    [ ('EscrowInfo, 0) ]
PlutusTx.makeIsDataIndexed ''SenderAddress [ ('SenderAddress, 0) ]
