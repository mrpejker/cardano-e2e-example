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
import Data.Aeson (FromJSON, ToJSON)

-- IOG imports
import Ledger           (Address, AssetClass)
import PlutusTx qualified
import PlutusTx.Prelude

newtype SenderAddress   = SenderAddress { sAddr :: Address }
  deriving newtype (FromJSON, ToJSON)
newtype ReceiverAddress = ReceiverAddress { rAddr :: Address }
  deriving newtype (FromJSON, ToJSON)

{- | EscrowInfo

Stores most of the information of the escrow:
 - Sender’s address.
 - The amount and asset class of the receiver’s payment.
-}
data EscrowInfo = EscrowInfo { sender      :: SenderAddress
                             , rAmount     :: Integer
                             , rAssetClass :: AssetClass
                             }

mkEscrowInfo :: SenderAddress -> Integer -> AssetClass -> EscrowInfo
mkEscrowInfo sAdd amount assetClass = EscrowInfo { sender = sAdd
                                                 , rAmount = amount
                                                 , rAssetClass = assetClass
                                                 }

PlutusTx.makeIsDataIndexed ''EscrowInfo    [ ('EscrowInfo, 0) ]
PlutusTx.makeIsDataIndexed ''SenderAddress [ ('SenderAddress, 0) ]