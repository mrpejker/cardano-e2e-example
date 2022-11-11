{-# LANGUAGE TemplateHaskell       #-}
{-|
Module      : Escrow.Types
Description : data types for Escrow Contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define the main types used in the contract.
It consists of a Parameter, Escrow Datum and Escrow Redeemer.
-}

module Escrow.Types where

-- IOG imports
import Ledger (Address, AssetClass, TokenName, minAdaTxOut, Redeemer(..), Value)
import Ledger.Ada qualified as Ada
import PlutusTx qualified

import Escrow.Business

type ContractAddress = Address

newtype Parameter = Parameter { rAddress :: ReceiverAddress }

data EscrowDatum = EscrowDatum
                   { eInfo       :: EscrowInfo
                   , eAssetClass :: AssetClass
                   }
    deriving Show

mkEscrowDatum :: SenderAddress -> Integer -> AssetClass -> AssetClass -> EscrowDatum
mkEscrowDatum sAdd amount asset cAsset = EscrowDatum
                                  { eInfo = mkEscrowInfo sAdd amount asset
                                  , eAssetClass = cAsset
                                  }

data EscrowRedeemer = CancelEscrow
                    | ResolveEscrow

cancelRedeemer :: Redeemer
cancelRedeemer = Redeemer $ PlutusTx.toBuiltinData CancelEscrow

resolveRedeemer :: Redeemer
resolveRedeemer = Redeemer $ PlutusTx.toBuiltinData ResolveEscrow

-- | Minimum amount of ADAs that every UTxO must have
{-# INLINABLE minAda #-}
minAda :: Value
minAda = Ada.toValue Ledger.minAdaTxOut

cTokenName :: TokenName
cTokenName = "controlToken"

PlutusTx.makeIsDataIndexed ''EscrowDatum    [ ('EscrowDatum, 0) ]
PlutusTx.makeIsDataIndexed ''EscrowRedeemer [ ('CancelEscrow, 0)
                                            , ('ResolveEscrow, 1)
                                            ]
