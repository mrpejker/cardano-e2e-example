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

module Escrow.Types
    ( -- * Types
      ScriptAddress
    , EscrowDatum (..)
    , EscrowRedeemer (..)
    -- * Business Types
    , EscrowInfo(..)
    , ReceiverAddress
    , SenderAddress
    -- * Smart Constructors
    , mkEscrowDatum
    , cancelRedeemer
    , resolveRedeemer
    , cTokenName
    -- * Business Smart Constructors
    , mkEscrowInfo
    )
where

-- IOG imports
import Ledger   ( Address, AssetClass, TokenName, Redeemer(..) )
import PlutusTx ( toBuiltinData, makeIsDataIndexed )

-- Escrow imports
import Escrow.Business ( EscrowInfo(..), ReceiverAddress, SenderAddress
                       , mkEscrowInfo
                       )

{- | A type synonym for explictly differentiate a Wallet Address from a
     ScriptAddress
-}
type ScriptAddress = Address

{- | The EscrowDatum type contains the escrow information about the amount and
     kind of tokens the receiver should send and to which address.
     It also contains the assetclass of the control token for checking the
     correct burning of the token when the escrow is finalized.
-}
data EscrowDatum = EscrowDatum
                   { eInfo       :: EscrowInfo
                   , eAssetClass :: AssetClass
                   }
    deriving Show

-- | Smart constructor for a EscrowDatum.
mkEscrowDatum
    :: SenderAddress
    -> Integer
    -> AssetClass
    -> AssetClass
    -> EscrowDatum
mkEscrowDatum sAdd amount asset cAsset =
    EscrowDatum
    { eInfo = mkEscrowInfo sAdd amount asset
    , eAssetClass = cAsset
    }

-- | Spending the script UTxO can be done with the Cancel or Resolve redeemer.
data EscrowRedeemer = CancelEscrow
                    | ResolveEscrow

-- | Untyped Cancel Redeemer.
cancelRedeemer :: Redeemer
cancelRedeemer = Redeemer $ toBuiltinData CancelEscrow

-- | Untyped Resolve Redeemer.
resolveRedeemer :: Redeemer
resolveRedeemer = Redeemer $ toBuiltinData ResolveEscrow

-- | Control token name.
cTokenName :: TokenName
cTokenName = "controlToken"

-- Boilerplate for deriving the FromData and ToData instances.
makeIsDataIndexed ''EscrowDatum    [ ('EscrowDatum, 0) ]
makeIsDataIndexed ''EscrowRedeemer [ ('CancelEscrow, 0)
                                   , ('ResolveEscrow, 1)
                                   ]
