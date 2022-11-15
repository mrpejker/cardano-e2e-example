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
    ( -- | Types
      ContractAddress
    , EscrowDatum (..)
    , EscrowRedeemer (..)
    -- | Smart Constructors
    , mkEscrowDatum
    , cancelRedeemer
    , resolveRedeemer
    , cTokenName
    )
where

-- IOG imports
import Ledger   ( Address, AssetClass, TokenName, Redeemer(..) )
import PlutusTx ( toBuiltinData, makeIsDataIndexed )

-- Escrow imports
import Escrow.Business ( EscrowInfo, SenderAddress, mkEscrowInfo )

type ContractAddress = Address

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

data EscrowRedeemer = CancelEscrow
                    | ResolveEscrow

cancelRedeemer :: Redeemer
cancelRedeemer = Redeemer $ toBuiltinData CancelEscrow

resolveRedeemer :: Redeemer
resolveRedeemer = Redeemer $ toBuiltinData ResolveEscrow

cTokenName :: TokenName
cTokenName = "controlToken"

makeIsDataIndexed ''EscrowDatum    [ ('EscrowDatum, 0) ]
makeIsDataIndexed ''EscrowRedeemer [ ('CancelEscrow, 0)
                                   , ('ResolveEscrow, 1)
                                   ]
