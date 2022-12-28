{-|
Module      : Escrow.OffChain.ObservableState
Description : Observable state for Escrow Contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

-}

module Escrow.OffChain.ObservableState
    (-- * Observable state information
      UtxoEscrowInfo(..)
    -- * Smart Constructors
    , mkUtxoEscrowInfo
    )
where

-- Non-IOG imports
import Data.Aeson   ( FromJSON, ToJSON )
import GHC.Generics ( Generic )

-- IOG imports
import Ledger ( TxOutRef, Value )

-- Escrow imports
import Escrow.Types ( EscrowInfo(..) )

{- | Enclose the complete information about a particular escrow instance:
     - The utxo reference for resolving or canceling.
     - The escrow info: Sender's address and receiver's payment info.
     - The total value locked: The Sender's payment.
-}
data UtxoEscrowInfo = UtxoEscrowInfo
                      { escrowUtxo  :: TxOutRef
                      , escrowInfo  :: EscrowInfo
                      , escrowValue :: Value
                      }
    deriving (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor of a UtxoscrowInfo.
mkUtxoEscrowInfo :: TxOutRef -> Value -> EscrowInfo -> UtxoEscrowInfo
mkUtxoEscrowInfo utxoRef v ei = UtxoEscrowInfo
                                { escrowUtxo  = utxoRef
                                , escrowInfo  = ei
                                , escrowValue = v
                                }
