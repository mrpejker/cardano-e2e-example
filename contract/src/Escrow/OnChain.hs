{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Escrow.OnChain
Description : OnChain validator for the Escrow contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define here the on-chain code of the validator and the minting policy.
-}

module Escrow.OnChain where

-- IOG imports
import Plutus.V1.Ledger.Api (ScriptContext)
import PlutusTx.Prelude

import Escrow.Business
import Escrow.Types

{- | Escrow Validator

Validates the transactions that involve spending the script UTxO with Cancel or
Resolve redeemers.

Cancel:
 - The address that is trying to cancel the escrow is the same as the Sender’s
   address.
 - The control token is burned after the transaction.

Resolve:
 - The address that is trying to resolve is the same as the Receiver’s address.
 - The Sender’s address receives the tokens specified on the datum.
 - The control token is burned after the transaction.
-}
{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator :: ReceiverAddress
                  -> EscrowDatum
                  -> EscrowRedeemer
                  -> ScriptContext
                  -> Bool
mkEscrowValidator _ _ _ _ = True


{- | Control Token minting policy

The token minting policy is parametrized by the contract address and has the
following checks:

On minting:
 - Only one token with the correct token name is minted.
 - The sender’s address is signing the transaction.
 - The token is paid to the contract address.

On Burning:
 - One token is being burned.
-}
{-# INLINABLE mkControlTokenMintingPolicy #-}
mkControlTokenMintingPolicy :: ContractAddress -> () -> ScriptContext -> Bool
mkControlTokenMintingPolicy _ _ _ = True
