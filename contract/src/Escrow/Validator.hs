
{-|
Module      : Escrow.Validator
Description : Definition of contract script.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define the boilerplate for compiling the validator.
-}

-- Uncomment this line for validator profiling:
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Escrow.Validator where

-- IOG imports
import Ledger ( CurrencySymbol, MintingPolicy
              , scriptAddress, scriptCurrencySymbol
              )
import Ledger.Typed.Scripts qualified as Scripts

import Escrow.Business
import Escrow.Types

-- | Definition of type family describing which types are used
--   as datum and redeemers.
data Escrowing
instance Scripts.ValidatorTypes Escrowing where
    type instance DatumType    Escrowing = EscrowDatum
    type instance RedeemerType Escrowing = EscrowRedeemer

escrowInst :: ReceiverAddress -> Scripts.TypedValidator Escrowing
escrowInst _ = undefined

escrowValidator :: ReceiverAddress -> Scripts.Validator
escrowValidator = Scripts.validatorScript . escrowInst

escrowAddress :: ReceiverAddress -> ContractAddress
escrowAddress = scriptAddress . escrowValidator

controlTokenMP :: ContractAddress -> MintingPolicy
controlTokenMP _ = undefined

controlTokenCurrency :: ContractAddress -> CurrencySymbol
controlTokenCurrency = scriptCurrencySymbol . controlTokenMP
