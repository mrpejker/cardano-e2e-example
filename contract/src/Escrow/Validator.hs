{-# LANGUAGE TemplateHaskell       #-}

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
              , mkMintingPolicyScript
              )
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx qualified

import Escrow.Business
import Escrow.OnChain
import Escrow.Types

-- | Definition of type family describing which types are used
--   as datum and redeemers.
data Escrowing
instance Scripts.ValidatorTypes Escrowing where
    type instance DatumType    Escrowing = EscrowDatum
    type instance RedeemerType Escrowing = EscrowRedeemer

escrowInst :: ReceiverAddress -> Scripts.TypedValidator Escrowing
escrowInst raddr = Scripts.mkTypedValidator @Escrowing
                   ($$(PlutusTx.compile [|| mkEscrowValidator ||])
                       `PlutusTx.applyCode`
                       PlutusTx.liftCode raddr
                   )
                   $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @EscrowDatum @EscrowRedeemer

escrowValidator :: ReceiverAddress -> Scripts.Validator
escrowValidator = Scripts.validatorScript . escrowInst

escrowAddress :: ReceiverAddress -> ContractAddress
escrowAddress = scriptAddress . escrowValidator

controlTokenMP :: ContractAddress -> MintingPolicy
controlTokenMP caddr =
    mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap . mkControlTokenMintingPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode caddr
  where
    wrap = Scripts.wrapMintingPolicy

controlTokenCurrency :: ContractAddress -> CurrencySymbol
controlTokenCurrency = scriptCurrencySymbol . controlTokenMP
