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
import Ledger               ( CurrencySymbol, MintingPolicy
                            , scriptAddress, scriptCurrencySymbol
                            , mkMintingPolicyScript
                            )
import Ledger.Typed.Scripts ( mkTypedValidator, TypedValidator, Validator
                            , ValidatorTypes (DatumType, RedeemerType)
                            , validatorScript, wrapMintingPolicy, wrapValidator
                            )
import PlutusTx             (compile, applyCode, liftCode)

import Escrow.Business
import Escrow.OnChain
import Escrow.Types

-- | Definition of type family describing which types are used
--   as datum and redeemers.
data Escrowing
instance ValidatorTypes Escrowing where
    type instance DatumType    Escrowing = EscrowDatum
    type instance RedeemerType Escrowing = EscrowRedeemer

escrowInst :: ReceiverAddress -> TypedValidator Escrowing
escrowInst raddr = mkTypedValidator @Escrowing
                   ($$(compile [|| mkEscrowValidator ||])
                       `applyCode`
                       liftCode raddr
                   )
                   $$(compile [|| wrap ||])
  where
    wrap = wrapValidator @EscrowDatum @EscrowRedeemer

escrowValidator :: ReceiverAddress -> Validator
escrowValidator = validatorScript . escrowInst

escrowAddress :: ReceiverAddress -> ContractAddress
escrowAddress = scriptAddress . escrowValidator

controlTokenMP :: ContractAddress -> MintingPolicy
controlTokenMP caddr =
    mkMintingPolicyScript $
    $$(compile [|| wrap . mkControlTokenMintingPolicy ||])
    `applyCode`
    liftCode caddr
  where
    wrap = wrapMintingPolicy

controlTokenCurrency :: ContractAddress -> CurrencySymbol
controlTokenCurrency = scriptCurrencySymbol . controlTokenMP
