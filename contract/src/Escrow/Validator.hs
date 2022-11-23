{-# LANGUAGE TemplateHaskell       #-}

{-|
Module      : Escrow.Validator
Description : Definition of contract script.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define the boilerplate for compiling the validator.
-}

module Escrow.Validator
    ( Escrowing
    -- * Escrow Validator
    , escrowInst
    , escrowValidator
    , escrowAddress
    -- * Escrow Minting Policy
    , controlTokenMP
    , controlTokenCurrency
    )
where

-- IOG imports
import Ledger ( CurrencySymbol, MintingPolicy
              , scriptAddress, scriptCurrencySymbol
              , mkMintingPolicyScript
              )
import Ledger.Typed.Scripts ( mkTypedValidator, TypedValidator, Validator
                            , ValidatorTypes (DatumType, RedeemerType)
                            , validatorScript, wrapMintingPolicy, wrapValidator
                            )
import PlutusTx ( compile, applyCode, liftCode )

-- Escrow imports
import Escrow.Business ( ReceiverAddress )
import Escrow.OnChain  ( mkEscrowValidator, mkControlTokenMintingPolicy )
import Escrow.Types    ( EscrowDatum, EscrowRedeemer, ContractAddress )

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
