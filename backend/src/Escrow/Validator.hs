{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}


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
import Ledger ( CurrencySymbol )
import Plutus.Script.Utils.V1.Typed.Scripts
import Plutus.Script.Utils.V1.Scripts
import Plutus.Script.Utils.V1.Address
import Plutus.V1.Ledger.Scripts
import PlutusTx ( compile, applyCode, liftCode )

-- Escrow imports
import Escrow.OnChain  ( mkEscrowValidator, mkControlTokenMintingPolicy )
import Escrow.Types    ( EscrowDatum, EscrowRedeemer
                       , ContractAddress
                       , ReceiverAddress
                       )

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
    wrap = mkUntypedValidator @EscrowDatum @EscrowRedeemer

escrowValidator :: ReceiverAddress -> Validator
escrowValidator = validatorScript . escrowInst

escrowAddress :: ReceiverAddress -> ContractAddress
escrowAddress = mkValidatorAddress . escrowValidator

controlTokenMP :: ContractAddress -> MintingPolicy
controlTokenMP caddr =
    mkMintingPolicyScript $
    $$(compile [|| wrap . mkControlTokenMintingPolicy ||])
    `applyCode`
    liftCode caddr
  where
    wrap = mkUntypedMintingPolicy

controlTokenCurrency :: ContractAddress -> CurrencySymbol
controlTokenCurrency = scriptCurrencySymbol . controlTokenMP
