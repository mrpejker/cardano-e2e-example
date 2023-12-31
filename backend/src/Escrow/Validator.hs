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
import Ledger ( CurrencySymbol )
import Plutus.Script.Utils.V1.Typed.Scripts ( TypedValidator
                                            , ValidatorTypes
                                            , DatumType, RedeemerType
                                            , mkTypedValidator
                                            , mkUntypedValidator
                                            , mkUntypedMintingPolicy
                                            , validatorScript
                                            )
import Plutus.Script.Utils.V1.Scripts ( scriptCurrencySymbol )
import Plutus.Script.Utils.V1.Address ( mkValidatorAddress )
import Plutus.V1.Ledger.Scripts       ( Validator, MintingPolicy
                                      , mkMintingPolicyScript
                                      )
import PlutusTx ( compile, applyCode, liftCode )

-- Escrow imports
import Escrow.OnChain  ( mkEscrowValidator, mkControlTokenMintingPolicy )
import Escrow.Types    ( EscrowDatum, EscrowRedeemer
                       , ScriptAddress
                       , ReceiverAddress
                       )

-- | Definition of type family describing which types are used
--   as datum and redeemers.
data Escrowing
instance ValidatorTypes Escrowing where
    type instance DatumType    Escrowing = EscrowDatum
    type instance RedeemerType Escrowing = EscrowRedeemer

escrowInst :: ReceiverAddress -> TypedValidator Escrowing
escrowInst raddr =
    mkTypedValidator @Escrowing
    ($$(compile [|| mkEscrowValidator ||])
        `applyCode`
        liftCode raddr
    )
    $$(compile [|| mkUntypedValidator @EscrowDatum @EscrowRedeemer ||])

escrowValidator :: ReceiverAddress -> Validator
escrowValidator = validatorScript . escrowInst

escrowAddress :: ReceiverAddress -> ScriptAddress
escrowAddress = mkValidatorAddress . escrowValidator

controlTokenMP :: ScriptAddress -> MintingPolicy
controlTokenMP caddr =
    mkMintingPolicyScript $
    $$(compile [|| mkUntypedMintingPolicy . mkControlTokenMintingPolicy ||])
    `applyCode`
    liftCode caddr

controlTokenCurrency :: ScriptAddress -> CurrencySymbol
controlTokenCurrency = scriptCurrencySymbol . controlTokenMP
