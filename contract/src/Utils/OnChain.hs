{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Utils.OnChain
Description : Utils functions for OnChain code.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.OnChain where

-- IOG imports
import Ledger
import Ledger.Credential (Credential(..))
import PlutusTx.Prelude

-- | Get the values paid to a public key address by a pending transaction.
{-# INLINABLE outputsAt #-}
outputsAt :: Address -> TxInfo -> [TxOut]
outputsAt addr p =
    let flt txOut@TxOut{..} | txOutAddress == addr = Just txOut
        flt _                                      = Nothing
    in mapMaybe flt (txInfoOutputs p)

-- | Get the values paid to a public key address by a pending transaction.
{-# INLINABLE valueOutputsAt #-}
valueOutputsAt :: Address -> TxInfo -> [Value]
valueOutputsAt addr p =
    let flt TxOut{..} | txOutAddress == addr = Just txOutValue
        flt _                                = Nothing
    in mapMaybe flt (txInfoOutputs p)

-- | Get the total value paid to a public key address by a pending transaction.
{-# INLINABLE valuePaidTo #-}
valuePaidTo :: TxInfo -> Address -> Value
valuePaidTo ptx addr = mconcat (valueOutputsAt addr ptx)

-- | The public key hash is part of the address.
{-# INLINABLE pubKeyHashInAddress #-}
pubKeyHashInAddress :: PubKeyHash -> Address -> Bool
pubKeyHashInAddress pkh addr =
    case addressCredential addr of
        PubKeyCredential spkh -> spkh == pkh
        _ -> False

-- | Get the only element on the list, fail otherwise.
{-# INLINABLE getSingleton #-}
getSingleton :: [a] -> a
getSingleton [a] = a
getSingleton []  = traceError "Empty list"
getSingleton _   = traceError "More than one element"
