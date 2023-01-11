{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Escrow.OnChain
Description : OnChain validator for the Escrow contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define here the on-chain code of the validator and the minting policy.
-}

module Escrow.OnChain
    ( -- * Validators
      mkEscrowValidator
    , mkControlTokenMintingPolicy
    )
where

-- IOG imports
import Plutus.V1.Ledger.Api   ( ScriptContext(..), TxInfo(..), TxOut(..)
                              , PubKeyHash, TokenName
                              )
import Plutus.V1.Ledger.Value ( CurrencySymbol, Value
                              , assetClass, assetClassValueOf, flattenValue
                              , leq
                              )
import PlutusTx.Prelude       ( Integer, Bool
                              , ($), (&&), (||), (==), (>), (<>)
                              , traceIfFalse
                              )

-- Escrow imports
import Escrow.Business ( ReceiverAddress, EscrowInfo(..)
                       , signerIsSender, signerIsReceiver, eInfoSenderAddr
                       , valueToSender
                       )
import Escrow.Types    ( EscrowDatum(..), EscrowRedeemer(..), ScriptAddress )
import Utils.OnChain   ( fromJust, getSingleton
                       , valuePaidTo, outputsAt, getTxOutDatum
                       )

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
mkEscrowValidator raddr EscrowDatum{..} r ctx =
    case r of
        CancelEscrow  -> cancelValidator eInfo signer
        ResolveEscrow -> resolveValidator info eInfo raddr signer
    &&
    traceIfFalse "controlToken was not burned"
                 (eAssetClass == assetClass mintedCS mintedTN && mintedA == -1)
  where
    mintedCS :: CurrencySymbol
    mintedTN :: TokenName
    (mintedCS, mintedTN, mintedA) = getSingleton $
                                    flattenValue $ txInfoMint info

    signer :: PubKeyHash
    signer = getSingleton $ txInfoSignatories info

    info :: TxInfo
    info = scriptContextTxInfo ctx


{- | Checks:
 - The address that is trying to cancel the escrow is the same as the Sender’s
   address.
-}
{-# INLINABLE cancelValidator #-}
cancelValidator :: EscrowInfo -> PubKeyHash -> Bool
cancelValidator EscrowInfo{..} signer =
    traceIfFalse
    "cancelValidator: Wrong sender signature" $ signerIsSender signer sender

{- | Checks:
 - The address that is trying to resolve is the same as the Receiver’s address.
 - The Sender’s address receives the tokens specified on the datum.
-}
{-# INLINABLE resolveValidator #-}
resolveValidator
    :: TxInfo
    -> EscrowInfo
    -> ReceiverAddress
    -> PubKeyHash
    -> Bool
resolveValidator info ei raddr signer =
    traceIfFalse "resolveValidator: Wrong receiver signature"
                 (signerIsReceiver signer raddr)
    &&
    traceIfFalse "resolveValidator: Wrong sender's payment"
                 (valueToSender ei `leq` senderV)
  where
    senderV :: Value
    senderV = valuePaidTo (eInfoSenderAddr ei) info

{- | Escrow Control Token minting policy

The token minting policy is parametrized by the contract address and has the
following checks:

On minting:
- Only one token with the correct token name is minted
- The token is paid to the contract address
- The sender’s address is signing the transaction
- The token being minted is the correct control token
- The amount of tokens that the receiver wants to offer is more than 0

On Burning:
 - One token is being burned.
-}
{-# INLINABLE mkControlTokenMintingPolicy #-}
mkControlTokenMintingPolicy :: ScriptAddress -> () -> ScriptContext -> Bool
mkControlTokenMintingPolicy addr _ ctx =
    traceIfFalse "Burning less or more than one token" (mintedA == -1)
    ||
    (   traceIfFalse "Minting more than one token"
                     (mintedA == 1)
     && traceIfFalse "The control token was not paid to the script address"
                     controlTokenPaid
     && traceIfFalse "Wrong information in Datum"
                     correctDatum
    )
  where
    mintedCS :: CurrencySymbol
    mintedTN :: TokenName
    mintedA :: Integer
    (mintedCS, mintedTN, mintedA) = getSingleton $
                                    flattenValue $ txInfoMint info

    controlTokenPaid :: Bool
    controlTokenPaid =
        assetClassValueOf (txOutValue escrowUtxo) (assetClass mintedCS mintedTN)
        ==
        mintedA

    correctDatum :: Bool
    correctDatum =
        traceIfFalse
          "Sender address in Datum doesn't coincide with transaction signer"
          correctSigner
     && traceIfFalse
          ("Control token Asset Class in Datum doesn't coincide with the" <>
          "minted token")
          correctControlAssetClass
     && traceIfFalse
          "Amount of tokens to pay to Receiver in Datum is less than 0"
          correctAmount

    correctSigner :: Bool
    correctSigner = signerIsSender signer (sender $ eInfo escrowDatum)

    correctControlAssetClass :: Bool
    correctControlAssetClass =
      eAssetClass escrowDatum == assetClass mintedCS mintedTN

    correctAmount :: Bool
    correctAmount = rAmount (eInfo escrowDatum) > 0

    escrowUtxo :: TxOut
    escrowUtxo = getSingleton $ outputsAt addr info

    escrowDatum :: EscrowDatum
    escrowDatum = fromJust $ getTxOutDatum escrowUtxo info

    signer :: PubKeyHash
    signer = getSingleton $ txInfoSignatories info

    info :: TxInfo
    info = scriptContextTxInfo ctx
