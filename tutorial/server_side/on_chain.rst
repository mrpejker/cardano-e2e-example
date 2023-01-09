On-Chain
========

Each time a user starts an escrow, a new script utxo is created, locking the tokens
to be paid and containing in the datum all the information needed for successfully
completing the exchange. Also, when a start happens, we mint a control token that
will be locked in this script utxo, and will be in charge of validating that it is
*well-formed*. Thus, every time we want to cancel or resolve an escrow, by finding
the corresponding *control token* on the script utxo we are sure the information
on it is correct.

The script attached to this utxo, in charge of validating the correct spending,
and the policy for minting the *control token*, are implemented on the :code:`OnChain`
and :code:`Validator` modules. As we mentioned, the on-chain validator and minting
policy are implemented on Haskell and then compiled to Plutus, which is the actual
code on the utxo.

In the :code:`OnChain` module, we will find the ``mkEscrowValidator`` and
``mkControlTokenMintingPolicy`` functions that are the on-chain validator and
the minting policy implementations, amoung other helper functions. Because we need
to compile these functions to Plutus, we must use the *Plutus Prelude* instead of
the *Haskell Prelude*. To avoid confusion about which Prelude we are using, we add
the ``NoImplicitPrelude`` pragma at the top of the module. As a result we don't
have any of the common Haskell types and functions in this scope. Thus we must
explictly import from the Plutus Prelude all we need:

.. code:: haskell

   import PlutusTx.Prelude ( Integer, Bool
                           , ($), (&&), (||), (==)
                           , traceIfFalse
                           )

Minting Policy
--------------

Starting an escrow involves minting a *control token*, thus running a validation
implemented on its minting policy that allows the minting or burning. We use this
fact to ensure some good starting properties on the script utxo we are creating.

The minting policy is implemented by ``mkControlTokenMintingPolicy``. The policy
is parameterized on the script address of the escrow utxo we are creating, besides
that the policy must take a redeemer (on this particular case ``()``) and a
script context. In the script context, we will find all transaction information.

In general, the most interesting information about the transaction is placed here

.. code:: haskell

   info :: TxInfo
   info = scriptContextTxInfo ctx

and it helps us to get the signer ``PubKeyHash`` of the transaction. In particular,
we expect only one signer and that is the reason we use ``getFromSingleton``, that
will return the only element of a list or fail

.. code:: haskell

   signer :: PubKeyHash
   signer = getFromSingleton $ txInfoSignatories info

Also from there, we get the complete list of tokens that are minted/burned, and
again because we only want a single token we use ``getFromSingleton``

.. code:: haskell

   mintedCS :: CurrencySymbol
   mintedTN :: TokenName
   mintedA :: Integer
   (mintedCS, mintedTN, mintedA) = getFromSingleton $
	                           flattenValue $ txInfoMint info

Last, we need the script utxo created on the transaction so we can check it locked
the *control token* and the signer corresponds with the escrow information stored
on the datum. Here ``addr`` is the parameter of the minting policy

.. code:: haskell

   escrowUtxo :: TxOut
   escrowUtxo = getFromSingleton $ outputsAt addr info

   escrowDatum :: EscrowDatum
   escrowDatum = fromJust $ getTxOutDatum escrowUtxo info

The minting policy then, will check then that either we are **burning** the *control token*

.. code:: haskell

   traceIfFalse "Burning less or more than one control token" (mintedA == -1)

**or** we are **minting** exactly one. In that case, we also check the token is paid to
the correct address and the datum information is correct

.. code:: haskell

      traceIfFalse "Minting more than one control token" (mintedA == 1)
   && traceIfFalse "The control token was not paid to the script address" controlTokenPaid
   && traceIfFalse "The signer is not the sender on the escrow" correctSigner

From the script utxo we can get the complete locked value ``txOutValue escrowUtxo`` and
check it has exactly the minted token by filtering the value with ``assetClassValueOf``

.. code:: haskell

   controlTokenPaid :: Bool
   controlTokenPaid =
	  assetClassValueOf (txOutValue escrowUtxo) (assetClass mintedCS mintedTN)
          ==
          mintedA

For the last check, we simply use the datum information and the business logic
function ``signerIsSender``

.. code:: haskell

   correctSigner :: Bool
   correctSigner = signerIsSender signer (sender $ eInfo escrowDatum)

We get the complete minting policy by putting everything together

.. code:: haskell

   {-# INLINABLE mkControlTokenMintingPolicy #-}
   mkControlTokenMintingPolicy :: ScriptAddress -> () -> ScriptContext -> Bool
   mkControlTokenMintingPolicy addr _ ctx =
       traceIfFalse "Burning less or more than one control token" (mintedA == -1)
       ||
       (   traceIfFalse "Minting more than one control token"
                        (mintedA == 1)
        && traceIfFalse "The control token was not paid to the script address"
                        controlTokenPaid
        && traceIfFalse "The signer is not the sender on the escrow"
                        correctSigner
       )
     where
       info :: TxInfo
       info = scriptContextTxInfo ctx

       signer :: PubKeyHash
       signer = getSingleton $ txInfoSignatories info

       correctSigner :: Bool
       correctSigner = signerIsSender signer (sender $ eInfo escrowDatum)

       controlTokenPaid :: Bool
       controlTokenPaid =
           assetClassValueOf (txOutValue escrowUtxo) (assetClass mintedCS mintedTN)
           ==
           mintedA

       escrowUtxo :: TxOut
       escrowUtxo = getSingleton $ outputsAt addr info

       escrowDatum :: EscrowDatum
       escrowDatum = fromJust $ getTxOutDatum escrowUtxo info

       mintedCS :: CurrencySymbol
       mintedTN :: TokenName
       mintedA :: Integer
       (mintedCS, mintedTN, mintedA) = getSingleton $
                                       flattenValue $ txInfoMint info


Validator
---------

The on-chain validator as we briefly mentioned is parameterized on the receiver
address. This design decision allows us to have a unique script address for each
``ReceiverAddress``. Also, a validator function must mandatory take a datum, a
redeemer and a script-context and return a boolean. Because we are using typed
validators we will use the types we briefly introduce in the Business Logic
:ref:`Types section <business_logic-types>`.

The ``EscrowRedeemer`` allows us to decide which validations over the spending
utxo we need to perform. Canceling an escrow will excecute ``cancelValidator``
and resolving will execute ``resolveValidator``.

The common validation we do, independently of the redeemer, is checking we are
burning the *control token*

.. code:: haskell

   traceIfFalse "controlToken was not burned"
                (eAssetClass == assetClass mintedCS mintedTN && mintedA == -1)

Clearly, for performing this check we need to be sure we are minting a token *negatively*,
but it's really important to check the currency symbol and the token name are the
correct ones. A natural option for having this information, which isn't going to
change, is as a parameter of the validator, but this causes a circular dependency,
given the minting policy (from where the currency symbol is computed) already
depends on the address of the validator. So, a solution is to store the asset
class of the control token on the datum together with the escrow information.

The remaining validator depends on the redeemer

.. code:: haskell

   case r of
       CancelEscrow  -> cancelValidator eInfo signer
       ResolveEscrow -> resolveValidator info eInfo raddr signer


Cancel an escrow involves only checking the signer of the transaction is who started
the escrow. That is, checking the sender's address is the signer. We get the sender
address from the ``EscrowInfo`` inside the ``EscrowDatum``, and the signer from
``txInfoSignatories info``. One important thing to notice here, and in general every
time we use txInfoSignatories, is that the script context only has the *pubkey hash*
information of the signer address (without *staking hash*).

.. code:: haskell

   {-# INLINABLE cancelValidator #-}
   cancelValidator :: EscrowInfo -> PubKeyHash -> Bool
   cancelValidator EscrowInfo{..} signer =
       traceIfFalse "cancelValidator: Wrong sender signature"
                    $ signerIsSender signer sender


The resolve validation is a little more interesting. We also check the signer, but
this time it should be the receiver, as we mentioned that is the parameter of the
validator. The interesting validation we need to perform is to be sure the sender
address gets paid, at least the amount the EscrowInfo said it needs to be paid to
complete the agreement. From the script context we can retrive all the tokens that
are being paid to the sender address, with ``valuePaidTo (eInfoSenderAddr ei) info``
and check that is at least more than the amount computed by ``valueToSender ei``.

.. code:: haskell

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

We get the complete validator by putting everything together

.. code:: haskell

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
       info :: TxInfo
       info = scriptContextTxInfo ctx

       signer :: PubKeyHash
       signer = getFromSingleton $ txInfoSignatories info

       mintedCS :: CurrencySymbol
       mintedTN :: TokenName
       (mintedCS, mintedTN, mintedA) = getFromSingleton $
                                       flattenValue $ txInfoMint info




Compile to Plutus
-----------------



.. code:: haskell

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

   escrowAddress :: ReceiverAddress -> ContractAddress
   escrowAddress = mkValidatorAddress . escrowValidator

   controlTokenMP :: ContractAddress -> MintingPolicy
   controlTokenMP caddr =
       mkMintingPolicyScript $
       $$(compile [|| mkUntypedMintingPolicy . mkControlTokenMintingPolicy ||])
       `applyCode`
       liftCode caddr

   controlTokenCurrency :: ContractAddress -> CurrencySymbol
   controlTokenCurrency = scriptCurrencySymbol . controlTokenMP
