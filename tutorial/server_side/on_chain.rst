On-Chain
========

The on-chain side of the implementation consists of validators and minting policies.
In our example, the sender locks in a script-utxo the tokens to be paid to the
receiver, containing in the datum all the necessary information. In the same
transaction, a `control token` is minted too, for checking that information.
For cancelling or resolving, it's necessary to spend the script-utxo, which must
contain the control token, and the validator performs all the necessary checks.

In the :code:`OnChain` module, we find the Haskell implementation of the 
validator and the minting policy, among other helper functions. In the :code:`Validator`
module we define mostly boilerplate code for compiling to Plutus.

OnChain
-------

Because all the functions on this module needs to be compiled to Plutus we must
use the *Plutus Prelude* instead of the *Haskell Prelude*. To avoid confusion about
which Prelude we are using, we add the ``NoImplicitPrelude`` pragma at the top of
the module. As a result we don't have any of the common types and functions in this
scope. Thus we must explictly import from the Prelude all we need:

.. code:: Haskell

   import PlutusTx.Prelude ( Integer, Bool
                           , ($), (&&), (||), (==)
                           , traceIfFalse
                           )

Minting Policy
~~~~~~~~~~~~~~

Starting an escrow involves minting a *control token*, thus running a validation
implemented on its minting policy that allows the minting or burning. We use this
fact to ensure some good starting properties on the script utxo we are creating.
For that, we have to distinct if we are minting or burning by checking 
information inside the script context: if the amount of minted tokens is -1 we
are burning, if it's 1 we are minting. If the transaction mints a different amount
of tokens, the minting policy fails and so the start operation.
If we are minting the control token, then we have to check that it's paid to the
escrow script and the datum is correct.


.. code:: Haskell

   {-# INLINABLE mkControlTokenMintingPolicy #-}
   mkControlTokenMintingPolicy :: ScriptAddress -> () -> ScriptContext -> Bool
   mkControlTokenMintingPolicy addr _ ctx =
       traceIfFalse "Burning less or more than one control token" (mintedA == -1)
           ||
           (   traceIfFalse "Minting more than one control token"
                            (mintedA == 1)
            && traceIfFalse "The control token was not paid to the script address"
                            controlTokenPaid
            && traceIfFalse "Wrong information in Datum"
                            correctDatum
           )
     where
       ....

The minting policy is parameterized on the script address of the escrow utxo we are
creating, so we know where the control token must go. We check that it's paid
to the script-utxo in function :code:`controlTokenPaid`

.. code:: Haskell

      controlTokenPaid :: Bool
      controlTokenPaid =
          assetClassValueOf (txOutValue escrowUtxo) (assetClass mintedCS mintedTN)
          ==
          mintedA

      escrowUtxo :: TxOut
      escrowUtxo = getSingleton $ outputsAt addr info

      mintedCS :: CurrencySymbol
      mintedTN :: TokenName
      mintedA :: Integer
      (mintedCS, mintedTN, mintedA) = getSingleton $
                                      flattenValue $ txInfoMint info
      

As we mentioned before, we ensure that only one token is being minted, and it's implemented
by calling :code:`getSingleton` from the Utils, which takes a list and fails (by :code:`traceError`)
if the list doesn't contain exactly one element. It's also used for getting the unique
output utxo belonging to the script.

For ensuring that the datum is correct we need to check that the `sender address` coincides with
the transaction signer, the amount of tokens to receive is greater than zero, and the control
token Asset Class is the one that it's being minted.

.. code:: Haskell

      correctDatum :: Bool
      correctDatum =
          traceIfFalse "The signer is not the sender on the escrow"
                       correctSigner
       && traceIfFalse "The asset minted does not match with the control token"
                       correctControlAssetClass
       && traceIfFalse "The receive amount of tokens to exchange is not positive"
                       correctAmount

For implementing those three checks we simply read the script-utxo datum and
compare its information with the corresponding one.


Validator
~~~~~~~~~

The on-chain validator as we briefly mentioned is parameterized on the receiver
address. This design decision allows us to have a unique script address for each
``ReceiverAddress``. Also, a validator function must mandatory take a datum, a
redeemer and a script-context and return a boolean. Because we are using typed
validators we will use the types we briefly introduce in the Business Logic
:ref:`Types section <business_logic-types>`.

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
       ....

The common validation we do, independently of the redeemer, is checking we are
burning the *control token*.

.. code:: haskell

   mintedCS :: CurrencySymbol
   mintedTN :: TokenName
   mintedA :: Integer
   (mintedCS, mintedTN, mintedA) = getFromSingleton $
	                           flattenValue $ txInfoMint info

For performing this check we need to be sure we are minting a token *negatively*,
but it's really important to check the currency symbol and the token name are the
correct ones. A natural option for having this information, which isn't going to
change, is as a parameter of the validator, but this causes a circular dependency,
given the minting policy (from where the currency symbol is computed) already
depends on the address of the validator. So, a solution is to store the asset
class of the control token on the datum (``eAssetClass``) together with the escrow
information.

The ``EscrowRedeemer`` allows us to decide which validations over the spending
utxo we need to perform. Canceling an escrow will excecute ``cancelValidator``
and resolving will use ``resolveValidator``.

.. code:: haskell

   {-# INLINABLE cancelValidator #-}
   cancelValidator :: EscrowInfo -> PubKeyHash -> Bool
   cancelValidator EscrowInfo{..} signer =
       traceIfFalse "cancelValidator: Wrong sender signature"
                    $ signerIsSender signer sender

Cancel an escrow involves only checking the signer of the transaction is who started
the escrow. That is, checking the sender's address is the signer. We get the sender
address from the ``EscrowInfo`` inside the ``EscrowDatum``, and the signer from
``txInfoSignatories info``. One important thing to notice here, and in general every
time we use txInfoSignatories, is that the script context only has the *pubkey hash*
information of the signer address (without *staking hash*).

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

The resolve validation is a little more interesting. We also check the signer, but
this time it should be the receiver, as we mentioned that is the parameter of the
validator. The interesting validation we need to perform is to be sure the sender
address gets paid, at least the amount the EscrowInfo said it needs to be paid to
complete the agreement. From the script context we can retrive all the tokens that
are being paid to the sender address, with ``valuePaidTo (eInfoSenderAddr ei) info``
and check that is at least more than the amount computed by ``valueToSender ei``.

Validator
---------

In this module, we implement the compilation to Plutus of the on-chain validator
and the minting policy. In general because we are using a typed approach, on both the
validator and the minting policy, we have to go from a typed to an untyped setting,
compile to Plutus, and apply the *lifted* parameters. Luckily it's mostly repetitive
boilerplate, and for that reason, we are not going to get into too much details.

.. code:: haskell

   controlTokenMP :: ScriptAddress -> MintingPolicy
   controlTokenMP saddr =
       mkMintingPolicyScript $
       $$(compile [|| mkUntypedMintingPolicy . mkControlTokenMintingPolicy ||])
       `applyCode`
       liftCode saddr

The ``compile`` function will translate the Haskell minting policy implementation,
to which we are going to apply the script address, and finally, wrap everything
into the ``MintingPolicy`` type with ``mkMintingPolicyScript``.

.. code:: haskell

   controlTokenCurrency :: ScriptAddress -> CurrencySymbol
   controlTokenCurrency = scriptCurrencySymbol . controlTokenMP

Given a ``MintingPolicy`` we can easily compute its currency symbol. Compiling
the (typed) on-chain validator involves more or less the same "steps".

.. code:: haskell

   data Escrowing
   instance ValidatorTypes Escrowing where
       type instance DatumType    Escrowing = EscrowDatum
       type instance RedeemerType Escrowing = EscrowRedeemer

We define an empty data type that will help us annotate the typed validator, so
we can type the datum and redeemer types.

.. code:: haskell

   escrowInst :: ReceiverAddress -> TypedValidator Escrowing
   escrowInst raddr =
       mkTypedValidator @Escrowing
       ($$(compile [|| mkEscrowValidator ||])
           `applyCode`
           liftCode raddr
       )
       $$(compile [|| mkUntypedValidator @EscrowDatum @EscrowRedeemer ||])

Similarly to the minting policy, we compile the Haskell implementation, and
apply the corresponding parameter. One key difference is that building a ``TypedValidator``
involves passing the compiled typed on-chain validator and the compiled translator
from the typed to the untyped validator.

.. code:: haskell

   escrowValidator :: ReceiverAddress -> Validator
   escrowValidator = validatorScript . escrowInst

   escrowAddress :: ReceiverAddress -> ScriptAddress
   escrowAddress = mkValidatorAddress . escrowValidator

Once we have a ``TypedValidator``, we can get the proper validator and compute
the address.
