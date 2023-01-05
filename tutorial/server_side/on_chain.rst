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
have any of the common Haskell types and functions in this scope. We must explictly
import from the Plutus Prelude all we need:

.. code:: haskell

   import PlutusTx.Prelude ( Integer, Bool
                           , ($), (&&), (||), (==)
                           , traceIfFalse
                           )

The on-chain validator, which briefly mention is parameterized on the receiver address.
This design decision allows us to have a unique script address for each ``ReceiverAddress``.
Also, a validator function must mandatory take a datum, a redeemer and a script-context
and return a boolean. Because we are using typed validators we will use the types
we briefly introduce in the Business Logic :ref:`Types section <business_logic-types>`.

The ``EscrowRedeemer`` allows us to decide which validations over the spending
utxo we need to perform. Canceling an escrow will excecute ``cancelValidator``
and resolving will execute ``resolveValidator``.

The common validation we do, independently of the redeemer, is checking we are
burning the *control token*. We can check this by simply getting all the minting
information from the ``ScriptContext`` (recalling *burning is negative minting*).
Once we get the complete list of tokens that are minted/burned with ``txInfoMint info``,
we check only one token is burned, with ``getSingleton`` and ``mintedA == -1``. And
the assetclass corresponds with the one stored on the ``EscrowDatum``, with
``eAssetClass == assetClass mintedCS mintedTN``.

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
       signer = getSingleton $ txInfoSignatories info

       mintedCS :: CurrencySymbol
       mintedTN :: TokenName
       (mintedCS, mintedTN, mintedA) = getSingleton $
                                       flattenValue $ txInfoMint info

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
