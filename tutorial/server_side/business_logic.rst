Business Logic and Types
=========================

An Escrow instance is fully specified by the following information:

- Receiver's address.
- Sender's address.
- Asset Class and amount of token that the sender pays.
- Asset Class and amount of token that the receiver pays.

As we mentioned before, in the design we chose for this implementation, the sender
starts an escrow by submitting a transaction that pays to a script an amount of some
asset class. That script is parameterized in the receiver address,
and the datum contains the rest of the information: sender's address, asset class and
amount of tokens that the receiver must pay.
For validating the resolution of an escrow we need to check that when
the script utxo is being spent, the sender receives the corresponding amount of tokens,
and the transaction is signed by the receiver.
For validating that an escrow is canceled, the transaction spending
the script utxo must be signed by the sender.

In the :code:`Business` module we implement the data-type corresponding to the `state`
of the dApp that will be located inside the `Datum`. We also implement the core checks
and computations that will be used at building and validating the transactions.

In the :code:`Types` module we implement the data-types corresponding to the `Datum` and
`Redeemer`, and some other boilerplate.

Business
--------

We define some types for making eaiser to reason about the roles in an escrow.
Both sender and receiver are identified by a :code:`WalletAddress`, but we define
wrappers over it:

.. code:: haskell
	  
    newtype SenderAddress = SenderAddress { sAddr :: WalletAddress }
        deriving newtype (HP.Eq, HP.Show, Eq, FromJSON, ToJSON)

    newtype ReceiverAddress = ReceiverAddress { rAddr :: WalletAddress }
        deriving newtype (HP.Show, Eq, FromJSON, ToJSON)


Thus, the validator parameter will be :code:`ReceiverAddress`, and the remaining
information needed once a escrow is started is defined in the following data-type:
	
.. code:: haskell

  data EscrowInfo = EscrowInfo
                    { sender      :: SenderAddress
                    , rAmount     :: Integer
                    , rAssetClass :: AssetClass
                    }
      deriving (HP.Eq, HP.Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

We have now all the necessary ingredients to define the core functions needed for
building and validating the resolution or cancellation of an escrow.

.. code:: haskell

  {-# INLINABLE signerIsSender #-}
  signerIsSender :: PubKeyHash -> SenderAddress -> Bool
  signerIsSender pkh SenderAddress{..} =
      pubKeyHashInAddress pkh (fromWalletAddress sAddr)

  {-# INLINABLE signerIsReceiver #-}
  signerIsReceiver :: PubKeyHash -> ReceiverAddress -> Bool
  signerIsReceiver pkh ReceiverAddress{..} =
      pubKeyHashInAddress pkh (fromWalletAddress rAddr)

  {-# INLINABLE valueToSender #-}
  valueToSender :: EscrowInfo -> Value
  valueToSender EscrowInfo{..} = assetClassValue rAssetClass rAmount
      

For resolving an escrow, the transaction signer must be the `Receiver`, and
function :code:`signerIsReceiver` should be used when validating.
In addition to that, the transaction must pay to the sender the corresponding
value specified in the `EscrowInfo`. The function
:code:`valueToSender` should be used for computing that value at the moment of
building the transaction (off-chain), and for validating it (on-chain).
Similarly, for validating the cancellation of an escrow, function :code:`signerIsSender`
should be used.

Due to the simplicity of this dApp example, the Business logic is a short module and
doesn't contain too much code to share between off-chain and on-chain. In other
cases, we could have a complex `state` that is updated on each transaction, and much more
logic is implemented in the Business module. Nevertheless, even simple, this example
has some pieces of code which are critical and can be shared for building and validating
the transactions. 

.. _business_logic-types:

Types
-----

In :code:`Types` module we basically define the validator Parameter, Datum and Redeemer types.

The Datum contains the Escrow info together with the Asset Class of the control token that
is minted at start. It's needed for knowing which token must be burned at resolving or
canceling, and cannot be located in the parameter due to a circularity problem (we'll explain more
about this later).

.. code:: haskell

  data EscrowDatum = EscrowDatum
                     { eInfo       :: EscrowInfo
                     , eAssetClass :: AssetClass
                     }
      deriving Show


The Redeemer type specifies the different ways to spend a script utxo. In this case we have two:
resolve or cancel.

.. code:: haskell

  data EscrowRedeemer = CancelEscrow
                      | ResolveEscrow


The rest code in the module is mainly boilerplate.

