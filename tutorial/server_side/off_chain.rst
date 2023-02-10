Off-Chain
==========

The off-chain side of the server implementation is in charge of defining the interface
for interacting with the client and performing the necessary actions
for each operation.
We divide the ``OffChain`` module in two: ``Interface`` and ``Operations``.

Inside :code:`Interface` we define the necessary types for specifying the
endpoints that the PAB service will provide to the client, and a type for
storing blockchain information that we want to communicate to the client.
In `plutus-apps jargon` it corresponds to the `Schema` and the
`Observable State`.

Inside :code:`Operations` we define the main functions that will be called from the
PAB service module for providing the endpoints, and functions corresponding to each operation,
that can query the blockchain and build transactions.


.. _offchain_interface:

Interface
----------

First thing we define is the Schema, which basically specifies the API that the
PAB service will provide.

.. code:: Haskell
	  
  type EscrowSchema = Endpoint "start"   StartParams
                  .\/ Endpoint "cancel"  CancelParams
                  .\/ Endpoint "resolve" ResolveParams
                  .\/ Endpoint "reload"  Integer

We define an endpoint for each operation: `start`, `cancel` and `resolve`. Each one has
parameters with the necessary input information.
In addition, we define a `reload` operation used for
querying blockchain information that is needed in the client side.

As we will see later, when an end-user connects to the PAB service, the corresponding wallet
address is set, so the parameter corresponding to the address that is performing
the operation is implicit. In other words, once the frontend connects with the PAB service,
it provides an API where the user address is set as a `context`.

For starting an escrow, in addition to the address of the user performing the operation,
we need to define the receiver address and the payments information:


.. code:: Haskell
	  
  data StartParams = StartParams
                     { receiverAddress   :: ReceiverAddress
                     , sendAmount        :: Integer
                     , sendAssetClass    :: AssetClass
                     , receiveAmount     :: Integer
                     , receiveAssetClass :: AssetClass
                     }
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

For canceling or resolving, it is required to have the reference to the script
UTxO corresponding to the specific escrow instance.
The validation script code must be included in the transaction and therefore,
as it is parameterized on the receiver address, the receiver address is also
required.

In the case of canceling, as it is done by the sender, both requirements are
included as parameters:

.. code:: Haskell

  data CancelParams = CancelParams
                      { cpTxOutRef        :: TxOutRef
                      , cpReceiverAddress :: ReceiverAddress
                      }
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


For resolving, as it is done by the receiver, the receiver address is `in the
context`, so we don't need to provide it as an endpoint parameter:

.. code:: Haskell

  newtype ResolveParams = ResolveParams { rpTxOutRef :: TxOutRef }
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


In addition to the Schema, we define the Observable State. It corresponds to
information that we want to make available to the client side through the PAB's
status endpoint. In this case we want to provide the list of escrows waiting to
be resolved by the connected user. This way, the frontend can display the
information in the UI and provide the option to resolve them.
For each escrow we include the UTxO reference, the Escrow Info (see
:ref:`section 2.1 <business_business>`), and the asset class and amount paid
by the sender:

.. code:: Haskell

  data UtxoEscrowInfo = UtxoEscrowInfo
                        { escrowUtxo    :: TxOutRef
                        , escrowInfo    :: EscrowInfo
                        , escrowPayment :: (AssetClass,Integer)
                        }
      deriving (Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

In addition to the escrows information, the Observable State includes a special integer called ``reloadFlag``:

.. code:: Haskell

  data ObservableState = ObservableState
                         { info       :: [UtxoEscrowInfo]
                         , reloadFlag :: Integer
                         }
      deriving (Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

The ``reloadFlag`` is used as a way to signal to the client side that the
Observable State has been updated in the PAB status after a call to the
``reload`` endpoint (see :ref:`section 3.2 <offchain_operations>` below).

The types defined here are the interface for communicating the client with the
PAB service. The client will send the endpoints parameters as JSON objects that
are converted to Haskell types. Vice versa, the Observable State is converted
to JSON for sending to the client. For the conversions, we currently use
derived instances of ``FromJSON`` and ``ToJSON``. To have more control over the
interface, developers can define their own instantiations of the JSON
conversion.


.. _offchain_operations:

Operations
-----------

Now that we have defined the interface of our off-chain code, it's turn to implement the core functionality
for each operation. First, we define the function that connects each endpoint with the corresponding
off-chain function. This function is called :code:`endpoints` and will be called from the PAB service
module. It receives a :code:`WalletAddress` corresponding to the `connected` user that is calling
the endpoint:

.. code:: Haskell

  endpoints
      :: WalletAddress
      -> Contract (Last [UtxoEscrowInfo]) EscrowSchema Text ()
  endpoints raddr = forever $ handleError logError $ awaitPromise $
                    startEp `select` cancelEp `select` resolveEp `select` reloadEp
    where
      startEp :: Promise (Last [UtxoEscrowInfo]) EscrowSchema Text ()
      startEp = endpoint @"start" $ startOp raddr

      cancelEp :: Promise (Last [UtxoEscrowInfo]) EscrowSchema Text ()
      cancelEp = endpoint @"cancel" $ cancelOp raddr

      resolveEp :: Promise (Last [UtxoEscrowInfo]) EscrowSchema Text ()
      resolveEp = endpoint @"resolve" $ resolveOp raddr

      reloadEp :: Promise (Last [UtxoEscrowInfo]) EscrowSchema Text ()
      reloadEp = endpoint @"reload" $ const $ reloadOp raddr

Then we define functions for each operation. Let's review the ``startOp``,
``resolveOp`` and ``reloadOp`` functions. We will show just some relevant code
snippets here.

First ``startOp``, that has the following header:

.. code:: Haskell

  startOp
      :: forall w s
      .  WalletAddress
      -> StartParams
      -> Contract w s Text ()
  startOp addr StartParams{..} = do

Starting an escrow consists of paying to a script the value that the sender
wants to pay to the receiver, including in the datum the corresponding escrow
information. So for specifying the transaction, we need to define the value and
datum that will be part of the script UTxO:

.. code:: Haskell

          senderVal = assetClassValue sendAssetClass sendAmount
          val       = minAda <> cTokenVal <> senderVal
          datum     = mkEscrowDatum (mkSenderAddress addr)
                                    receiveAmount
                                    receiveAssetClass
                                    cTokenAsset

The value consists of a minimum amount of ADA, the control token that will be
minted and the tokens that should be paid to the receiver.
In the datum we include the sender's address, the payment expected and the
control token asset class that will be burned at resolving or canceling.

Then we specify the transaction by defining lookups and constraints:

.. code:: Haskell

          lkp = mconcat
                [ typedValidatorLookups (escrowInst receiverAddress)
                , plutusV1OtherScript validator
                , plutusV1MintingPolicy (controlTokenMP contractAddress)
                ]
          tx  = mconcat
                [ mustPayToTheScriptWithDatumInTx datum val
                , mustMintValue cTokenVal
                , mustBeSignedBy senderPpkh
                ]
  
In :code:`lkp` we define the lookups. In this case we are not spending any script UTxO, but we
are generating a new one and minting a token, so we declare the validator and minting policy.
We will review their implementation in the :ref:`next section <onchain>`.

In :code:`tx` we define the constraints. We declare that we pay to the script the defined datum and
value, we mint the control token, and we require that the transaction must be
signed with the sender's public key.

Now we just need to `yield` the specified unbalanced transaction making it
accessible to the client side:

.. code:: Haskell
	  
          mkTxConstraints @Escrowing lkp tx >>= yieldUnbalancedTx

The following diagram illustrates the unbalanced transaction that is yielded to
the client for balancing, signing and submitting:

.. figure:: /img/unbalancedStart.png

	    
Let's review now the resolve operation:

.. code:: Haskell
	  
  resolveOp
      :: forall w s
      .  WalletAddress
      -> ResolveParams
      -> Contract w s Text ()
  resolveOp addr ResolveParams{..} = do
      ....
      ....

We have to build a transaction that spends the script UTxO, pays to the sender
the tokens specified in the Escrow Info, and burns the control token.
We also have to specify that the receiver gets the payment in the corresponding
address.
First, we get the UTxO and extract from there the Escrow Info:

.. code:: Haskell

      utxo  <- findValidUtxoFromRef rpTxOutRef contractAddress cTokenAsset
      eInfo <- getEscrowInfo utxo

We use the following auxiliary functions for it: ``findValidUtxoFromRef`` gets the UTxO
content from a given UTxO reference if the address is the given one, and the value
contains a token of the given asset class. The function  ``getEscrowInfo`` reads the datum of a
given UTxO and returns the Escrow Info inside it.

For defining the transaction, we need to specify the payment that goes to the
sender and the one that goes to the receiver:

.. code:: Haskell

      let cTokenVal      = assetClassValue cTokenAsset (-1)
          senderWallAddr = eInfoSenderWallAddr eInfo
          senderPayment  = valueToSender eInfo <> minAda
          escrowVal      = utxo ^. decoratedTxOutValue
          receivePayment = escrowVal <> cTokenVal

The sender address is defined in the Escrow Info, and for defining the payment
we use the function ``valueToSender``, implemented in the Business module.
This function will be also used in the on-chain validator for checking that the payment received by
the sender is correct.
Regarding the receiver's payment, it is basically the entire value contained in
the script UTxO, without the control token that must be burned.

Now we define the lookups and constraints:

.. code:: Haskell

          lkp = mconcat
              [ plutusV1OtherScript validator
              , unspentOutputs (singleton rpTxOutRef utxo)
              , plutusV1MintingPolicy (controlTokenMP contractAddress)
              ]
          tx = mconcat
              [ mustSpendScriptOutput rpTxOutRef resolveRedeemer
              , mustMintValue cTokenVal
              , mustBeSignedBy receiverPpkh
              , mustPayToWalletAddress senderWallAddr senderPayment
              , mustPayToWalletAddress addr receivePayment
              ]

In addition to the validator and control token minting policy, we include
in the lookups the UTxO that is spent in this transaction.
The constraints specify that we spend the script UTxO using the redeemer
:code:`resolveRedeemer`, we burn the control token, the transaction must be
signed by the receiver, pays to the sender the corresponding tokens specified
in the Escrow Info, and pays to the receiver the corresponding value.

Finally, we build the unbalanced transaction and yield it:

.. code:: Haskell

      mkTxConstraints @Escrowing lkp tx >>= yieldUnbalancedTx

The following diagram illustrates the yielded transaction:

.. figure:: /img/unbalancedResolve.png


Let's finally review the reload operation, which doesn't generate any transaction,
but it's in charge of reading the blockchain and writing
the updated obervable state. It corresponds to a list containing
the information of every escrow waiting to be resolved by the corresponding user address:

.. code:: Haskell

  reloadOp
      :: forall s
      .  WalletAddress
      -> Integer
      -> Contract (Last ObservableState) s Text ()
  reloadOp addr rFlag = do
      let contractAddress = escrowAddress $ mkReceiverAddress addr
          cTokenCurrency  = controlTokenCurrency contractAddress
          cTokenAsset     = assetClass cTokenCurrency cTokenName

      utxos      <- lookupScriptUtxos contractAddress cTokenAsset
      utxosEInfo <- mapM (mkUtxoEscrowInfoFromTxOut cTokenAsset) utxos

      tell $ Last $ Just $ mkObservableState rFlag utxosEInfo

For updating the observable state we need to look for the list of UTxOs
belonging to the script address. Function ``lookupScriptUtxos`` is used for
this, looking for UTxOs in the given address and containing the given token, in our case the control token.
Then we have to read the datum inside each UTxO, using the auxiliary function
``mkUtxoEscrowInfoFromTxOut``. Finally, we write the updated
observable state by calling the monadic function ``tell``.
