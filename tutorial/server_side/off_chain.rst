Off-Chain
==========

The off-chain side of the server implementation is in charge of defining the interface
for interacting from a client, and performing the necessary actions of each operation
that basically consist of querying the blockchain and building transactions.
We divide the :code:`OffChain` module in two: :code:`Interface` and :code:`Operations`.

Inside :code:`Interface` we define the necessary types for specifying the
endpoints that the PAB service will provide to the client, and a type for
storing blockchain information that we want to communicate to the client. 
In `plutus-apps jargon` it corresponds to the `Schema` and the
`Observable State`.

Inside :code:`Operations` we define the main functions that will be called from the
PAB service module for providing the endpoints, and functions corresponding to each operation,
that can query the blockchain and build transactions.


Interface
----------

First thing we define is the Schema, which basically specifies the API that the
PAB service will provide. 

.. code:: Haskell
	  
  type EscrowSchema = Endpoint "start"   StartParams
                  .\/ Endpoint "cancel"  CancelParams
                  .\/ Endpoint "resolve" ResolveParams
                  .\/ Endpoint "reload"  ()

We define an endpoínt for each operation: `start`, `cancel` and `resolve`. Each one has
some parameters with the necessary input information.
In addition to them, we define a `reload` operation, which is used for
getting some state from the dApp that we need in the client side.


As we'll see later, when an end-user connects to the PAB service, the corresponding wallet
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

For canceling or resolving, the information needed is the reference to the utxo
corresponding to the specific escrow instance. For finding a utxo in the blockchain,
it's necessary the script address, and given that the validator is parameterized in the
receiver address, we also need that information. So in conclusion, for both operations
we need the scrip-utxo reference and the receiver address.
    

.. code:: Haskell

  data CancelParams = CancelParams
                      { cpTxOutRef        :: TxOutRef
                      , cpReceiverAddress :: ReceiverAddress
                      }
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


For resolving, given that the receiver address in `in the context` (the receiver is
the only one that can resolve an escrow), we don't need to provide that as part
of the endpoint parameter.

.. code:: Haskell

  newtype ResolveParams = ResolveParams { rpTxOutRef :: TxOutRef }
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


In addition to the Schema, we define the Observable State. It corresponds to the information
that we want to get from the client side.
In this case, we want to provide, for each user, the list of escrows waiting to be resolved
by this user. Thus, the frontend can display the list of escrows with their information.
For each escrow we include the utxo reference, the Escrow Info (containing the sender's address
and the asset class and amount to be paid for resolving), and the asset class and amount paid
by the sender.


.. code:: Haskell

  data UtxoEscrowInfo = UtxoEscrowInfo
                        { escrowUtxo    :: TxOutRef
                        , escrowInfo    :: EscrowInfo
                        , escrowPayment :: (AssetClass,Integer)
                        }
      deriving (Show, Generic)
      deriving anyclass (FromJSON, ToJSON)


The Observable State will be a list of :code:`UtxoEscrowInfo`.

The types defined here are the interface for communicating the client with the PAB service.
The client will send the endpoints parameters as JSON objects, that are converted to the Haskell
type, and vice-versa, the Observable State is converted to JSON for sending to the client.
We are currently using the derived instances of :code:`FromJSON` and :code:`ToJSON`, but the developer
can define an own definition, for having more control over the interface (and not depend on
the `deriving` implementation).


Operations
-----------

Now that we have defined the interface of our off-chain code, it's turn to implement the core functionality
for each operation. First, we define the function that connects each endpoint with the corresponding
off-chain function. This function is called :code:`endpoints` and will be called from the PAB service
module. It receives a :code:`WalletAddress` corresponding to the `connected` user that is calling
the endpoint.

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


Then we define functions for each operation. Let's review :code:`start`, :code:`resolve` and :code:`reload`
functions. We will show just some relevant code snippets here.

Starting an escrow consists of paying to a script the desired value that the sender wants to pay to the
receiver, including in the datum the corresponding Escrow Info.

.. code:: Haskell

  startOp
      :: forall w s
      .  WalletAddress
      -> StartParams
      -> Contract w s Text ()
  startOp addr StartParams{..} = do

So for specifying the transaction, we need to define the value and datum that will be part of
the script-utxo

.. code:: Haskell

      let 
          senderVal = assetClassValue sendAssetClass sendAmount
          val       = minAda <> cTokenVal <> senderVal
          datum     = mkEscrowDatum (mkSenderAddress addr)
                                    receiveAmount
                                    receiveAssetClass
                                    cTokenAsset

The value consists of a minimum amount of ADA, the control token which will be minted in this transaction,
and the tokens that should be paid to the receiver.
In the datum we include the sender's address, the payment expected and the control token asset class, that
will be burned at resolving or canceling.

Then we specify the transaction by defining lookups and constraints

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
  
In :code:`lkp` we define the lookups. In this case we are not spending any script-utxo, but we
are generating a new one and minting a token, so we declare the validator and minting policy.
We'll review their implementation in the following section.

In :code:`tx` we define the constraints. We declare that we pay to the script the defined datum and
value, we mint the control token, and the transaction must be signed by the sender public key.

Now we just need to `yield` the specified unbalanced transaction for being accessible from the
client side.

.. code:: Haskell
	  
          mkTxConstraints lkp tx >>= yieldUnbalancedTx

This would be the unbalanced transaction that `is sent` to the client for balancing, signing and submitting:

.. figure:: /img/unbalancedStart.png

	    
Let's review now the resolve operation. It receives the wallet address corresponding to the user
triggering the operation and the reference of the utxo generated at start.

.. code:: Haskell
	  
  resolveOp
      :: forall w s
      .  WalletAddress
      -> ResolveParams
      -> Contract w s Text ()
  resolveOp addr ResolveParams{..} = do

We have to build a transaction that spends the script utxo, pays to the sender
the tokens specified in the Escrow Info, and burns the control token.
First, we get the utxo and extract from there the Escrow Info.

.. code:: Haskell

      utxos <- lookupScriptUtxos contractAddress cTokenAsset
      utxo  <- findMUtxo rpTxOutRef utxos
      eInfo <- getEscrowInfo utxo

We use some utility functions for it. :code:`lookupScriptUtxos` gets a list of
utxos from a given address and containing a token of a given Asset Class.
:code:`findMUtxo` gets the utxo content from a given utxo reference and a list
of utxos. Finally :code:`getEscrowInfo` reads the datum of a given utxo and returns
the Escrow Info inside it.

For defining the transaction, we need to specify the payment that goes to the sender.

.. code:: Haskell

      let senderWallAddr = eInfoSenderWallAddr eInfo
          senderPayment  = valueToSender eInfo <> minAda

The sender address is defined in the Escrow Info, and for defining the payment
we use the function :code:`senderPayment`, implemented in the Business logic module.
This function will be used too in the on-chain validator for checking that the payment received by
the sender is correct.

Now we define the lookups and constraints.

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
              ]

In addition to the validator and control token minting policy, we include
in the lookups the utxo that is spent in this transaction.
The constraints specify that we spend the script-utxo using the redeemer
:code:`resolveRedeemer`, we burn the control token, the transaction must be
signed by the receiver, and pays to the sender the corresponding tokens specified
in the Escrow Info.

.. code:: Haskell

      mkTxConstraints @Escrowing lkp tx >>= yieldUnbalancedTx

The resulting unbalanced transaction is as follows

.. figure:: /img/unbalancedResolve.png


Let's finally review the :code:`reload` operation, which doesn't generate any transaction,
but it's in charge of reading the blockchain and writing
the updated obervable state. It corresponds to a list containing
the information of every escrow waiting to be resolved by the corresponding user address.


.. code:: Haskell

  reloadOp
      :: forall s
      .  WalletAddress
      -> Contract (Last [UtxoEscrowInfo]) s Text ()
  reloadOp addr = do
      let contractAddress = escrowAddress $ mkReceiverAddress addr

      utxos      <- utxosAt contractAddress
      utxosEInfo <- mapM mkUtxoEscrowInfoFromTxOut $ toList utxos

      tell $ Last $ Just utxosEInfo

Given that we are parameterizing the validator on the receiver address, getting the
corresponding list of escrow info is straightforward. We just need to get the utxos
belonging to the validator address (using :code:`utxosAt`), read the datum inside each
utxo (calling :code:`mkUtxoEscrowInfoFromTxOut`) and then write the updated observable state
(by calling :code:`tell`).

