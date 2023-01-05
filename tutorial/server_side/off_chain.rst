Off-Chain
==========

The off-chain side of the server implementation is in charge of defining the interface
for interacting from a client, and performing the necessary actions of each operation
that basically consist of querying the blockchain and building transactions.
We divide the :code:`OffChain` module in two: :code:`Interface` and :code:`Operations`.

Inside :code:`Interface` we define the necessary types for specifying the
endpoints that the PAB service will provide to the client, and the type for storing the
current state of the dApp. In `plutus-apps jargon` it corresponds to the `Schema` and the
`Observable State`.

Inside :code:`Operations` we define the main functions that will be called from the
PAB service module for providing the endpoints, and functions corresponding to each operation,
that can query the blockchain and build transactions.


Interface
----------

First thing we define is the `Schema`, which basically specifies the API that the
PAB service will provide. 

.. code:: Haskell
	  
  -- | Escrow Schema
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
that we want to get from the blockchain from the client side.
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

