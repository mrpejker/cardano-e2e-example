Off-Chain
==========

The off-chain side of the server implementation is in charge of defining the interface
for interacting from a client, and performing the necessary actions of each operation
that basically consist of querying the blockchain and building transactions.
We divide the :code:`OffChain` module in two: :code:`Interface` and :code:`Operations`.

Inside :code:`Interface` we define the necessary types for specifying the
endpoints that the PAB service will provide to the client, and the type for storing the
current state of the dApp. In plutus-apps jargon it corresponds to the `Schema` and the
`Observable State`.

Inside :code:`Operations` we define the main functions that will be called from the
PAB service for providing the endpoints, and functions corresponding to each operation,
that can query the blockchain and build transactions.


Interface
---------

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



		  

Operations
---------
