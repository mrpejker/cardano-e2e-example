Plutus Application Backend
==========================

Now we reviewed the core pieces of the server implementation, it's turn to
take a look to the web service. It's implemented using the PAB module
from plutus-apps libraries.

As we mentioned before, in our approach we use some features of the plutus-apps
libraries, and discard others. The PAB library allows to implement a web
service providing the API for interacting with the off-chain code of the dApp.
We just need a subset of that API containing the operations **activate**,
**endpoint**, **status** and **stop**.

For interacting with the PAB service it's necessary to get an *instance id*
by calling *activate*. The activation provides an interface through
dApp *endpoints*, defined in a schema. Different schemas will offer
different interfaces, which are chosen at activation.
In our case we have only one, but other complex dApps could provide
more.
The activation could require some parameters which are instantiated and
will persist while the instance is alive. In the Escrow example,
the client connects with the PAB by activating
an instance providing a wallet address. After that, the four endpoints
defined in the schema are accessible for performing the operations.
For any communication from the server to the client, as sending
unbalanced transactions or other blockchain query responses,
the *status* endpoint is used.
Finally, an instance can be deleted by calling *stop*. 


Endpoints Specification
-----------------------

Let's review how this flow we described above works for the
Escrow example. One of the key points of design is the schema or endpoints' set.
We saw how to define this on the Off-chain section, now is time to see how
this is related to the PAB service. Let's consider an example where
the sender wants to exchange 10 tokens ``A`` by 20 tokens ``B``.

We have only one kind of activation, and will let us interact with the endpoints
defined on the ``EscrowSchema``. It needs the user's ``WalletAddress``.

Activating an instance is performed by the ``api/contract/activate`` PAB endpoint,
requiring the following information in the body

.. code::

   {
      "caID":{
         "waPayment":{
            "getPubKeyHash":"PUBKEYHASH"
         },
         "waStaking":{
            "getPubKeyHash":"PUBKEYHASH"
         }
      },
      "caWallet":{
         "getWalletId":0000000000000000000000000000000000000000
      }
   }

This JSON has two main fields: ``caID`` and ``caWallet``. The latter is necessary
if the PAB is used with cardano-wallet, so we set any number (given that we
don't use that functionality).
The important information goes inside ``caID``, where basically we specify the
kind of activation we are doing and the corresponding parameters. In our case
we have only one kind of activation, whose parameter is a wallet address, specified
by payment pub key hash (``waPayment``) and staking pub key hash (``waStaking``).

The response of the activation call is an instance id, that is necessary for calling
the off-chain operations. The PAB endpoint for calling them depends on the schema
exposed in the activation:
``api/contract/instance/[instance-id]/endpoint/[off-chain operation]``.

For instance, if we want to perform an ``start`` for creating a escrow, we must call
``api/contract/instance/[instance-id]/endpoint/start`` specifying in the body
the ``StartParams`` information on JSON format.

.. code::

   {
      "receiverAddress":{
         "waPayment":{
            "getPubKeyHash":"PUBKEYHASH"
         },
         "waStaking":{
            "getPubKeyHash":"PUBKEYHASH"
         }
      },
      "sendAssetClass":{
         "unAssetClass":[
            {
               "unCurrencySymbol":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            },
            {
               "unTokenName":"A"
            }
         ]
      },
      "sendAmount":10,
      "receiveAssetClass":{
         "unAssetClass":[
            {
               "unCurrencySymbol":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            },
            {
               "unTokenName":"B"
            }
         ]
      },
      "receiveAmount":20
   }


Notice that we don't need to pass again the wallet address of the user performing the operation,
because it is set at activation and it persists until the instance is stopped. For doing that,
the PAB endpoint stop is called: ``api/contract/instance/[instance-id]/stop``.


Implementation
--------------

Let's briefly review how the web service is implemented. It's mainly boilerplate,
we just need to *connect* the PAB activation with the corresponding off-chain code.
Inside ``app`` folder we find two modules: ``Handlers``, containing the code for
connecting the PAB activation with the corresponding off-chain code, and ``Main``
which contains the main function of the web service.

Inside ``Handlers`` we define the data type ``Escrow`` that will represent the different ways
of connection, this is one of the key parts of the module. Each different connection
will have its own set of endpoints.

.. code:: Haskell

   newtype Escrow = Connect WalletAddress
       deriving (Eq, Ord, Show, Generic)
       deriving anyclass (FromJSON, ToJSON, ToSchema)

As we mentioned before, in our example we have only one kind of activation, so our ``Escrow``
type has a unique constructor that we call ``Connect``. It receives the activation parameter,
which in this case is a ``WalletAddress``.

Now the only remaining part is to relate the ``Escrow`` type with the off-chain code.
Remember that we defined an ``endpoints`` function in ``OffChain.Operations`` module:

.. code:: Haskell

  endpoints
      :: WalletAddress
      -> Contract (Last [UtxoEscrowInfo]) EscrowSchema Text ()
  endpoints raddr = forever $ handleError logError $ awaitPromise $
                    startEp `select` cancelEp `select` resolveEp `select` reloadEp
    where
      .....
      .....

This function implements an infinite loop exposing the dApp endpoints corresponding
to each operation.
Connecting this function with the PAB handler is mainly boilerplate:

.. code:: Haskell

   instance HasDefinitions Escrow where
       getDefinitions = []
       getSchema      = const []
       getContract    = getEscrowContract

   getEscrowContract :: Escrow -> SomeBuiltin
   getEscrowContract (Connect wa) = SomeBuiltin $ endpoints wa

We need to instantiate the ``HasDefinitions`` typeclass, where the only
function that we are interested on is ``getContract``.
The others are not necessary for implementing the web service in our approach,
so we complete them with a trivial definition.
In ``getContract`` we basically specify which off-chain function is called for
each constructor of the ``Escrow`` type, i.e., for each way of activation.
We have only one and corresponds to the ``endpoints`` function.

Finally, inside ``Main`` module we have the main function that runs the service.
It's boilerplate and the only hole to be filled is the type containing the
ways to activate the PAB service, which has defined the handlers. In our case it's
the ``Escrow`` type.

.. code:: Haskell

   main :: IO ()
   main = runWith $ handleBuiltin @Escrow
