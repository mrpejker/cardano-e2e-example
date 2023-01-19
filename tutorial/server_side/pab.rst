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
Finally, an instance can be deleted by calling *stop*. 


Endpoints Specification
-----------------------

Let's review how this flow we described above works for the
Escrow example. One of the key points of design is the schema or endpoint's set.
We saw how to define this on the off-chain section, now is time to see how
this is related with the PAB service. Let's consider an example where
the sender wants to exchange 10 tokens ``A`` by 20 tokens ``B``.

We have only one kind of activation, that we will call *connect*,
and will let us interact with the endpoints defined on the ``EscrowSchema``. This
connect accion will need the ``WalletAddress`` of the user.

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
we have only one kind of activation and the parameter is a wallet address, specified
by payment key hash (``waPayment``) and staking key hash (``waStaking``).

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

We implement the little module ``Handlers``, which will have primarily boilerplate
code, in which we will define the different ways of connecting to the off-chain.
The ``Main`` module, which is even more simple, will just run the handlers.

First, we define the data type ``Escrow`` that will represent the different ways
of connection, this is one of the key parts of the module. Each different connection
will have its own set of endpoints.

.. code:: Haskell

   newtype Escrow = Connect WalletAddress
       deriving (Eq, Ord, Show, Generic)
       deriving anyclass (FromJSON, ToJSON, ToSchema)

Here, because the dApp is really simple, there is only one way of connecting to
the off-chain code. We only can ``Connect`` to the PAB from the client side by
providing the ``WalletAddress`` of the user. This wallet address is the one that
will be used by the function ``endpoints``.

The remaining key part then is to relate this ``Connect`` definition with the
``endpoints`` function. We will do this by instanciating the ``Escrow`` type
in the ``HasDefinitions`` typeclass.

.. code:: Haskell

   instance HasDefinitions Escrow where
       getDefinitions = []
       getSchema      = const []
       getContract    = getEscrowContract

   getEscrowContract :: Escrow -> SomeBuiltin
   getEscrowContract (Connect wa) = SomeBuiltin $ endpoints wa

The important part to be completed is the definition of ``getContract``. This
function takes something of type ``Escrow`` and will return any wrapped ``Contract``
monad action. Clearly, we must give a definition for each different way of connection.
In this case, given a ``Connect wa`` we return the wrapped ``endpoints`` function
applied to the wallet address of the user.

The remaining definitions of the typeclass, ``getDefinitions`` and ``getSchema``,
are not relevant to our approach, and we always will use this implementation for
any other dApp.

Finally, this typeclass instance is used then in the ``Main`` module to implement
the run funcion that will run the executable.

.. code:: Haskell

   main :: IO ()
   main = runWith $ handleBuiltin @Escrow
