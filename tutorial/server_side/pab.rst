Plutus Application Backend
==========================

Now we have all the important pieces related to the server ready, we need to
plug the off-chain code into the PAB, so we can build the executable that will
be the complete *PAB service*.

The PAB, as we mentioned, allows us to execute off-chain operations through API
calls. The typical flow for doing these calls is by first *activating* an *instance*
(or *session*). Potentially could be different kinds of activations, one of the
interesting design patterns for us is that each activation will expose different
*schemas*, and every schema will define a set of endpoints. Performing an activation
will give us an *instance-id*, that we must use every time we want to call an off-chain
operation through one of the endpoints available on the schema. Lastly, when we don't
want to interact anymore with the PAB we *stop* the session.

Endpoints Specification
-----------------------

Let's review how this typical flow we just described works for our particular
escrow example. One of the key points of design is the schema or endpoint's set.
We saw how to define this on the off-chain section, now is time to see how from
this definition we can interact through the PAB. The complete `endpoints specification <https://github.com/joinplank/cardano-e2e-example/blob/main/doc/endpoints-spec.md>`_
can be found on the design document folder.

On the following we will introduce the flow with an example where the sender wallet
wants to exchange 10 tokens ``A`` by 20 tokens ``B`` with a receiver wallet.

In this dApp there is only one kind of activation, that we will call *connect*
and will let us interact with the endpoints defined on the ``EscrowSchema``. This
connect accion will need the ``WalletAddress`` of the user.

Activating an instance is perform by the ``api/contract/activate`` fixed endpoint
of the PAB, and in the body we give the information about the kind of activation.

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

On this particular case, because we only have one kind of activation we just give
the wallet address through ``caID``. Besides that, the activate endpoint needs a
wallet id to work, but the good news is that for our approach can be any.

Once the activation is done, the response of the call will be an instance id, that
we must use for calling the off-chain operations. This is done using another
PAB endpoint that depends on the schema exposed on the activation:
``api/contract/instance/[instance-id]/endpoint/[off-chain operation]``.

For instance, if we want to do an ``start`` for creating a escrow we must call
``api/contract/instance/[instance-id]/endpoint/start`` and in the body we must
pass the ``StartParams`` information on JSON format.

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

Finally we can end our session on the PAB, by calling ``api/contract/instance/[instance-id]/stop``.

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
