Plutus Application Backend
==========================

Now we have all the important pieces related to the server ready, we need to
plug the off-chain code into the PAB, so we can build the executable that will
be the complete *PAB service*.

We implement the little module ``Handlers``, which will have primarily boilerplate
code, in which we will define the different ways of connecting to the off-chain.
The ``Main`` module, which is even more little, will just run the handlers.

First, we define the data type ``Escrow`` that will represent the different ways
of connection, this is one of the key parts of the module.

.. code:: Haskell

   newtype Escrow = Connect WalletAddress
       deriving (Eq, Ord, Show, Generic)
       deriving anyclass (FromJSON, ToJSON, ToSchema)

Here, because the dApp is really simple, there is only one way of connecting to
the off-chain code. We only can ``Connect`` to the PAB from the client side by
providing the ``WalletAddress`` of the user. This wallet address is the one that
will be used by the function ``endpoints``.



The remaining part then is to relate this ``Connect`` definition with the
``endpoints`` function. We will do this by instanciating the ``Escrow`` type
in the ``HasDefinitions`` typeclass.

.. code:: Haskell

   instance HasDefinitions Escrow where
       getDefinitions = []
       getSchema      = const []
       getContract    = getEscrowContract

   getEscrowContract :: Escrow -> SomeBuiltin
   getEscrowContract (Connect e) = SomeBuiltin $ endpoints e

This typeclass instance is used then in the ``Main`` module

.. code:: Haskell

   main :: IO ()
   main = runWith $ handleBuiltin @Escrow
