Testing
=======

Given the novelty in the programming paradigm of Cardano and the lack of a standard
framework for implementing dApps, testing is an area that is still under research
and development. Main projects in the ecosystem follow different
directions and strategies for testing their dApps, depending on the chosen architecture.

Regarding unit testing, each step in the development could be tested:
validator, transaction building, blockchain querying, transaction
balancing, among others.
Another big category of tests is about integration. In our approach, we have a big part
of the dApp flow implemented in the server side, and a smaller one but still important
is done in the client.
For testing the entire flow, blockchain simulators, private and public testnets can be used.

Strictly speaking, the kind of tests we present in this tutorial would
correspond to integration tests running on a simulated blockchain. We can test
the server side of our dApp, ensuring that the transactions are built as expected, and they
are properly validated.
The framework we use for it is based on the ``EmulatorTrace``, included in plutus-apps lib.

We implement first some basic traces, covering the different use cases of our Escrow example.
Then we explore a much more powerful approach, based on the `QuickCheck <https://www.cse.chalmers.se/~rjmh/QuickCheck>`_
Haskell library, which allows to automaticaly generate complex traces and ensure good properties.
The implementation is located at ``backend/test``.

Emulator Trace
--------------

In the Emulator Trace we simulate wallets performing operations by calling endpoints
provided by the corresponding schemas. A trace basically consists of a sequence of endpoint
calls from different simulated wallets, and its corresponding test predicates over the
wallets state at the end of the sequence.
When the trace runs, the cardano blockchain is simulated locally, submitting transactions
and running the validators when corresponding.
In ``Tests.OffChain`` we implemented traces for the main use cases. Let's review a trace consisting of
a ``wallet1`` starting an escrow with ``wallet2`` as receiver. What we need to do is:

- each wallet activates the endpoints set specifying their address.
- ``wallet1`` calls ``start`` specifying the escrow parameters: receiver address (``wallet2``),
  amount and asset class of tokens to send (50 ``tokenA``),
  and amount and asset class of tokens to receive (100 ``tokenB``).
- ``wallet2`` calls ``reload`` for reading the corresponding Observable State where the
  escrows waiting to be resolved are contained.
- ``wallet2`` calls ``resolve`` specifying the escrow to be resolved, which should
  be located at the head of the pending escrows list.

.. code:: Haskell

  trace :: EmulatorTrace ()
  trace =
      let startParams = mkStartParams
                          (mkReceiverAddress wallet2Addr)
                          50
                          (assetClass tokenACurrencySymbol tokenA)
                          100
                          (assetClass tokenBCurrencySymbol tokenB)

      in do
      h1 <- activateContractWallet wallet1 $ endpoints wallet1Addr
      h2 <- activateContractWallet wallet2 $ endpoints wallet2Addr

      callEndpoint @"start" h1 startParams
      void $ waitNSlots 10

      callEndpoint @"reload" h2 mockReloadFlag
      utxos <- getEscrowInfoList h2

      let resolveParams = mkResolveParams $ escrowUtxo $ head utxos
      callEndpoint @"resolve" h2 resolveParams
      void $ waitNSlots 10

At activation, a *handler* is returned, which is then used for calling the endpoints. This
simulates the activation-id we obtain when activating an instance with the PAB.
The trace implementation is straightforward. We use some utilities for obtaining
the escrow list contained in the observable state after calling ``reload``, and for
defining the operation parameters.
We also include some ``waitNSlots`` in the middle of the operations for simulating that
they are performed in different blockchain blocks.

After running this trace, we can test that the wallets' balances change properly:
``wallet1`` balance should change by substracting 50 ``tokenA`` and adding 100 ``tokenB``.
``wallet2`` balance should change by substracting 100 ``tokenB`` and adding 50 ``tokenA``.

.. code:: Haskell

  test :: TestTree
  test = checkPredicateOptions
          (defaultCheckOptions & emulatorConfig .~ emConfig)
          testMsg
          (walletFundsChange wallet1 (paymentA (-50) <> paymentB 100)
          .&&. walletFundsChange wallet2 (paymentB (-100) <> paymentA 50))
          trace

As before, we use some utilities for making easier the code reading. ``paymentA`` and
``paymentB`` are functions for creating a ``Value`` of ``tokenA`` and ``tokenB`` respectively.

We include more traces covering other use cases. It's important to note that in this example
we are testing our dApp behavior when our off-chain code is used. Another group of tests that
could be implemented with this testing framework is about possible *attacks*. For that, another
off-chain code building malicious or wrong transactions could be implemented, and the validator
should reject them.

Property Based Testing
----------------------

Up to this point, for testing a complete backend, we are using the ``EmulatorTrace``
for checking the off-chain implementation, also serving as some sort of humble e2e
test. Obviously, for this kind of test, we need to think of some combination of
operations that we translate on traces that will run on the emulator, and at the end,
we check everything went as planned by checking wallet balances. Clearly, this
approach requires thinking enough about particular traces trying to cover general
and ideally all corner cases, which for many situations could be insufficient.

An ideal scenario would be to auto-generate sequences of operations, each one
defining a trace, that can be run by the emulator and then check the result
of every run fits with some specification of the dApp backend. The ``ContractModel``
framework comes to attack this scenario in a very successful way.

Performing Property Based Testing involves writing general properties about the
server side operations. This means checking that `for any` randomly generated trace,
the wallet balances at the end of the execution are `correct`, or there is always a
way of retrieving locked tokens from any script utxo, among other interesting properties.
The key part for these properties is the implementation of an instance of the
``ContractModel`` typeclass over an ``EscrowModel`` type that will encode an
`abstract state` of the dApp backend. The instance will implement the set of actions
in correspondence to the operations together with a strategy to `arbitrarily` generate
them. Besides that there will be a `specification` of each action over the ``EscrowModel``
and finally a `semantic` of each action defined in term of the ``EmulatorTrace``.

On the ``tests/Tests/Prop`` folder, we find the complete implementation related
to property based testing. The relevant modules are ``Escrow`` where the properties
are implemented, and ``EscrowModel`` where the ``ContractModel`` particular instance
for the escrow can be found. Besides these modules, we have ``Gen`` which implements
some helper functions to generate tokens, wallets, etc, randomly. Finally, the
``Extra`` module in which we implemented some helper off-chain code functionality
needed for the ``ContractModel`` implementation.

Writing properties
~~~~~~~~~~~~~~~~~~

There are many interesting properties we can implement following this approach.
New ones defined by us or simply complete "holes" on some of the properties the
plutus-apps library has. As we briefly mentioned on this module we implement
a ``Basic`` strategy that will check the specification and the semantics we give
for the ``EscrowModel`` coincides with respect of wallet's balances. Together
with another more intersting property, called ``NoLockedFunds``, for checking we
always can retrive the funds locked on any escrow script utxo.

.. code:: Haskell

   -- | Basic property testing.
   propBasic :: Actions EscrowModel -> Property
   propBasic = propRunActionsWithOptions
               (options & increaseMaxCollateral)
               defaultCoverageOptions
               (const $ pure True)

The basic property comes al

.. code:: Haskell

   -- | No locked funds property testing.
   propNoLockedFunds :: Property
   propNoLockedFunds = checkNoLockedFundsProofWithOptions
                       (options & increaseMaxCollateral) noLockProof

   -- | No locked funds proofs.
   noLockProof :: NoLockedFundsProof EscrowModel
   noLockProof = defaultNLFP
                 { nlfpMainStrategy   = finishingMainStrategy
                 , nlfpWalletStrategy = finishingWalletStrategy
                 }

####

, and also a `semantic`

Completing the `ContractModel` instance implies implementing the
`arbitraryAction` for given the rules for generating valid traces
because in general not every combination of actions of a contract is a
valid trace. For instance, if the contract wasn't “started”, then any
action doesn’t make sense. Lastly, we need to complete some
“bureaucratic” implementation, so the `ContractModel` instances know
how to manage the emulator handlers, the connection with the off-chain
code, contract starts, etc.


- set of actions related to the operations
- specification
- semantics

that will specify which operations are available on the dApp backend,
and for those operations, we need to do two main things: (1) implement what we call the `specification`
of the operations, and (2) implement the `semantics` of each one of these operations
using the emulator.
Later for a particular action trace, we can be sure that the expected result of
the emulator coincides with the specification.


Concretely we need to implement the contract actions as an `Action`
type. For these, we define the “semantics” by implementing the
`perform` function, and for defining the “specification” we need to
implement the `nextState` function. In general, the specification will
be written stating how the wallet balances evolve (together with other
information) and also how the particular state of the contract
change. Clearly, we give “local” definitions for each action on the
`nextState` implementation so we can write `preconditions` for
ensuring the correct initial state.


#######################


Contract Model Instance
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: Haskell

   -- | This type represent all the Escrows a Wallet can resolve
   newtype EscrowModel = EscrowModel
                         { _toResolve :: Map Wallet [ExchangeInfo] }
       deriving (Show, Eq, Data)

   makeLenses 'EscrowModel

   emptyEscrowModel :: EscrowModel
   emptyEscrowModel = EscrowModel { _toResolve = empty }

   deriving instance Eq   (ContractInstanceKey EscrowModel w s e params)
   deriving instance Show (ContractInstanceKey EscrowModel w s e params)

   instance ContractModel EscrowModel where
       {- | Actions that can be done using the contract.
            - Start: Starts a new Escrow with the given parameters.
            - Resolve: Resolve the specific Escrow given the ExchangeInfo
            - Cancel: Cancels an existing Escrow
       -}
       data Action EscrowModel =
           Start { sWallet :: Wallet -- ^ Sender wallet
                 , rWallet :: Wallet -- ^ Receiver wallet
                 , sPay    :: (AssetClass, Integer)
                 -- ^ AssetClass and amount of the send Asset
                 , rPay    :: (AssetClass, Integer)
                 -- ^ AssetClass and amount of the receive Asset
                 }
           | Resolve { rWallet :: Wallet     -- ^ Receiver wallet
                     , eInfo :: ExchangeInfo -- ^ Exchange information
                     }
           | Cancel { rWallet :: Wallet     -- ^ Receiver wallet
                    , eInfo :: ExchangeInfo -- ^ Exchange information
                    }
           deriving (Eq, Show, Data)

       {- | Two kinds of handlers, the standard related to the Escrow dApp, and
            one that allows the wallet to lookup for specific escrows to cancel.
       -}
       data ContractInstanceKey EscrowModel w s e params where
           UserH :: Wallet
                 -> ContractInstanceKey EscrowModel (Last [UtxoEscrowInfo])
                                        EscrowSchema Text ()
           LookupH :: Wallet
                   -> ContractInstanceKey EscrowModel (Last [UtxoEscrowInfo])
                                          LookupSchema Text ()

       initialInstances = []

       instanceWallet (UserH w)   = w
       instanceWallet (LookupH w) = w

       instanceContract _ (UserH w)   _ = endpoints $ mockWAddress w
       instanceContract _ (LookupH _) _ = lookupEndpoint

       initialState   = emptyEscrowModel
       startInstances = eStartInstances

       -- | Arbitrary escrow model actions.
       arbitraryAction = eArbitraryAction

       precondition = ePrecondition

       -- | Escrow model specification.
       nextState = escrowSpecification
       -- | Escrow semantics using the emulator.
       perform   = escrowSemantics

       shrinkAction = eShrinkAction
       monitoring = eMonitoring
