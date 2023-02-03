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
plutus-apps library has to obtain known desirable properties. As we briefly mentioned
on the ``Escrow`` module we implement a ``Basic`` strategy that will check the specification
and the semantics we give for the ``EscrowModel`` coincides with respect of wallet's
balances. Together with another more intersting property, called ``NoLockedFunds``,
for checking we always can retrive the funds locked on any escrow script utxo.

.. code:: Haskell

   propBasic :: Actions EscrowModel -> Property
   propBasic = propRunActionsWithOptions
               (options & increaseMaxCollateral)
               defaultCoverageOptions
               (const $ pure True)

The basic property comes almost for free from the ``ContractModel`` instance of
the ``EscrowModel`` type, we just use ``propRunActionsWithOptions`` with default
options. That is enough to check the wallets’ balance correspondence between the
specification and the semantics. As we mentioned, a more interesting property can
be performed. We check that it is impossible to `block` funds forever in the script
utxo. The method for ensuring this is by implementing two strategies that will be a
kind of `proofs` we always can claim the locked funds. A proof will be a general
recipe for building a sequence of actions that will retrieve all the locked funds
for any initial sequence of actions.

.. code:: Haskell

   propNoLockedFunds :: Property
   propNoLockedFunds = checkNoLockedFundsProofWithOptions
                       (options & increaseMaxCollateral)
		       noLockProof

   noLockProof :: NoLockedFundsProof EscrowModel
   noLockProof = defaultNLFP
                 { nlfpMainStrategy   = finishingMainStrategy
                 , nlfpWalletStrategy = finishingWalletStrategy
                 }

To implement this property, we use ``checkNoLockedFundsProofWithOptions``, and
besides some default options, we need to provide a ``NoLockedFundsProof`` that
will implement the two strategies: ``finishingMainStrategy`` and ``finishingWalletStrategy``.
These are the "holes" we mentioned before we need to complete.

The implementation of ``finishingMainStrategy`` is proof that we can always
claim the locked funds from the script utxo using any wallet. Given the escrow
has a ``cancel`` operation, then for any unresolved escrow, we can always claim
the locked funds using this operation. Thus, the sequence of actions for retrieving
all the locked funds will be a sequence of ``cancels`` for all the created escrows.

.. code:: Haskell

   finishingMainStrategy :: DL EscrowModel ()
   finishingMainStrategy = do
       resolveMap <- viewContractState toResolve
       sequence_ [ action (Cancel w tInfo)
                 | w <- wallets
                 , w `Map.member` resolveMap
                 , tInfo <- fromJust $ Map.lookup w resolveMap
                 ]

We get all the escrows with ``viewContractState toResolve`` and then build the
sequence of cancel actions for every wallet that started an escrow. In a similar
way, we implement the ``finishingWalletStrategy``. The only difference here is
that we prove a particular wallet can claim all its locked funds without any help
from other wallets. Thus, in our example, both strategies are the same.

Contract Model Instance
~~~~~~~~~~~~~~~~~~~~~~~

The other core part we implement for writing properties is the ``ContractModel``
instance. We start by implementing the ``EscrowModel`` data type that will represent
an `abstract` state of the dApp, that is, all the escrows to be resolved or canceled.
We implement an instance of the ``ContractModel`` for this type.

.. code:: Haskell

   data ExchangeInfo =
       ExchangeInfo
       { tiSenderWallet      :: Wallet
       -- ^ The wallet that starts the escrow.
       , tiSenderAmount        :: Integer
       -- ^ The amount of tokens the sender locks in the script utxo.
       , tiSenderAssetClass    :: AssetClass
       -- ^ The asset class of the tokens the sender locks in the script utxo.
       , tiReceiverAmount     :: Integer
       -- ^ The amount of tokens the receiver must send to resolve the escrow.
       , tiReceiverAssetClass :: AssetClass
       {- ^ The asset class of the tokens the receiver must send to resolve the
            escrow. -}
       }

   newtype EscrowModel = EscrowModel
                         { _toResolve :: Map Wallet [ExchangeInfo] }

For simplicity, the ``EscrowModel`` encapsulates only a map from wallets
to lists of escrows to be **resolved**. So, the ``ExchangeInfo`` type contains
all the necessary information to specify an escrow. Of course, this design decision
allows us to cancel escrows and not only resolve them.

As part of the abstract representation of the dApp, we implement the `actions` the
dApp can perform. This is one of the first things we implement to give an instance
of the ``ContractModel``, together with, as we briefly mentioned, the `specification`
and the `semantics` of each action, and a way to generate them `arbitrarily`. There
are also other functions we need to implement, but we focus only on this four.

.. code:: Haskell

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

       -- | Arbitrary escrow model actions.
       arbitraryAction = eArbitraryAction

       -- | Escrow model specification.
       nextState = escrowSpecification

       -- | Escrow semantics using the emulator.
       perform   = escrowSemantics

       ...
       ...

In this particular escrow example, the actions are closely related to the operations
of the dApp server side support. We can ``Start``, ``Resolve`` or ``Cancel``, the
start encodes the complete information for creating an escrow. The resolve and
the cancel are very similar. In fact, it is  important to notice that the ``rWallet``
field is the receiver wallet, and the sender wallet information that is the one
which can perform the cancel operation is inside the ``ExchangeInfo``. This decision
can be a little confusing at first, but it simplifies the implementation. For
these actions we implement a function for randomly generate them.

.. code:: Haskell

   eArbitraryAction :: ModelState EscrowModel -> Gen (Action EscrowModel)
   eArbitraryAction s = do
       connWallet <- genWallet
       let toRes = Map.lookup connWallet (s ^. contractState . toResolve)
       oneof $ genStart connWallet :
             [ genResolve connWallet (fromJust toRes)
             | isJust toRes && not (null $ fromJust toRes)
             ] ++
             [ genCancel connWallet (fromJust toRes)
             | isJust toRes && not (null $ fromJust toRes)
             ]

The ``eArbitraryAction`` function randomly picks a wallet with ``genWallet`` and
then randomly uses `one of` the given generators that can be ``genStart``,
``genResolve`` or ``genCancel``.  For the last two, it uses the abstract state of
the dApp for filling in correct escrow information. Notice that we can completely
randomize a start, but to resolve or cancel, we need to have started an escrow.
The actualization of the abstract state is performed by the specification of the
each action, and also there we update the wallet balances.

.. code:: Haskell

   escrowSpecification :: Action EscrowModel -> Spec EscrowModel ()
   escrowSpecification Start{sWallet,rWallet,sPay,rPay} = do
       let (acA, aA) = sPay
           (acB, aB) = rPay

       withdraw sWallet (minAda <> assetClassValue acA aA)

       toResolve $~ insertWith (++) rWallet [ExchangeInfo sWallet aA acA aB acB]
       wait 2
   escrowSpecification Resolve{rWallet, eInfo} = do
       let ExchangeInfo{..} = eInfo
           rVal = assetClassValue tiReceiverAssetClass tiReceiverAmount
           sVal = assetClassValue tiSenderAssetClass tiSenderAmount

       withdraw rWallet rVal
       deposit rWallet sVal
       deposit tiSenderWallet (minAda <> rVal)

       toResolve $~ adjust (delete eInfo) rWallet
       wait 8
   ...
   ...

The start action must ``withdraw`` from the sender wallet ``sWallet`` the amount
``aA`` of token ``acA`` to lock in the escrow together with the minimal amount of
ADA, ``minAda <> assetClassValue acA aA`` and insert on the state that the receiver
wallet ``rWallet`` can resolve a new escrow completing the rest of the exchange
information. Then, the resolve action ``withdraw`` and ``deposit`` the different
tokens into de receiver wallets. Withdraw the tokens to be paid to the sender and
deposits the tokens coming from the sender. Besides that, on the sender wallet,
we deposit the tokens we withdraw from the receiver wallet plus the minimal amount
of ADA. Lastly, we delete the exchange information from the list of escrows to
resolve for the receiver wallet.

Finally, we implement the semantics of each action using the emulator trace. This
implementation, in general, should be straightforward given we call the endpoints
and query the observable state as we do it when writing traces.

.. code:: Haskell

   escrowSemantics
       :: HandleFun EscrowModel
       -> (SymToken -> AssetClass)
       -> ModelState EscrowModel
       -> Action EscrowModel
       -> SpecificationEmulatorTrace ()
   escrowSemantics h _ _ Start{sWallet,rWallet,sPay,rPay} = do
       let (acA, aA) = sPay
           (acB, aB) = rPay

       callEndpoint @"start" (h $ UserH sWallet) $
           mkStartParams (mkReceiverAddress $ mockWAddress rWallet) aA acA aB acB
       delay 2
   escrowSemantics h _ _ Resolve{rWallet, eInfo} = do
       callEndpoint @"reload" (h $ UserH rWallet) mockReloadFlag
       delay 5
       Last obsState <- observableState $ h $ UserH rWallet
       let utxoEscrowInfo = fromJust $
                            findEscrowUtxo eInfo (info $ fromJust obsState)

       callEndpoint @"resolve" (h $ UserH rWallet) $
           mkResolveParams (escrowUtxo utxoEscrowInfo)
       delay 2
   ...
   ...

First of all, a missing detail we are not going to mention because it is mainly
"boilerplate", is the activation of the endpoints for each wallet. The interesting
thing for us is that each wallet has its own handler that works as in the emulator
and we can retrieve with ``h $ UserH wallet``.

The semantics of the start action is basically call the ``start`` endpoint by
passing all the nesessary information, using the ``h $ UserH sWallet`` sender
wallet handler. The resolve action is more interesting because for resolving an
escrow we need to get the list of escrows we can resolve. We call ``reload`` so
then we can get the observable state and search for the escrow that matches the
exchange information. Once we get the ``utxoEscrowInfo``, we call ``resolve``.

The semantics of the start action is basically call the ``start`` endpoint by passing
all the necessary information, using the ``h $ UserH sWallet`` sender wallet handler.
The resolve action is more interesting because for resolving an escrow, we need
to get the list of escrows we can resolve. We call the ``reload`` endpoint so then
we can get the observable state and search for the escrow that matches the exchange
information. Once we get the ``utxoEscrowInfo``, we call the ``resolve`` endpoint
with the script utxo.
