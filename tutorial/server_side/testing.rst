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
          (walletFundsChange wallet1 (valueA (-50) <> valueB 100)
          .&&. walletFundsChange wallet2 (valueB (-100) <> valueA 50))
          trace

As before, we use some utilities for making easier the code reading. ``valueA`` and
``valueB`` are functions for creating a ``Value`` of ``tokenA`` and ``tokenB`` respectively.

We include more traces covering other use cases. It's important to note that in this example
we are testing our dApp behavior when our off-chain code is used. Another group of tests that
could be implemented with this testing framework is about possible *attacks*. For that, another
off-chain code building malicious or wrong transactions could be implemented, and the validator
should reject them.

Property Based Testing
----------------------

Up to this point, regarding testing, we are using the ``EmulatorTrace``
as some kind of integration test where the entire dApp flow is simulated by
calling off-chain endpoints and checking wallet balances.
This approach requires thinking enough about particular traces trying to cover interesting
cases, but for many situations it could be insufficient.

An ideal scenario would be to auto-generate sequences of operations, each one
defining a trace, run them on the emulator and then check some desired properties.
This way of testing is called *Property Based Testing*, offered by the
``ContractModel`` framework.

Instead of thinking concrete possible traces, the Property Based Testing approach
consists of *specifying* what is the expected behavior after each dApp operation,
*generating* random traces, running them on ``EmulatorTrace``, and finally checking that the
final state on the emulator matches with the expected one according to the specification.
For this, an instance of ``ContractModel`` typeclass must be defined.
It specifies an `abstract state` of the dApp, how this state
changes after the execution of each operation (which in this context is called `action`),
how to `arbitrarily` generate sequence of operations, and how to relate each action with
an off-chain operation, tipically an endpoint call. This `relation` between an action
in the contract model and an off-chain operation could be considered as the `semantics`
of the specification.
Once the ``ContractModel`` instance is defined, different `properties` can be specified.
Some of them are partially implemented in plutus-apps lib and the developer just
need to complete some holes related to the particular instance. The ``NoLockedFunds`` is
an example of these properties, which expresses an invariant ensuring that there exists a
way of retrieving tokens locked in a script.

On the ``tests/Tests/Prop`` folder, we find the complete implementation related
to Property Based Testing. The relevant modules are ``EscrowModel``, where the ``ContractModel`` instance
is defined, and ``Escrow``, where the properties are implemented.
Besides these modules, we have ``Gen`` which implements
some helper functions to generate tokens, wallets, etc, randomly. Finally, the
``Extra`` module in which we implemented some helper off-chain code functionality
needed for the ``ContractModel`` implementation.


Contract Model Instance
~~~~~~~~~~~~~~~~~~~~~~~

Let's review how the ``ContractModel`` instance is defined in our Simple Escrow
dApp. First, we define a data type called ``EscrowModel`` which specifies
the state we want to predicate over. It consists of a list of all escrows
already started and waiting to be resolved or canceled.

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

The ``EscrowModel`` stores a map from wallets to the lists of escrows to be **resolved**
by those wallets (i.e. the receivers).
This design decision makes trivial to retrieve escrows to be resolved, but
it's a little harder to find the escrows to be canceled given that
it's necessary to look inside the ``ExchangeInfo`` for finding the sender wallet.

As part of the abstract representation of the dApp, we implement the `actions` the
dApp can perform. This is one of the first things we implement to give an instance
of ``ContractModel``, together with the `specification`
and the `semantics` of each action, and a way to generate them `arbitrarily`. There
are also other functions we need to implement, but we focus only on these in this text.

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

In our example, the actions are closely related to the off-chain endpoints we offer
to the users as operations, but in other examples it could be different.
``Start`` action encodes the complete information for creating an escrow.
``Resolve`` and ``Cancel`` are very similar and contains
the information needed for resolving or canceling an escrow.
Notice that the ``rWallet`` field is the receiver wallet,
and the sender wallet is inside the ``ExchangeInfo``, as we mentioned before.

Then, we implement a function for randomly generate sequence of actions

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
randomly uses `one of` the given generators that can be ``genStart``,
``genResolve`` or ``genCancel``.  For the last two, it uses the abstract state of
the dApp for filling in correct escrow information. Notice that we can completely
randomize a start action, but to resolve or cancel,
we need to have an escrow already started.


The specification of how the state is updated after each action and how wallet
balances change is defined in function ``escrowSpecification``:

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
ADA, and insert on the state that the receiver
wallet ``rWallet`` can resolve a new escrow completing the rest of the exchange
information.
Then, the resolve action correspond to ``withdraw`` from the receiver wallet
the tokens to be paid to the sender,
and ``deposit`` to the receiver wallet the tokens that come from the sender.
In addition to that, we deposit to the sender wallet the tokens
we withdraw from the receiver wallet plus the minimal amount
of ADA. Lastly, we delete the exchange information from the list of escrows to
resolve by the receiver wallet.

Finally, we implement the `semantics` of each action using the emulator trace. This
implementation, in general, should be straightforward given that we call the endpoints
and query the observable state as we do it when writing traces.
We show here just the definition corresponding to `start` and `resolve` actions.
The interested reader can review the `cancel` one in the source code.

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

The semantics of the `start` action consists of basically calling the ``start`` endpoint by
passing all the nesessary information, using the ``h $ UserH sWallet`` sender
wallet handler. The `resolve` action is more interesting because for resolving an
escrow we need to get the list of escrows we can resolve. We call ``reload`` so
then we can get the observable state and search for the escrow that matches the
exchange information. Once we get the ``utxoEscrowInfo``, we call ``resolve``.


Writing properties
~~~~~~~~~~~~~~~~~~

Once we implement the instance of ``ContractModel`` we define properties
our dApp should satisfy. The plutus-apps library provides some good ones,
and the developer can define any others.

In our implementation, we implement a `basic` strategy for checking
wallet balances after running generated traces. This property comes
`for free` from plutus-apps after defining the contract model.
In addition to that, we implement the ``NoLockedFunds`` property,
which ensures that it's possible to retrieve funds locked on any
script utxo related to our dApp.

The implementation is located inside the ``Escrow`` module:

.. code:: Haskell

   propBasic :: Actions EscrowModel -> Property
   propBasic = propRunActionsWithOptions
               (options & increaseMaxCollateral)
               defaultCoverageOptions
               (const $ pure True)

The `basic` property is almost boilerplate. We just use
``propRunActionsWithOptions`` with default
options, whicht is enough to check the wallets’ balance correspondence between
the specification and the semantics (i.e., the final state on the emulator
trace after running the traces).

Then we implement the property ensuring that it's impossible to
block funds forever in script utxos.
Plutus-apps library helps on that and requires to implement some kind of
`proof` showing that it's always possible to claim locked funds. This
proof is a general recipe for building a sequence of actions that retrieves
locked funds for any possible initial configuration.

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

We use the library function ``checkNoLockedFundsProofWithOptions``, and
besides some default options, we need to provide a ``NoLockedFundsProof``
implementing two strategies: ``finishingMainStrategy`` and ``finishingWalletStrategy``.

The `main strategy` consists of proving that it's possible to claim the locked funds
using `any` wallet. In our case, the funds are locked when an escrow is started,
and given that the sender can cancel the escrow at any moment, it's evidence that
for any possible case, there exists a way to retrieve the funds (by canceling escrows).


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
sequence of cancel actions for every wallet that started an escrow.

The `wallet strategy` consists of showing that a particular wallet can claim its own
locked funds without help of other wallets. In our case the strategy coincides with
the main one, given that the only way to lock funds in our dApp is by creating
an escrow, and the way of retrieving the funds is by canceling it.
