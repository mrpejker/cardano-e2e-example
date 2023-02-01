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
