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
correspond to integration tests running on a simulated blockchain.
The framework we use for it is based on the ``EmulatorTrace``, included in plutus-apps lib.

We implement first some basic traces, covering the different use cases of our Escrow example.
Then we explore a much more powerful approach, based on the `QuickCheck <https://www.cse.chalmers.se/~rjmh/QuickCheck>`_
Haskell library, which allows to automaticaly generate complex traces and ensure good properties.
The implementation is located at ``backend/test``.


Emulator Trace
--------------



In the Emulator Trace we can simulate wallets performing operations by calling endpoints
provided by the corresponding schemas



Property Based Testing
----------------------
