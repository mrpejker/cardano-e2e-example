Cardano dApp Architecture
=========================

In order to introduce the dApp architecture we propose, let's quickly 
remind what a smart contract or dApp is on Cardano.
Unlike EVM blockchains, where a smart contract is a program that runs on-chain,
the unique piece of code running on-chain in Cardano is a **validator**. This is just
a boolean function that decides if a submitted transaction must be accepted or not.
The state of a dApp is stored in one or more utxos, and
the way of updating it, is by submitting transactions. Here comes the off-chain
code, which is in charge of building, balancing, signing and submitting transactions.

For implementing the off-chain side of a dApp, we propose a Client-Server architecture.
The unique component on the Client side is the **Browser**,
which runs the dApp frontend code, balances, signs and submits transactions.
On the Server side, we run three services: the Plutus Application Backend (**PAB**),
an Indexer service (currently **Blockfrost**), and a **Budget** service.
The PAB is in charge of running the dApp off-chain code for building an
unbalanced transaction. The Indexer service is used by PAB for querying the
blockchain. Finally, the Budget service provides the evaluation of Plutus scripts,
obtaining memory and cpu units needed for processing a transaction on the blockchain.

The following diagram shows the complete flow for performing a dApp operation:

.. figure:: /img/dapp-flow.png

The Browser sends an HTTP request to the corresponding operation endpoint provided by
PAB (1). Then it performs some queries to the Indexer (2) and gets the information needed
for building the unbalanced transaction (3). For getting it, it's necessary to poll the PAB status
by calling an endpoint called *reload* (4).
Then, the Browser balances the received transaction (5) and obtains the memory and cpu units of
the included scripts by calling the Budget service (6). With that information, the definitive balance
can be computed (7) and the transaction is ready for signing (8) and submitting (9). 
	    


Server Side
-----------

From the three components included on the Server side, two of them are common services for
any dApp: the Indexer and the Budget.
The remaining one is the PAB, that exposes endpoints for getting the application state and
for building unbalanced transactions for each operation.

The plutus-apps library provides an easy way to implement a web server for exposing endpoints
connected to each dApp operation. This web server is commonly called Plutus Application Backend.
In its original version, the PAB and all the related componentes, were conceived for being
in charge of the entire flow of building, balancing, signing, submitting and check
status of transactions. In our approach, we propose to use the plutus-apps tools
**only for building unbalanced transactions**.
It avoids a lot of problems related to blockchain syncing, transaction submissions,
rollbacks and others, but still allows to use Haskell code for running the necessary
dApp business logic, with the huge benefit of sharing the same code between the on-chain
validator and the off-chain code.
Concretely, we use *Contract Monad* just for connecting with the Indexer service (currently
using Blockfrost, but it can be replaced by any other) and the *Constraints Library*,
which provides a declarative interface for building transactions.

The following diagram shows the design of the PAB service of a dApp:

.. figure:: /img/pab-architecture.png
