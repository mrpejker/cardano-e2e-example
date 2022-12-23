Cardano dApp Architecture
=========================

In order to introduce the dApp architecture we propose, let's quickly 
remind what a smart contract, or better called **decentralized Application** (dApp),
is on Cardano.
Unlike EVM blockchains, where a smart contract is a program that runs on-chain,
the unique piece of code running on-chain in Cardano is a **validator**. This is just
a boolean function that decides if a submitted transaction must be accepted or not.
The state of a dApp is stored in one or more utxos, and
the way of updating it, is by submitting transactions that spends those utxos
and produce others. Here comes the off-chain
code, which is in charge of building, balancing, signing and submitting transactions.

For implementing the off-chain side of a dApp, we propose a Client-Server architecture.
The unique component on the Client side is the **Browser**,
which runs the dApp frontend code, balances, signs and submits transactions.
On the Server side, we run three services: the Plutus Application Backend (**PAB**),
an Indexer service (currently **Blockfrost**), and a **Budget** service.
The PAB is in charge of running the dApp off-chain code for building an
unbalanced transaction. The Indexer service is used by PAB for querying the
blockchain. Finally, the Budget service provides the evaluation of Plutus scripts,
obtaining memory and cpu units that a validator consumes, which is
needed for fully balancing transactions.

The following diagram shows the complete flow for performing a dApp operation:

.. figure:: /img/dapp-flow.png

The Browser sends an HTTP request to the corresponding operation endpoint provided by
PAB (1), which then performs some queries to the Indexer (2) and gets the information needed
for building the unbalanced transaction (3). For getting it, it's necessary to poll the PAB status
by calling an endpoint that we called *reload* (4).
Once the unbalanced transaction is received, the Browser balances it (5) and obtains the memory and cpu units of
the included scripts by calling the Budget service (6). With that information, the definitive balance
can be computed (7) and the transaction is ready for signing (8) and submitting (9). 
	    


Server Side
-----------

From the three components included on the Server side, two of them are common services for
any dApp: the Indexer and the Budget. 
Let's focus on the remaining one, which is specific to each dApp,
in charge of exposing endpoints for getting the application state and
building unbalanced transactions for each operation. 

The plutus-apps library provides an easy way to implement a web server for exposing endpoints
connected to each dApp operation. This web server is known as Plutus Application Backend.
In its original version, the PAB and all the related componentes were conceived for being
in charge of the entire flow of building, balancing, signing, submitting and checking
status of transactions. In our approach, we propose to use the plutus-apps tools
**only for building unbalanced transactions**.
It avoids a lot of problems related to blockchain syncing, transaction submissions,
rollbacks and others, but still allows to use Haskell code for running the necessary
dApp business logic, with the huge benefit of sharing the same code between the on-chain
validator and the off-chain code.
Concretely, we use *Contract Monad* just for connecting with the Indexer service (currently
using Blockfrost, but it can be replaced by any other) and the *Constraints Library*,
which provides a declarative interface for building transactions.

The following diagram shows a simplified version of PAB service's design of a dApp:

.. figure:: /img/pab-architecture.png

In general, the computation needed for updating the state of a dApp must be done in both
sides off-chain and on-chain. We call **Business logic** to the piece of code in charge of
that. It's implemented on Haskell using **Plutus Prelude** library.
The **OnChain** module contains validator code, which depends on the Business logic
and Plutus Prelude. This Haskell code is then compiled to Plutus Core.
The **OffChain** module contains the core code for building transactions.
It depends on plutus-apps libraries **Contract Monad** (for querying the blockchain)
and **Constraints Library** (for building the transaction), the OnChain module for
including the validators in the transactions, and Business logic for the core
computations.
Finally, the web service is implemented on the **Main** module, using the plutus-apps
tool **PAB**. It basically interprets the OffChain code, exposing the dApps endpoints
and running the Contract monad for each operation.


Client Side
-----------

In our approach, the core part of a dApp is implemented on the Server, but
important parts are done on Client side. The end user interacts with the dApp
web frontend for triggering the operations. The critical work of building
the unbalanced transaction is done on the server and then we balance, sign
and submit on the browser.
For implementing the web frontend we present the **PAB Client** library which provides an easy
interface for interacting with PAB, Budget service and light wallets. 

.. figure:: /img/pab-client-architecture.png


The dApp developer implements the **dApp frontend** using a set of libraries.
**Contract endpoints** provides an API for abstracting the connection with PAB, which
uses a lower level module called **PAB API**. Their main purpose consists on obtaining
the dApp state and unbalanced transactions to be submitted.
**CIP30Wallet Wrapper** provides an API for easily connecting with CIP30 wallets and
obtaining utxos information, signing and submitting transactions.
Balancing algorithm is implemented on **Balancer** module. Both CIP30Wallet Wrapper
and Balancer depends on the external **Serialization Library**. 
Finally, **Budget API** provides an API for connecting to the Budget service.


