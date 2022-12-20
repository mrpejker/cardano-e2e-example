Cardano dApp Architecture
=========================

In order to introduce the dApp architecture we propose, let's quickly 
remind what a smart contract or dApp is on Cardano.
Unlike EVM blockchains, where a smart contract is a program that runs on-chain,
the unique piece of code running on-chain in Cardano is a **validator**. This is just
a boolean function deciding if a submitted transaction must be accepted or not.
The state of a dApp is stored in one or more utxos, and
the way of updating it is through submitting transactions. Here comes the off-chain
code, which is in charge of building, balancing, signing and submitting transactions.

For implementing the off-chain side of a dApp, we propose a Client-Server architecture.
On the Server side, we run an http web-server which provides an API
for connecting with the dApp operations. Each operation performs the
necessary logic for reading the state on the blockchain and building an unbalanced transaction.
On the Client side, a web frontend connects to the server for getting
unbalanced transactions and then balancing, signing and sumbitting them,
completing the dApp flow.



.. figure:: /img/architecture.png



Server Side
-----------


