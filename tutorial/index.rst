=============================================
Tutorial on building end-to-end Cardano dApps
=============================================

This tutorial covers the complete end-to-end flow for implementing a decentralized
Application (dApp) on Cardano Blockchain.
At the moment of writing it, there is no standard way to fully implement
an end-user Cardano dApp. Different projects have developed their own tools and proposed
different approaches. The proposal presented here consists of a Client-Server architecture,
where the Server side is based on the open-source libraries **plutus-apps**, developed by IOG,
and the Client side is a web frontend using the **PAB Client** typescript
library, which provides the necessary functionality for connecting with the
Plutus Application Backend to receive unbalanced transactions, and then
balance, sign, and submit them.

The text is based on a concrete implementation, a dApp example whose purpose is
to have something simple but containing enough functionality to show how to
use the proposed tools.
It's important to note that this tutorial doesn't pretend to cover
how to design complex solutions on the eUTxO model. The example is very simple and some
design decisions are related to the specific purpose of including common features of
any interesting dApp: parameterized validators, minting policies, how to read and write
datums, among others. 

It's assumed that the reader is familiar with the eUTxO model, Haskell
programming language and Plutus validators (for example, they already completed
the Plutus Pioneer Program).

The structure is as follows:
in *Preliminaries* section we introduce the example to implement, a detailed explanation of
the proposed architecture, and instructions for building and running the example dApp.
Then we go into details of the *Server side*, covering the main aspects
of the Haskell implementation, introducing the proposed modules design, going through
each component, and explaining the main implementation aspects.
Finally, we review the *Client side* by introducing the PAB Client library and how to use it.



Preliminaries
==============

.. toctree::
   :maxdepth: 2
   :numbered:

   preliminaries/the_example.rst
   preliminaries/dapp_architecture.rst
   preliminaries/setup_and_run.rst

Server side
===========

.. toctree::
   :maxdepth: 2
   :numbered:

   server_side/modules_design.rst
   server_side/business_logic.rst
   server_side/off_chain.rst
   server_side/on_chain.rst
   server_side/pab.rst
   server_side/testing.rst

Client side
===========
.. toctree::
   :maxdepth: 2
   :numbered:
