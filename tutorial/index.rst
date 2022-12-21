=============================================
Tutorial on building end-to-end Cardano dApps
=============================================

Introduction
==============

Preliminaries
-------------
.. toctree::
   :maxdepth: 2
   :numbered:

   preliminaries/the_example.rst
   preliminaries/dapp_architecture.rst
   preliminaries/setup_and_run.rst

Server side
===========

The complete implementation is organized as a cabal project composed of a library,
where the interesting dApp implementation is placed, a tests-suite and finally
some mostly boilerplate implementation for building the executable that will run
the service. This service, which we call the *PAB*, will be in charge of querying
the blockchain and building unbalanced transactions.

.. toctree::
   :maxdepth: 2
   :numbered:

   server_side/modules_design.rst
   server_side/business_logic.rst
   server_side/off_chain.rst
   server_side/on_chain.rst
   server_side/testing.rst
   server_side/pab.rst

Client side
===========
.. toctree::
   :maxdepth: 2
   :numbered:
