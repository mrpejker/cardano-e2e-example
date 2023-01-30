Introduction
============

The client side provides the frontend for our dApp and all the pieces of
sofware required for the execution of the complete application flow. It relies
heavily on `Cardano PAB Client <https://github.com/joinplank/cardano-pab-client/>`_,
a library we developed for general purpose frontend Cardano/Plutus development.

The **PAB Client library** provides all the functionality to connect to the
server side services such as the PAB and the Budget service. Moreover, it
provides a full-featured general purpose Cardano balancer, that can be used to
balance transactions built in any possible way, not only with the PAB. It also
includes additional Cardano utilities, such as a wallet wrapper that provides
typing and additional functionality to the basic API defined in CIP 30.

In the following sections we describe how to use the different PAB Client
modules using the escrow example as a reference. We choose to structure the
presentantion following the logical order that requires performing a dApp
operation, as illustrated in the following diagram:

.. figure:: /img/frontendSequence.png

The flow starts with the connection to the PAB, that can be either done with
the low level module PAB API (:ref:`section 2 <pab_api>`), or with the high
level module ContractEndpoints built on top of it (:ref:`section 3
<contract_endpoints>`). As the PAB yields an unbalanced transaction, next step
is to balance it using the Balancer (:ref:`section 4 <balancer>`). Finally,
singing and submission of the final transaction is done using the wallet. The
complete flow is put together in the escrow frontend (:ref:`section 5
<frontend>`), where the User Interface calls the main Escrow module that
provides the full dApp functionality.
