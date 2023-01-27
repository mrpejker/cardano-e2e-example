.. _contract_endpoints:


The Contract Endpoints module
=============================

This module provides a more abstract way to connect to the PAB, following
design patterns we find generally useful for dApps. It is built on top of the
PAB API module.

To be able to use this module, the off-chain code must also follow the design
patterns.


Create a ContractEndpoints instance
-----------------------------------

The creation of a ``ContractEndpoints`` instance implies the activation of a
contract instance in the PAB. We implement two patterns to do this.

First, a basic pattern is just to activate a contract instance with no other
consequence. This is done using the ``connect`` constructor. For instance:

.. code-block:: typescript

    const pabUrl = 'http://localhost:9080/api';
    const endpoints = await ContractEndpoints.connect(
      pabUrl,
      call: {
        tag: ... ,      // string (optional)
        contents: ...,
      },
    );

Here, ``pabUrl`` is the PAB URL, and ``call`` follows the same structure as the
one used in the :ref:`activate <pab_api-activate>` method of PAB API.

A second pattern is to activate a contract instance where the off-chain
activation code also yields a transaction. This pattern can be used to create
script UTxOs in the blockchain together with the contract instantiation in the
PAB. It is implemented in the ``start`` constructor.

For instance:

.. code-block:: typescript

    const pabUrl = 'http://localhost:9080/api';
    const endpoints, result = await ContractEndpoints.start(
      pabUrl,
      call: {
        tag: ... ,      // string (optional)
        contents: ...,
      },
    );
    if (failed(result)) {
      ...  // here take a look at result.error
    }
    const tx = result.value;  // of type ExportTx

The call is very similar to ``connect``, but it returns not only the endpoints
instance but also the resulting transaction wrapped into the result object.
Inside ``start``, the transaction is obtained by polling the PAB status until
the yielded transaction shows up, as in ``doOperation`` described below.

The transaction comes together with complementary information in an object of
type ``ExportTx``, as explained in :ref:`section 2.4.1 <pab_api-exporttx>`.


Perform an operation
--------------------

The main pattern we implement is the process of getting an unbalanced
transaction from the PAB, that requires calling and endpoint and then polling
the PAB status until the yielded transaction shows up.

For instance, in the escrow example we can call the “resolve” endpoint this way:

.. code-block:: typescript

    const result = await this.endpoints.doOperation(
      {
        tag: "resolve",  // string
        contents: ...,   // something of type ResolveParams
      }
    );
    if (failed(result)) {
      ...  // here take a look at result.error
    }
    const tx = result.value;  // of type ExportTx

The method returns an object of type ``Result<ExportTx>``. The utility type
``Result`` implements a design pattern for operations that can succeed or fail
without using exceptions. If the call is successful, the transaction together
with its complementary information can be found in the ``value`` attribute, in
an object of type ``ExportTx``, as explained in :ref:`section 2.4.1 <pab_api-exporttx>`.

Internally, ``doOperation`` is polling the PAB status until a new transaction
shows up in the ``cicYieldedExportTxs`` field. Alternatively, the polling will
stop if an error is logged into the ``cicCurrentState.logs`` list or in the
``cicCurrentState.err`` field of the PAB status.

Therefore, to avoid infinite polling, **the Haskell off-chain code for the
endpoint must be programmed accordingly**, by always either yelding a
transaction or logging an error. To log an error, the ``logError`` function can
be used.


Reload the observable state
---------------------------

Another important pattern we implement is the definition of endpoints that only
update the observable state in the PAB status, with no transaction yielding.
These endpoints can be used for performing blockchain queries and obtaining
useful information for the frontend.

.. code-block:: typescript

    const result = await this.endpoints.reload(
      {
        tag: "reload",
        contents: [],
      }
    );
    if (failed(response)) {
      ...  // here take a look at result.error
    }
    const escrows = response.value as PABObservableState;

In this pattern, it is also required to poll the PAB status after calling the
endpoint. To be able to tell that the observable state has been updated, we
include an integer flag in it. The expected value is passed to the backend, and
the stopping condition for polling is that this value shows up in the PAB
status.

For the pattern to work, **the Haskell off-chain code for the endpoint must be
programmed accordingly**, by taking this integer as a parameter and setting it
into the observable state.
