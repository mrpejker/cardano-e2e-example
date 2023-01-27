The PAB API
===========

This module provides functionality for connecting to a PAB running at the
server side. It is basically a thin wrapper for doing HTTP requests to the PAB.

We can use this module directly to have a low-level interaction with the PAB.
However, in our end-to-end example we prefer to use contractEndpoints, a more
abstract connection to the PAB built on top of the PAB API module (see
:ref:`next section <contract_endpoints>`).

Currently, only the most important PAB operations are supported, but they are
all that we need for running a full featured dApp:

* activate: To activate a new contract instance.
* endpoint: To call an endpoint of a currently active contract instance.
* status: To query the status of a currently active contract instance.
* stop: To stop an active contract instance.

In the following subsections we describe how to use the PAB API.


Create a PAB API instance
-------------------------

To create a PAB API instance we must just pass the PAB base URL:

.. code-block:: typescript

    const pabApi = new PABApi('http://localhost:9080/api');


.. _pab_api-activate:

Activate a contract instance
-----------------------------

There are two ways to activate an instance, depending on how the PAB handlers
are configured. If the PAB has a single handler, it is enough to just specify
the activation parameters when calling the activation function. For instance:

.. code-block:: typescript

    const call = {
        contents: ...
    }
    const contractId = await pabApi.activate(call);

The structure of the parameters depends on the handler definition, and how its
serialization to JSON is defined.

For a PAB that has several handlers, an additional ID for the handler must be
provided:

.. code-block:: typescript

    const call = {
        tag: ... // handler ID
        contents: ...
    }
    const contractId = await pabApi.activate(call);

If successful, the activate call will return a string with the contract
instance ID. We will have to provide this ID to the instance specific calls.

When activating a contract instance, the PAB will execute specific logic
defined in the Haskell off-chain code. This logic can include doing queries,
building and yielding transactions, logging information and updating the
observable state. However, the results of the execution are not immediately
returned by the PAB. Instead, they must be obtained by querying the instance
status as described in :ref:`section 2.4 <pab_api-status>`.


Call an endpoint
----------------

To call an endpoint we must provide the contract ID, the endpoint name and the
parameters. The structure of the parameters depends on the endpoints schema
definition, and how the serialization to JSON of the involved types is defined.

For instance, in the escrow example we can call the "resolve" endpoint this way:

.. code-block:: typescript

    await pabApi.endpoint(
        contractId,
        tag: "resolve",
        contents: ...,  // something of type ResolveParams
    );

As in activation, when calling an endpoint the PAB will execute specific
off-chain logic. The results of the endpoint execution are not immediately
returned by the PAB. They must be obtained by querying the instance status as
described in the :ref:`next section <pab_api-status>`.


.. _pab_api-status:

Query the instance status
-------------------------

The status function retrieves the contract instance status from the PAB. In the
status a lot of useful information can be found, such as the logs, the
observable state and the yielded transactions.

After activating an instance or calling an endpoint, the status will eventually
reflect the results of the call. As this is not immediate, it may be necessary
to query the status several times until it is updated.

To query the status we must just provide the instance ID as follows:

.. code-block:: typescript

    const status = await this.pabApi.status(contractId);

If sucessful, the function returns an object of type `PABStatus <https://github.com/joinplank/cardano-pab-client/blob/7761589d993e81744ab49a84fe52cc88e7d9dfc1/src/common.ts#L100>`_,
with all the information provided by the PAB. Currently, the PAB status has the
following fields:

.. code-block:: typescript

    export type PABStatus = {
        cicDefinition: {
            tag: string,
            contents: unknown
        },
        cicCurrentState: {
            hooks: Array<unknown>,
            observableState: unknown,
            logs: Array<PabLog>,
            err: unknown,
            lastLogs: Array<PabLog>,
        },
        cicYieldedExportTxs: Array<ExportTx>,
        cicContract: { unContractInstanceId: string },
        cicStatus: string,
        cicWallet: {
            prettyWalletName: string,
            getWalletId: string,
        }
    };

In the following subsections we describe two important fields of the status.


Yielded transactions
~~~~~~~~~~~~~~~~~~~~

In the status, the yielded transactions are accumulated in a list under the
``cicYieldedExportTxs`` field. When the off-chain code yields a transaction, it
is added at the end of this list, together with complementary information
useful for balancing.

Each entry in the list is of ``ExportTx`` type and has three fields:

* ``transaction``: The CBOR of the unbalanced transaction in hexadecimal format.
* ``inputs``: A list with information for each of the transaction input UTxOs
  included in the lookups (``unspentOutputs`` lookup) (TODO: check this!). Each
  entry includes the following fields:

    * ``id``: Transaction ID for the UTxO.
    * ``index``: Output index for the UTxO.
    * ``address``: Address that owns the UTxO.
    * ``amount``: Lovelace locked in the UTxO.
    * ``assets``: Other assets locked in the UTxO.
    * ``datum``: If present, datum hash stored into the UTxO.

* ``redeemers``: List of redeemers for the Plutus scripts that must be
  executed. Each entry has fields:

    * ``purpose``: "spending" or "minting".
    * ``data``: The redeemer data (passed to the validator)
    * ``input``: Only for "spending", the UTxO reference of the spent input.
    * ``policy_id``: Only for "minting", the policy ID of the minted asset.


.. _pab_api-exporttx:

Observable state
~~~~~~~~~~~~~~~~

In the status, the observable state is included in the ``cicCurrentState``
field, ``observableState`` subfield. The JSON structure of the observable state
depends on the particular dApp and its off-chain code.

In the case of the escrow example, the observable state has the following
structure:

.. code-block:: typescript

    type PABObservableState = Array<{
      escrowUtxo: Plutus.TxOutRef,
      escrowValue: Plutus.Value,
      escrowInfo: {
        sender: ReturnType<WalletAddress["toPAB"]>;
        rAssetClass: Plutus.AssetClass;
        rAmount: number;
      }
    }>;

We can see here that the observable state is a list that contains all the
escrows that can be resolved. This information is useful to display in the UI
but also to determine the parameters that must be passed to ``resolve``.
