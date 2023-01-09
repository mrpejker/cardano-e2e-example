The PAB API
===========

This module provides functionality for connecting to a PAB running at the server side.
It is basically a thin wrapper for doing HTTP requests to the PAB.

We can use this module directly to have a low-level interaction with the PAB.
However, in our end-to-end example we prefer to use contractEndpoints, a more abstract connection to the PAB built on top of the PAB API module (see next section ...).

Currently, only the most important PAB operations are supported, but they are all that we need for running a full featured dApp:

- activate: To activate a new contract instance.

- endpoint: To call an endpoint of a currently active contract instance.

- status: To query the status of a currently active contract instance.

- stop: To stop an active contract instance.

In the following subsections we describe how to use the PAB API.


Create a PAB API instance
-------------------------

To create a PAB API instance we must just pass the PAB base URL:

.. code-block:: typescript

    const pabApi = new PABApi('http://localhost:9080/api');


Activate a contract instance
-----------------------------

There are two ways to activate an instance, depending on how the PAB handlers are configured.
If the PAB has a single handler, it is enough to just specify the activation parameters when calling the activation function. For instance:

.. code-block:: typescript

    const walletId = ???  // TODO
    const call = {
        params: ...
    }
    const contractId = await pabApi.activate(walletId, call);

The structure of the parameters depends on the handler definition, and how its serialization to JSON is defined.

For a PAB that has several handlers, an additional ID for the handler must be provided:

.. code-block:: typescript

    const walletId = ???  // TODO
    const call = {
        endpointTag: ... // handler ID
        params: ...  //
    }
    const contractId = await pabApi.activate(walletId, call);

If successful, the activate call will return a string with the contract instance ID. We will have to provide this ID to the instance specific calls we describe in the following sections.

When activating a contract instance, the PAB will execute specific logic defined in the Haskell off-chain code.
This logic can include doing queries, building and yielding transactions, logging information and updating the observable state.

However, the results of the execution are not immediately returned by the PAB.
Instead, they must be obtained by querying the instance status as described in :ref:`section 2.4 <pab_api-status>`.


Call an endpoint
----------------

To call an endpoint we must provide the contract ID, the endpoint name and the parameters.
The structure of the parameters depends on the endpoints definition (schema?),
and how the serialization to JSON of the involved types is defined.

For instance, in the e2e example we can call the "start" endpoint this way:

.. code-block:: typescript

    const call = {
        endpointTag: "start",
        params: ...  // something of type StartParams
    }
    await pabApi.endpoint(contractId, call);

As in activation, when calling an endpoint the PAB will execute specific off-chain logic.
The results of the endpoint execution are not immediately returned by the PAB.
They must be obtained by querying the instance status as described in the :ref:`next section <pab_api-status>`.


.. _pab_api-status:

Query the instance status
-------------------------

The status function retrieves the contract instance status from the PAB.
In the status a lot of useful information can be found, such as the logs, the observable state and the yielded transactions.

After activating an instance or calling an endpoint, the status will eventually reflect the results of the call.
As this is not immediate, it may be necessary to query the status several times until it is updated.

To query the status we must just provide the instance ID as follows:

.. code-block:: typescript

    const status = await this.pabApi.status(contractId);

If sucessful, the function returns an object of type `PABStatus <https://github.com/joinplank/cardano-pab-client/blob/7761589d993e81744ab49a84fe52cc88e7d9dfc1/src/common.ts#L100>`_, with all the information provided by the PAB.
Currently, the PAB status has the following fields:

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

In the status, the yielded transactions are accumulated in a list under the 'cicYieldedExportTxs' field.
When the off-chain code yields a transaction, it is added at the end of this list, together with complementary information useful for balancing.

Each entry in the list is of ExportTx type and has three fields:

- transaction: The CBOR of the unbalanced transaction in hexadecimal format.
- inputs: A list with information for each of the transaction input UTxOs included in the lookups ("unspentOutputs" lookup) (TODO: check this!). Each entry includes the following fields:
    - id: Transaction ID for the UTxO.
    - index: Output index for the UTxO.
    - address: Address that owns the UTxO.
    - amount: lovelace locked in the UTxO.
    - assets: other assets locked in the UTxO.
    - datum: if present, datum hash stored into the UTxO.
- redeemers: List of redeemers for the Plutus scripts that must be executed. Each entry has fields:
    - purpose: "spending" or "minting".
    - data: the redeemer data (passed to the validator)
    - input: only for "spending", the UTxO reference of the spent input.
    - policy_id: only for "minting", the policy ID of the minted asset.

.. code-block:: typescript

    export type ExportTxInput = {
        id: string // txId of the utxo
        index: number // index of the utxo
        address: string
        amount: Amount // of lovelace
        assets: Assets // others than lovelace
        datum: string | null
    };

    export type ExportTx = {
        transaction: string;
        inputs: ExportTxInput[];
        redeemers: ExportTxRedeemer[];
    };

    type SpendingRedeemer = {
        purpose: "spending";
        data: string;
        input: { id: string, index: number }
    };

    type MintingRedeemer = {
        purpose: "minting";
        data: string;
        policy_id: string;
    };

    export type ExportTxRedeemer = SpendingRedeemer | MintingRedeemer;


Observable state
~~~~~~~~~~~~~~~~

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
