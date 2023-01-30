Balancer
========

This module is a key part of the PAB Client library. Unbalanced transactions
requires several modifications until they can be signed and submitted to the
blockchain, and the balancer module takes care of this.

Moreover, the module is a full-featured general purpose Cardano balancer, so it
can be used to balance transactions built in any possible way, not only with
the PAB.

Currently, the balancer supports Cardano transactions up to the Alonzo era,
including of course all Plutus-related features. Next version will include
support for Babbage era features.


Balancing process overview
--------------------------

Recall that in our approach, the PAB builds transactions that has only set
those aspects that reflect the abstract logic of the operation we want to
perform in our dApp. All the bureaucratic aspects of the transaction are left
to the balancer.

The main purpose of the balancer is to perform these two steps:

1. Properly balance the transaction, this is, add wallet inputs to pay the
value that is not already yet covered by the inputs, and change outputs to pay
back to the user wallet the leftover value.

2. Compute and set the transaction fee, and the memory and CPU budget for every
redeemer. To do this, the balancer must have access to a **budget service**.

However, there is a circularity between these two goals, because step 1
requires to know the fee value computed in step 2, as the balancing condition
is::

  total input value + minted value = total output value + burned value + fee

At the same time step 2 requires the execution of the Plutus scripts in the
same conditions as it will be executed in the blockchain, including the inputs
and outputs added in step 1.

Our workaround for this circularity is to perform step 1 using an **upper bound
for the fee** that must be provided when calling the balancer.

After this, step 2 can be performed with a transaction that looks almost
exactly like the one to be submitted, obtaining very precise budgets and
therefore a very precise fee value.

Last, as the fee obtained in step 2 will be smaller than the upper bound used
in step 1, a small readjustment must be done in the ADA value of the change
output, in what we call the **rebalancing step**. This change is so small that
it does not affect the execution budgets.

Besides these main goals, the balancer must also take care of other minor
aspects of the transaction, all related to compliance with the `Cardano
specification <https://github.com/input-output-hk/cardano-ledger>`_:

* Add the collateral input.
* In the redeemers list, readjust the "spend" indexes to match the new input
  list.
* Recompute the ``script_data_hash`` field according to the new witness set.
* Optionally, merge all outputs paid to the change address.


Example: An escrow resolve
--------------------------

In the escrow example, the resolve operation creates a transaction that
consumes a script UTxO that corresponds to an escrow started by a sender. To
resolve it, the transactions pays to the receiver the tokens locked in the UTxO
to the sender the tokens he expects,

The following diagram illustrates the unbalanced transaction provided by the
PAB after a call to the resolve endpoint:

.. figure:: /img/unbalancedResolve.png

Here, it can be seen that the tokens paid to the sender are not yet present in
any input of the transaction. It is work of the balancer to search in the
receiver's wallet for the UTxOs that can cover this payment, and add them as
inputs.

A possible result of the balancing process can be the transaction illustrated
by the following diagram:

.. figure:: /img/balancedResolve.png

In this case, the balancer was lucky enough to find all the required tokens to
be paid in a single wallet UTxO, so it was only necessary to add one input.
Also a new output was added, the change output, used to pay back to the wallet
the leftover funds.


Helper modules
--------------

As balancing requires a lot of contextual information, it is easier to use the
balancer in connection with other modules provided by the PAB Client library,
such as ``ContractEndpoints`` (described in the previous section),
``CIP30WalletWrapper`` and ``TxBudgetAPI``.

The ``CIP30WalletWrapper`` modules provides typing and additional functionality
to the basic wallet javascript API defined in `CIP 30
<https://cips.cardano.org/cips/cip30/>`_.

To instantiate a ``CIP30WalletWrapper``, we must first obtain the CIP 30 wallet
from the browser and enable it. The ``getWalletInitialAPI`` function can help
with this. For instance, we can load the Eternl wallet and wrap it the
following way:

.. code-block:: typescript

    const { getWalletInitialAPI } = await import("cardano-pab-client");

    const walletName = "eternl";
    const walletInitialAPI = getWalletInitialAPI(window, walletName);
    const walletInjectedFromBrowser = await walletInitialAPI.enable();
    const wallet = await CIP30WalletWrapper.init(walletInjectedFromBrowser);

The ``TxBudgetAPI`` is an API to access the `budget service
<https://github.com/joinplank/plutus-budget-service/>`_.
We can instantiate it as follows:

.. code-block:: typescript

    const { TxBudgetAPI } = await import("cardano-pab-client");

    const budgetUrl = "http://localhost:3001"
    const txBudgetApi = new TxBudgetAPI({
      baseUrl: budgetUrl,
      timeout: 10000,
    })


Instantiate the balancer
------------------------

To instantiate the balancer we must only provide the protocol parameters of the
blockchain we are using. The PAB Client library includes a function to query
the protocol parameters using Blockfrost:

.. code-block:: typescript

    const { Balancer, getProtocolParamsFromBlockfrost } = await import("cardano-pab-client");

    // Initialize Balancer
    const protocolParams = await getProtocolParamsFromBlockfrost(
      blockfrostUrl,
      blockfrostApiKey,
    );
    const balancer = await Balancer.init(protocolParams);


Call the balancer
-----------------

The easiest way to use the balancer is by calling the ``fullBalanceTx``
function, that takes care of the whole work following the previously described
process.

To call ``fullBalanceTx``, we must provide the following parameters:

.. code-block:: typescript

  public async fullBalanceTx(
    { transaction, inputs },
    { utxos, collateral, changeAddress },
    { feeUpperBound, mergeSignerOutputs, changeOutputIndex },
    exUnitsEvaluator,
  )

As you can see, the parameters are logically grouped, and the grouping also
makes it easy to obtain them from the helper modules:

* Transaction related (provided by ``ContractEndpoints``):

  * ``transaction``: The unbalanced transaction (a string with the CBOR in
    hexadecimal format).
  * ``inputs``: The information about the script inputs.

* Wallet related (provided by ``CIP30WalletWrapper``):

  * ``utxos``: Wallet UTxOs that can be selected for payment.
  * ``collateral``: Collateral UTxO.
  * ``changeAddress``: Change address.

* Balancing settings:

  * ``feeUpperBound``: Fee upper bound in lovelace.
  * ``mergeSignerOutputs``, ``changeOutputIndex``: Other optional parameters.

* ``exUnitsEvaluator``: Connector to the budget service (provided by
  ``TxBudgetAPI``).

For example, if we have correctly instantiated ``contractEndpoints``,
``wallet`` and ``txBudgetApi``, we can obtain a transaction from the PAB and
fully balance it the following way:

.. code-block:: typescript

    const pabResponse = await contractEndpoints.doOperation(...);
    const walletInfo = await wallet.getWalletInfo();
    const balancerResult = await balancer.fullBalanceTx(
      pabResponse.value,
      walletInfo,
      { feeUpperBound: 1000000 },
      txBudgetApi
    );
    if (failed(balancerResult)) {
      ...  // here take a look at balancerResult.error
    }
    const balancedTx = balancerResult.value;

The balancer returns an object of type ``Result<string>``. The utility type
``Result`` implements a design pattern for operations that can succeed or fail
without using exceptions. If the call is successful, the balanced transaction
can be found in the ``value`` attribute.


Other uses
----------

For the escrow example, `fullBalanceTx` is good enough to cover all our needs.
However, in some cases more flexibility may be needed. For instance, a possible
balancing approach is to used hardcoded values for memory and CPU budget,
removing the need for the budget service. In this case, the following lower
level functions of the balancer library can be used:

* balanceTx
* setExecutionUnits
* rebalanceTx
