The Balancer
============

This module is a key part of the PAB Client library.
Unbalanced transactions requires several modifications until they can be signed and submitted to the blockchain, and the balancer module takes care of this.

Moreover, the module is a full-featured general purpose Cardano balancer, so it can be used to balance transactions built in any possible way, not only with the PAB.

(including coin selection, budget calculation, minimal fee, etc.)

Currently, the balancer supports Cardano transactions up to the Alonzo era, including of course all Plutus-related features.
Next version will include support for Babbage era features.


Balancing process overview
--------------------------

Recall that in our approach, the PAB builds transactions that has only set those aspects that reflect the abstract logic of the operation we want to perform in our dApp.
All the bureaucratic aspects of the transaction are left to the balancer.

The main purpose of the balancer is to perform these two steps:

1. Properly balance the transaction, this is, add wallet inputs to pay the value that is not already yet covered by the inputs, and change outputs to pay back to the user wallet the leftover value.

2. Compute and set the transaction fee, and the memory and CPU budget for every redeemer. To do this, the balancer must have access to a **budget service**.

There is a circularity between these two goals, because step 1 requires to know the fee value computed in step 2, as the balancing condition is::

  total input value + minted value = total output value + burned value + fee

However, at the same time step 2 requires the execution of the Plutus scripts in the same conditions as it will be executed in the blockchain, including the inputs and outputs added in step 1.

Our workaround for this circularity is to perform step 1 using an **upper bound for the fee** that must be provided when calling the balancer.

After this, step 2 can be performed with a transaction that looks almost exactly like the one to be submitted, obtaining very precise budgets and therefore a very precise fee value.

(In fact, our balancer doesn't need to artificially increase the fee)

Last, as the fee obtained in step 2 will be smaller than the upper bound used in step 1, a small readjustment must be done in the ADA value of the change output, in what we call the **rebalancing step**.
This change is so small that it does not affect the execution budgets.


Instantiate the balancer
------------------------

To instantiate the balancer we must only provide the protocol parameters of the blockchain we are using.
The PAB Client library includes a function to query the protocol parameters using Blockfrost:

.. code-block:: typescript

    const { Balancer, getProtocolParamsFromBlockfrost } = await import("cardano-pab-client");

    // Initialize Balancer
    const protocolParams = await getProtocolParamsFromBlockfrost(
      blockfrostUrl,
      blockfrostApiKey,
    );
    const balancer = await Balancer.init(protocolParams);
