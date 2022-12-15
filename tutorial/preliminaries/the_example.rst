The Example - A Simple Escrow
=============================

The example we present in this tutorial is a well known problem where smart contracts
are a great solution. It's about exchanging tokens between two users. 

The Problem
-----------

Alice and Bob want to exchange tokens. Alice will pay ``k1`` amount of ``A``
tokens and receive ``k2`` amount of ``B`` tokens from Bob. If Alice sends
the tokens directly to Bob’s wallet, then there is no certainty that she
will receive the corresponding payment. Alice should trust Bob after sending her
tokens.
A solution that doesn’t depend on trusting the users must be found.

How to solve it
---------------

A third trusted party can be involved. Instead of paying directly to Bob’s wallet,
Alice locks her tokens in a *smart contract* and specifies what tokens and how many
of them she wants to receive.
The smart contract ensures that Bob will receive his tokens only if he pays
the corresponding to Alice.

From now on, we'll call the *sender* to the user that starts an escrow (Alice), and
the *receiver*, the user that ends an escrow by receiving the locked tokens
and sending back the corresponding payment to the sender (Bob).

We'll consider three operations:

- **Start**: the sender starts the escrow locking the funds and setting
  the information about the payment to be received.

  
- **Resolve**: the receiver gets the funds locked and pays the corresponding tokens
  to the sender, finishing the escrow.

  
- **Cancel**: the sender cancels the escrow getting back the locked tokens.


eUTxO-model-based solution
--------------------------

An easy solution can be implemented with Plutus scripts. The sender
pays the desired value to a script in which specifies all the necessary information:

- Receiver's address ``RA``
- Sender's address ``SA``.
- Asset Class of token to receive ``B``.
- Amount of tokens to receive ``k2``.

Thus, the validator can express that if a receiver wants to spend the script utxo,
the transaction must be signed by a wallet with address ``RA``, and it must include
a payment of ``k2`` tokens ``B`` to the address ``SA``.

  
There are several design options for storing the information in a script. We choose
to set the receiver's address as a *parameter* of the script, and the rest of the
escrow information in the *datum*.
This is an arbitrary decision given that all
the information could be located in the parameter (none of the operations need
to change it), as well in the datum. We just want to show in the example how to deal
with a parameterized validator and read/write datums. A consequence of this decision is
that escrows with same receiver address will share the same script address. 

Operations
~~~~~~~~~~

Start


.. figure:: /img/startEscrow.png


Resolve

.. figure:: /img/resolveEscrow.png


Cancel

.. figure:: /img/cancelEscrow.png
