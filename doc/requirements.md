# Simple Escrow dApp Requirements

## Problem

Alice and Bob want to exchange tokens. Alice will pay `k1` amount of `A`
tokens and receive `k2` amount of `B` tokens from Bob. If Alice sends
the tokens directly to Bob’s wallet, then there is no certainty that she
will receive the corresponding payment. Alice should trust Bob after sending her
tokens.
A solution that doesn’t depend on trusting the users must be found.

## How to solve it

A third trusted party can be involved. Instead of paying directly to Bob’s wallet,
Alice sends her tokens to that third party, which is in charge of ensuring that
Bob will receive the tokens only if he makes the corresponding payment to Alice.
This third party shouldn’t be another user, otherwise,
we fall into the same problem: we need to trust some user.
Smart contracts are good for it.

## Technical Solution

The trusted party can be implemented using plutus scripts.
Two options can be considered:

**Option 1:** The two users send their tokens, and then retrieve the demanded ones.
On this idea, each entity should pay the tokens to a script and collect, when
corresponding, the required tokens.

**Option 2:**  In this idea, the user who starts the contract locks their tokens
in a script and waits for the other user to resolve it.
That means that the second user pays their tokens directly to the first one, and
receives the tokens locked in the script.
In the same transaction, both users receive their required tokens.

The main difference between the options is that Option 2 takes two transactions to
resolve and the other one takes four. We’ll follow option 2 in this project.

## Use cases

**Use case: Connecting to the escrows**

Context: Alice wants to interact with the Simple Escrow dApp and see which
escrows she is involved in as a receiver.

- Alice selects in the interface the Connect Wallet button that corresponds to
the wallet she wants to use.
- The dApp uses the address (obtained from the wallet) to search for the escrows
she is involved in.
- The dApp shows the user the lists of every escrow started by another user, where
the receiver is Alice.

**Use case: Starting a new escrow**

Context: Alice is connected to the Simple Escrow dApp (previous use case).

Alice wants to exchange `k1` amount of token `A` to user Bob with address `bobAddress`
by `k2` amount of  `B` tokens.

- Alice selects in the interface the option for creating a new escrow.
- dApp asks for the information:
    - Amount and asset class Alice wants to pay.
    - Amount and asset class Alice wants to receive.
    - Receiver’s address.
- Alice completes the information:
    - `k1`, `A`
    - `k2`, `B`
    - `bobAddress`
- dApp builds the `startEscrow` transaction using the information
(including Alice’s address, which is obtained at connection) and asks Alice to
sign it with her wallet.
- Alice signs the transaction.
- dApp submits the transaction that locks `k1` tokens `A`  from Alice's wallet
to the escrow script.

**Use case: Canceling an existing escrow**

Context: User Alice has started an escrow whose receiver is Bob, but she regrets
it and wants to receive her tokens back.

The corresponding escrow script that has the `bobAddress` as receiver is locking
`k1` amount of token `A` from Alice.

- Alice introduce the `bobAddress` to list the all the escrows where Bob is
the receiver and she the sender.
- The dApp uses that address to search for the those escrows.
- Alice selects from that list of escrows, the one she wants to cancel.
- dApp builds the `cancelEscrow` transaction and asks Alice to sign.
- Alice signs the transaction.
- dApp submits the transaction that pays back the `k1` amount of token `A` to
Alice.

**Use case: Resolving an escrow**

Context: Alice started an escrow with Bob as the receiver.

The escrow script has locked `k1` amounts of tokens `A` from Alice and the required
payment for resolving it is `k2` tokens `B` from Bob.

- Bob selects in the list of escrows to be resolved, the escrow he wants to resolve.
- dApp builds the `resolveEscrow` transaction and asks Bob to sign with his wallet.
- Bob signs the transaction.
- dApp submits the transaction paying `k1` amount of tokens `A` to Bob and `k2`
of token `B` to Alice.

## Other considerations

- The Simple Escrow dApp will accept any non-ADA cardano native tokens. ADA will
be excluded.
- When a user starts an escrow, a min amount of ADA (min-ADA) is paid to the script.
That amount of ADA is paid back when the escrow is canceled.
When the escrow is resolved, the receiver will get the sender’s payment plus
min-ADA, and will send the specified tokens plus min-ADA.
