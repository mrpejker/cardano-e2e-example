# Simple Escrow dApp Design

## Overview

The Simple Escrow dApp will be implemented in the Cardano blockchain using Plutus Scripts.

Each time a user starts an escrow, a new script UTxO is created, containing in the datum the information of this particular instance:
payment details together with the sender addresses.
In order to ensure initial conditions of the escrow, a special *control Token* is minted at start.
This strategy follows the ideas described in [this article](https://well-typed.com/blog/2022/08/plutus-initial-conditions/)
from Well-Typed, where the minted NFT is called the *state token*.

The script is parameterized by the receiver address.  This allows each user to find all escrows they need to resolve in an easy way.
The *control Token* Minting Policy is parameterized by the script address, which is needed for ensuring that
the token is paid to the right script utxo. Because of this dependency, it's not possible to include the control token
asset class in the script parameter, which would be desiderable. It's solved including the asset class in the datum.

When an escrow instance is canceled or resolved, the corresponding UTxO is spent, and funds go to the corresponding wallet addresses.
The control Token is burned.

## Script UTxO

### **Address**

The script is parameterized by the receiver address. So that each user can find the escrows they need to resolve.

### **Datum**

- Sender’s address
- The amount and asset class of the receiver’s payment
- Asset Class of the Control Token

### **Value**

- Control Token
- The sender’s tokens to exchange
- min-ADA

## Transactions

### **Start**

In this transaction, a user locks the tokens they want to exchange and specifies the tokens they want to receive and from whom. The control Token is minted.

![startEscrow diagram](img/startEscrow.png)

### **Cancel**

The user can cancel the escrow and receive the locked tokens back. The control Token is burned.

![cancelEscrow diagram](img/cancelEscrow.png)

### **Resolve**

The other user pays `k2` Token `B`, closing the script and burning the control token. Each user gets the corresponding tokens.

![resolveEscrow diagram](img/resolveEscrow.png)

An **important clarification** for the transactions is what happen when one of the token A or token B is ADA. Considering that the smallest unit of **ADA** is **Lovelace**, the Wallet funds or the Script locked value change in the following way: <br>
Every time we have **N** **ADAs** and **k1** **Token A** and A is **Lovelace**, then the total amount of **ADAs** will be **(N * 1_000_000) + k1 Lovelace**

## Validator Scripts

### **Script Validator**

Validates the transactions that involve spending the **script UTxO**: Cancel and Resolve.

In the **Cancel operation**, the validator checks:

- The address that is trying to cancel the escrow is the same as the Sender’s address
- The control token is burned after the transaction

In the **Resolve operation** the validator checks:

- The address that is trying to resolve is the same as the Receiver’s address
- The Sender’s address receives the tokens specified on the datum
- The control token is burned after the transaction

### **Control Token minting policy**

The token minting policy is parametrized by the script address and has the following checks:

**Minting:**

- Only one token with the correct token name is minted
- The token is paid to the script address
- The sender’s address is signing the transaction
- The token being minted is the correct control token
- The amount of tokens that the receiver wants to offer is more than 0

**Burning:**

- One token is being burned
