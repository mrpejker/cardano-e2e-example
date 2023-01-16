# Simple Escrow dApp Design

## Overview

The Simple Escrow dApp will be implemented in the Cardano blockchain using Plutus Scripts.

Each time a user starts an escrow, a new script UTxO is created, containing in the datum the information of this particular instance: payment details together with the sender addresses. A special *control Token* is minted each time a new escrow instance is started, ensuring that the produced UTxO is well-formed and the information is right. The Asset Class of this Token is also stored in the datum to check it is burned when the escrow is cancelled or resolved.

The script is parameterized by the receiver address.  This allows each user to find all escrows they need to resolve more quickly.

The *control Token* Minting Policy is parameterized by the contract address.

When an escrow instance is canceled or resolved, the corresponding UTxO is spent, and funds go to the corresponding wallet addresses. The control Token is burned.

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

- If the token A is **ADA**, the initial amount of **ADA** of the Sender's input value will be `N + K1`. <br>
The Script's output will have `minAda + k1` amount of **ADA**

### **Cancel**

The user can cancel the escrow and receive the locked tokens back. The control Token is burned.

![cancelEscrow diagram](img/cancelEscrow.png)

- If the token A is **ADA**, the amount of Ada in the Sender's Wallet input will be `N + K1` and in the output will be `N + minAda + K1 + k1` **ADA**. <br>
The amount of **ADA** blocked in the Script's input is `minAda + K1`

### **Resolve**

The other user pays `k2` Token `B`, closing the script and burning the control token. Each user gets the corresponding tokens.

![resolveEscrow diagram](img/resolveEscrow.png)

- If the token A is **ADA**, the Script's input value will be `minAda + k1` **ADA**<br>
The Receiver's input value will have `N + K1` **ADA** and the amount of **ADA** on the Receiver's output value will be `N + K1 + k1`. <br>
- In case of the token B is **ADA**, the initial amount of **ADA** in the Receiver's input will be `N + K2` and the output value of the Receiver's Wallet will be `N + K2 - k2` **ADA**. <br>
The output value of the Sender's Wallet will have `minAda + k2` **ADA**.

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

The token minting policy is parametrized by the contract address and has the following checks:

**Minting:**

- Only one token with the correct token name is minted
- The token is paid to the contract address
- The sender’s address is signing the transaction
- The token being minted is the correct control token
- The amount of tokens that the receiver wants to offer is more than 0

**Burning:**

- One token is being burned
