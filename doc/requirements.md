# Escrow Requirements

## Overview

This Escrow contract allows users to exchange a pair of tokens. Each instance of the escrow is independent from the others and specifies which and how many tokens are to be exchanged, along with the pkh of both parties involved.

At the launch of the contract, **a user** locks some amount `k1` of `a` tokens in the **instance UTxO** and specifies the amount `k2` of `b` tokens they want to receive and from whom. Then the **other user** can consume the **instance UTxO** and claim the tokens `a` by paying `k2` tokens `b`.

The user that initially locked their funds can cancel the exchange and receive them back as long as the contract hasn’t been resolved.

An important observation is that every instance of a contract that a user creates, is parametrized by the same token. That means that a single user can have many instances in the same contract address.