# **Endpoints Specification**

## **Activation**

We only have one kind of activation. The activation allows the Wallet to interact with the endpoints defined in the [**EscrowSchema**](https://github.com/joinplank/cardano-e2e-example/blob/95a0dbbdfb812971c466516a3e525d95432b0b63/backend/src/Escrow/OffChain/Interface.hs#L41-L44). On this particular case, because we only have one kind of activation we just give the wallet address through caID. Besides that, the activate endpoint needs a wallet id to work, but the good news is that for our approach can be any.

In the body we give the following information:

 ```
{
   "caID": WalletAddress,
   "caWallet":{
      "getWalletId":0000000000000000000000000000000000000000
   }
}
 ```
A WalletAddress needs a `PubKeyHash` for the Payment PubKeyHash, and the StakingHash that may not be present. All the WalletAddress are defined as:
 ```
  {
    "waPayment": { "getPubKeyHash": string },
    "waStaking": { "getPubKeyHash": string }
  }
 ```

## **Operations**

### **Start**

The `start` endpoint creates a new escrow from the information given on the body of the call, with the following format. The `StartParams` JSON should be:

 ```
{
    "receiverAddress": WalletAddress,
    "receiveAssetClass": AssetClass,
    "receiveAmount": number,
    "sendAssetClass": AssetClass,
    "sendAmount": number
}
 ```

The **AssetClasses** have a `TokenName` and a `CurrencySymbol`. It uses the derived `AssetClass` JSON interpretation:
```
{
    "unAssetClass": [
        { "unCurrencySymbol": string },
        { "unTokenName": string }
    ]
}
```

### **Cancel**

The `cancel` endpoint cancels an existing escrow with the parameters needed to find the escrow. That is the transaction utxo reference of the escrow to cancel and the Receiver Address. The `CancelParams` JSON in the body should be:

 ```
{
    "cpReceiverAddress": WalletAddress,
    "cpTxOutRef": TxOutRef
}
 ```

A **TxOutRef** takes a `TxId` and a number.
 ```
  {
    "txOutRefId": { "getTxId" : string },
    "txOutRefIdx": number
  }
 ```

### **Resolve**

The `resolve` endpoint resolves an existing escrow. As the signer of this endpoint call in the OffChain code is the receiver, it is not necessary to give the Receiver Address as argument to find the escrow. The `ResolveParams` in the body is a JSON should be:

 ```
{
    rpTxOutRef :: TxOutRef
}
 ```

## **Observable State**

The **Observable State** can be loaded with the `reload` endpoint. The type of response from calling `status` is **UtxoEscrowInfo**:

 ```
{
    "escrowUtxo": TxOutRef,
    "escrowInfo": EscrowInfo,
    "escrowPayment": (AssetClass, number)
}
 ```

The **EscrowInfo** stores most of the information from the escrow:
 ```
{
    "sender": WalletAddress,
    "rAmount": number,
    "rAssetClass": AssetClass
}
 ```
