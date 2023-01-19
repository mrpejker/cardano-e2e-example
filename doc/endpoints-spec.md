# **Endpoints Specification**

## **Activation**

The only kind of activation that we call *connect*, allows the Wallet to interact with the endpoints defined in the **EscrowSchema**. On this particular case, because we only have one kind of activation we just give the wallet address through caID. Besides that, the activate endpoint needs a wallet id to work, but the good news is that for our approach can be any. <br>
In the body we give the following information:

 ```
{
   "caID": WalletAddress,
   "caWallet":{
      "getWalletId":0000000000000000000000000000000000000000
   }
}
 ```
A WalletAddress needs a `PubKeyHash` for the Payment PubKeyHash, and the StakingHash can be `undefined`. All the WalletAddress are defined as:
 ```
  {
    "waPayment": { "getPubKeyHash": string };
    "waStaking": { "getPubKeyHash": string } | undefined;
  }
 ```
<br>

## **Operations**
<br>

### **Start**

The start operation starts a new escrow with the required parameters, hitting the <code>Start</code> endpoint.<br>
The **StartParams** is a JSON that holds all the necessary information to start a new Escrow. This JSON goes in the body of the request.

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

The cancel operation cancel an existing escrow, it is used hitting the <code>Cancel</code> endpoint.

The **CancelParams** that goes in the body is a JSON that use two parameters to find the escrow we want to cancel. That is the transaction reference where the escrow was started and the Receiver Address:

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

The resolve operation resolves an existing escrow, hitting the <code>Resolve</code> endpoint.

The **ResolveParams** in the body is a JSON that only has the transaction reference where the escrow we want to resolve was created. As the signer of the resolve operation in the OffChain code is the receiver, it is not necessary to give the Receiver Address as argument to find the escrow.


 ```
{
    rpTxOutRef :: TxOutRef
}
 ```
<br>

## **Observable State**
<br>

The **Observable State** can be requested with the <code>Reload</code> endpoint. It does not takes any parameters and the type of the response is what we call **UtxoEscrowInfo**.

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