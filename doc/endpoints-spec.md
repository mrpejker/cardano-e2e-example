# **Endpoints Specification**

## **Instance Activation**

The only kind of activation that we call *connect*, allows the Wallet to interact with the endpoints defined in the **EscrowSchema**. To activate the instance, we use the PAB fixed endpoint <code> api/contract/activate</code>. In the body we give the following information:

 ```
{
   "caID": WalletAddress,
   "caWallet":{
      "getWalletId":0000000000000000000000000000000000000000
   }
}
 ```

Where:

 - A WalletAddress needs a `PubKeyHash` for the Payment PubKeyHash, and the StakingHash can be `undefined`. All the WalletAddress are defined as:
 ```
  {
  waPayment: { getPubKeyHash: string };
  waStaking: { getPubKeyHash: string } | undefined;
  }
 ```

This PAB endpoint returns the contract **instance-id** that it is used to call the Escrow endpoints:

 ```
{"unContractInstanceId":"031f234d-69f5-4252-1586-1bd697faf1d5"}
 ```
<br>

## **Escrow Endpoints**

For calling the OffChain operations we use the PAB endpoint <code> api/contract/instance/[instance-id]/endpoint/[off-chain operation] </code>

### **Start**

The PAB endpoint call is <code> api/contract/instance/[instance-id]/endpoint/start</code> <br>
The start operation starts a new escrow with the required parameters, hitting the <code>Start</code> endpoint.

 ```
{ tag: "Start", params: startParams }
 ```

Where:

- startParams is a JSON that holds all the necessary information to start a new Escrow.

 ```
{
    receiverAddress : WalletAddress,
    receiveAssetClass: AssetClass,
    receiveAmount: number,
    sendAssetClass: AssetClass,
    sendAmount: number
}
 ```

- The AssetClasses have a `TokenName` and a `CurrencySymbol`. It uses the derived `AssetClass` JSON interpretation:
```
{ "unAssetClass": [
    {"unCurrencySymbol": string},
    {"unTokenName": string}
  ]
}
```

### **Cancel**

The PAB endpoint call is <code> api/contract/instance/[instance-id]/endpoint/cancel</code> <br>
The cancel operation cancel an existing escrow, it is used hitting the <code>Cancel</code> endpoint.

 ```
{ tag: "Cancel", params: cancelParams }
 ```

Where:

- cancelParams is a JSON that use two parameters to find the escrow we want to cancel. That is the transaction reference where the escrow was started and the Receiver Address:

 ```
{
    cpReceiverAddress: WalletAddress,
    cpTxOutRef: TxOutRef
}
 ```

A **TxOutRef** takes a `TxId` and a number.
 ```
  {
    txOutRefId: { getTxId : string },
    txOutRefIdx: number
  }
 ```

### **Resolve**

The PAB endpoint call is <code> api/contract/instance/[instance-id]/endpoint/resolve</code> <br>
The resolve operation resolves an existing escrow, hitting the <code>Resolve</code> endpoint.

 ```
{ tag: "Resolve", params: resolveParams}
 ```

Where:

- resolveParams is a JSON that only has the transaction reference where the escrow we want to resolve has been created.
As the signer of the resolve operation in the OffChain code is the receiver, it is not necessary to give the Receiver Address as argument to find the escrow.

 ```
{
    rpTxOutRef :: TxOutRef
}
 ```
