import type { AssetClass, TxOutRef, Plutus, WalletAddress } from "cardano-pab-client";

export type StartParams = {
  receiverAddress: WalletAddress
  sendAssetClass: AssetClass
  sendAmount: number
  receiveAssetClass: AssetClass
  receiveAmount: number
}

export async function mkStartParams(
  rAdd: string,
  sCurrency: string,
  sTokenN: string,
  sAm: number,
  rCurrency: string,
  rTokenN: string,
  rAm: number,
): Promise<StartParams> {
  const { WalletAddress, AssetClass, succeeded } = await import("cardano-pab-client");
  const result = await WalletAddress.fromBech32Address(rAdd)
  if (succeeded(result)) {
    const wAdd = result.value;
    return {
      receiverAddress: wAdd,
      sendAssetClass: new AssetClass(sCurrency, sTokenN),
      sendAmount: sAm,
      receiveAssetClass: new AssetClass(rCurrency, rTokenN),
      receiveAmount: rAm,
    }
  } else {
    throw new Error(result.error);
  }
}

export type ReceiverAddress = { rAddr: WalletAddress }

export function mkReceiverAddress(wAdd: WalletAddress): ReceiverAddress {
  return { rAddr: wAdd }
}

export type CancelParams = {
  cpReceiverAddress: WalletAddress,
  cpTxOutRef: Plutus.TxOutRef
}

export async function mkCancelParams(rAdd: string, ref: string): Promise<CancelParams> {
  const { WalletAddress, TxOutRef, succeeded } = await import("cardano-pab-client");
  const [txId, idx]: string = ref.split["#"];
  const result = await WalletAddress.fromBech32Address(rAdd)
  if (succeeded(result)) {
    const cpReceiverAddress = result.value;
    const cpTxOutRef = new TxOutRef(txId, Number(idx)).toPlutusTxOutRef();
    return { cpTxOutRef, cpReceiverAddress };
  } else {
    throw new Error(result.error);
  }
}

export type ResolveParams = {
  rpTxOutRef: Plutus.TxOutRef
}

export function mkResolveParams(ref: TxOutRef): ResolveParams {
  return { rpTxOutRef: ref.toPlutusTxOutRef() };
}
