import type { TxOutRef, Plutus, WalletAddress } from "cardano-pab-client";

export type StartParams = {
  receiverAddress: ReturnType<WalletAddress["toPAB"]>
  sendAssetClass: Plutus.AssetClass
  sendAmount: number
  receiveAssetClass: Plutus.AssetClass
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
    const receiverAddress = result.value.toPAB();
    const sendAssetClass = new AssetClass(sCurrency, sTokenN).toPlutusAssetClass();
    const receiveAssetClass = new AssetClass(rCurrency, rTokenN).toPlutusAssetClass();
    return {
      receiverAddress,
      sendAssetClass,
      sendAmount: sAm,
      receiveAssetClass,
      receiveAmount: rAm,
    }
  } else {
    throw new Error(result.error);
  }
}

export type ReceiverAddress = { rAddr: ReturnType<WalletAddress["toPAB"]> }

export function mkReceiverAddress(wAdd: WalletAddress): ReceiverAddress {
  return { rAddr: wAdd.toPAB() }
}

export type CancelParams = {
  cpReceiverAddress: ReturnType<WalletAddress["toPAB"]>
  cpTxOutRef: Plutus.TxOutRef
}

export async function mkCancelParams(rAdd: string, ref: string): Promise<CancelParams> {
  const { WalletAddress, TxOutRef, succeeded } = await import("cardano-pab-client");
  const [txId, idx]: string = ref.split["#"];
  const result = await WalletAddress.fromBech32Address(rAdd)
  if (succeeded(result)) {
    const cpReceiverAddress = result.value.toPAB();
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
