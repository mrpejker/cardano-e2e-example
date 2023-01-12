import type { TxOutRef, Plutus, WalletAddress, Address } from "cardano-pab-client";

export type StartParams = {
  receiverAddress: WalletAddress
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
  const { Address, AssetClass, succeeded } = await import("cardano-pab-client");
  const result = await Address.fromBech32(rAdd)
  if (succeeded(result)) {
    const receiverAddress = result.value.toWalletAddress();
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

export type ReceiverAddress = { rAddr: WalletAddress }

export function mkReceiverAddress(wAdd: Address): ReceiverAddress {
  return { rAddr: wAdd.toWalletAddress() }
}

export type CancelParams = {
  cpReceiverAddress: WalletAddress
  cpTxOutRef: Plutus.TxOutRef
}

export async function mkCancelParams(rAdd: string, ref: string): Promise<CancelParams> {
  const { Address, TxOutRef, succeeded } = await import("cardano-pab-client");
  const [txId, idx]: string[] = ref.split("#");
  const result = await Address.fromBech32(rAdd)
  if (succeeded(result)) {
    const cpReceiverAddress = result.value.toWalletAddress();
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
