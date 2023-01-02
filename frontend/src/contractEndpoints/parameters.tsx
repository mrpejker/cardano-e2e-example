
export type AssetClass = { unAssetClass: [CurrencySymbol, TokenName] };

export function mkAssetClass(cs: string, tn: string): AssetClass {
  return {
    unAssetClass: [
      { unCurrencySymbol: cs },
      { unTokenName: tn }
    ]
  }
}

export type Value = {
    getValue: [ [ CurrencySymbol, [ [TokenName, number] ] ] ]
  }

const filterEscrowValue = (elem, index, array) => {
  const adaT = ''
  const ctokenT = 'controlToken'
  const tn = elem[1][0][0].unTokenName
  return tn !== adaT && tn !== ctokenT
  }

export function getValueAmount(val: Value): number {
  return val.getValue.filter(filterEscrowValue)[0][1][0][1]
}

export function getValueAsset(val: Value): string {
  return val.getValue.filter(filterEscrowValue)[0][1][0][0].unTokenName
}

export type TxId = { getTxId: string };

export type TxOutRef = { txOutRefId: TxId, txOutRefIdx: number }

export function mkTxOutRef(ref: TxId, idx: number): TxOutRef {
  return {
    txOutRefId: ref,
    txOutRefIdx: idx
  }
}

export type CurrencySymbol = { unCurrencySymbol: string };

export type TokenName = { unTokenName: string };

export type WalletAddress = {
  waPayment: { getPubKeyHash: string };
  waStaking: { getPubKeyHash: string } | undefined;
};

export function mkWalletAddress(ppkh: string, spkh?: string): WalletAddress {
  return {
    waPayment: { getPubKeyHash: ppkh },
    waStaking: spkh ? { getPubKeyHash: spkh } : undefined
  };
}

export function mkWalletAddressFromString(wa: string): WalletAddress {
  return {
    waPayment: { getPubKeyHash: wa.substring(2, 58) },
    waStaking: { getPubKeyHash: wa.substring(58) }
  };
}

export async function bech32AddressToWalletAddress(addr: string): Promise<WalletAddress> {
  const { SerLibLoader } = await import("cardano-pab-client");
  await SerLibLoader.load();
  const S = SerLibLoader.lib;
  const baseAddress = S.BaseAddress.from_address(S.Address.from_bech32(addr));
  if (!baseAddress) {
    throw new Error("Bad address");
  } else {
    const payment = baseAddress.payment_cred().to_keyhash()?.to_hex();
    const stake = baseAddress.stake_cred().to_keyhash()?.to_hex();
    return mkWalletAddress(payment, stake)
  }
}

export type ReceiverAddress = { rAddr: WalletAddress }

export function mkReceiverAddress(wAdd: WalletAddress): ReceiverAddress {
  return { rAddr: wAdd }
}

export type StartParams = {
  receiverAddress: WalletAddress
  sendAssetClass: AssetClass
  sendAmount: Number
  receiveAssetClass: AssetClass
  receiveAmount: Number
}

export async function mkStartParams(rAdd: string, sCurrency: string,
  sTokenN: string, sAm: Number, rCurrency: string, rTokenN: string,
  rAm: Number): Promise<StartParams> {
  const wAdd = await bech32AddressToWalletAddress(rAdd)
  return {
    receiverAddress: wAdd,
    sendAssetClass: mkAssetClass(sCurrency, sTokenN),
    sendAmount: sAm,
    receiveAssetClass: mkAssetClass(rCurrency, rTokenN),
    receiveAmount: rAm,
  }

}

export type CancelParams = {
  cpReceiverAddress: WalletAddress,
  cpTxOutRef: TxOutRef
}

export async function mkCancelParams(rAdd: string, ref: string): Promise<CancelParams> {
  const wAdd = await bech32AddressToWalletAddress(rAdd)
  return {
    cpTxOutRef: txOutRefFromString(ref),
    cpReceiverAddress : wAdd
  }
}

function txOutRefFromString(ref: string): TxOutRef {
  const txId: TxId = {getTxId: ref.substring(0,64)}
  const idx = Number(ref.substring(65))
  return mkTxOutRef(txId, idx)
}

export type ResolveParams = {
  rpTxOutRef: TxOutRef
}

export function mkResolveParams(ref: TxOutRef): ResolveParams {
  return {
    rpTxOutRef: ref
  }
}
