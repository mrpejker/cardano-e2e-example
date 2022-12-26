
export type AssetClass = { unAssetClass: [CurrencySymbol, TokenName] };

export function mkAssetClass(cs: string, tn: string): AssetClass {
  return {
    unAssetClass: [
      { unCurrencySymbol: cs },
      { unTokenName: tn }
    ]
  }
}

export function mkAssetClassFromAc(ac: string): AssetClass {
  return {
    unAssetClass: [
      { unCurrencySymbol: ac.substring(0, 56) },
      { unTokenName: ac.substring(56) }
    ]
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

export async function mkStartParams(rAdd: string, sAsset: string, sAm: Number,
  rAsset: string, rAm: Number): Promise<StartParams> {
  const wAdd = await bech32AddressToWalletAddress(rAdd)
  return {
    receiverAddress: wAdd,
    sendAssetClass: mkAssetClassFromAc(sAsset),
    sendAmount: sAm,
    receiveAssetClass: mkAssetClassFromAc(rAsset),
    receiveAmount: rAm,
  }

}