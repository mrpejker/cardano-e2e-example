import type {
  ContractEndpoints,
  CIP30WalletWrapper,
  TxBudgetAPI,
  Balancer,
  AssetClass,
  TxOutRef,
  WalletAddress,
  Plutus,
  Address
} from "cardano-pab-client";
import { CancelParams, ResolveParams, StartParams } from "./parameters";

/**
 * The representation of the contract Utxo state
 */
export type EscrowInfo = {
  sender: Address;
  rAssetClass: AssetClass;
  rAmount: number;
}

export type UtxoEscrowInfo = {
  escrowUtxo: TxOutRef
  escrowInfo: EscrowInfo
  escrowPayment: [AssetClass, Number]
}

export type ObsState = UtxoEscrowInfo[]

export class UserEndpoints {
  PABClient: typeof import("cardano-pab-client");
  contractState: ObsState = undefined
  endpoints: ContractEndpoints = undefined
  wallet: CIP30WalletWrapper = undefined
  txBudgetApi: TxBudgetAPI = undefined
  balancer: Balancer = undefined

  constructor(
    contractState: ObsState,
  ) {
    this.contractState = contractState
  }

  public async connect(walletName: "nami" | "eternl"): Promise<UserEndpoints | null> {
    // load PAB Client just once!
    this.PABClient = await import("cardano-pab-client");

    const { Balancer, CIP30WalletWrapper, ContractEndpoints, TxBudgetAPI,
      getWalletInitialAPI, getProtocolParamsFromBlockfrost,
    } = this.PABClient;

    const walletInitialAPI = getWalletInitialAPI(window, walletName);
    // this will ask the user to give to this dApp access to their wallet methods
    const walletInjectedFromBrowser = await walletInitialAPI.enable();

    // Initialize CIP30 wallet wrapper
    this.wallet = await CIP30WalletWrapper.init(walletInjectedFromBrowser);

    const walletAddress = await this.wallet.getWalletAddress();
    console.log(`Connected Address: ${JSON.stringify(walletAddress)}`);

    // Initialize ContractEndpoints
    this.endpoints = await ContractEndpoints.connect(
      process.env.REACT_APP_PAB_URL,
      { contents: walletAddress },
    );

    // Initialize tx budget service API
    this.txBudgetApi = new TxBudgetAPI({
      baseUrl: process.env.REACT_APP_BUDGET_URL,
      timeout: 10000,
    })

    // Initialize Balancer
    const protocolParams = await getProtocolParamsFromBlockfrost(
      process.env.REACT_APP_BLOCKFROST_URL,
      process.env.REACT_APP_BLOCKFROST_API_KEY,
    );
    this.balancer = await Balancer.init(protocolParams);

    return this;
  }

  public async start(sp: StartParams) {
    const { failed, setMetadataMessage } = this.PABClient;
    // Try to get unbalanced transaction from PAB
    const pabResponse = await this.endpoints.doOperation(
      { tag: "start", contents: sp }
    );
    if (failed(pabResponse)) {
      console.log(`Didn't get the unbalanced transaction from the PAB. Error: ${pabResponse.error}`);
      alert(`Didn't get the unbalanced transaction from the PAB. Error: ${pabResponse.error}`);
      return;
    }
    // the pab yielded the unbalanced transaction. balance, sign and submit it.
    const etx = pabResponse.value;
    console.log(`Unbalanced tx: ${JSON.stringify(etx)}`);

    // set a metadata message to the transaction
    etx.transaction = await setMetadataMessage(etx.transaction, "Start Escrow");

    const walletInfo = await this.wallet.getWalletInfo();
    // fully balance the transaction
    const balancerResult = await this.balancer.fullBalanceTx(
      etx,
      walletInfo,
      { feeUpperBound: 1000000, mergeSignerOutputs: false },
      this.txBudgetApi
    );
    if (failed(balancerResult)) {
      console.log(`Balancer failed with error: ${balancerResult.error}`);
      alert(`Balancer failed with error: ${balancerResult.error}`);
      return;
    }
    const balancedTx = balancerResult.value;
    // print to the console the fully balanced tx cbor for debugging purposes
    console.log(`Balanced tx: ${balancedTx}`);
    // now that the transaction is balanced, sign and submit it with the wallet
    const walletResponse = await this.wallet.signAndSubmit(balancedTx);
    if (failed(walletResponse)) {
      console.log(`Start failed when trying to submit it. Error: ${walletResponse.error}`);
      alert(`Start failed when trying to submit it. Error: ${walletResponse.error}`);
      return;
    }
    const txHash = walletResponse.value;
    console.log(`TX HASH: ${txHash}`)
    alert(`Start suceeded. Tx hash: ${txHash}`);
  }

  public async reload(): Promise<ObsState> {
    const { failed } = this.PABClient;
    const response = await this.endpoints.reload({ tag: "reload", contents: [] })
    if (failed(response)) {
      console.log(`Reload Failed. Error: ${response.error}`);
      alert(`Reload Failed. Error: ${response.error}`);
      return;
    }
    const escrows = response.value as PABObservableState;
    console.log(escrows)
    const observableState = parsePABObservableState(escrows);
    console.log("Finishing Reload");
    return observableState;
  }

  public async cancel(cp: CancelParams) {
    const { failed, setMetadataMessage } = this.PABClient;

    const pabResponse = await this.endpoints.doOperation(
      { tag: "cancel", contents: cp }
    );
    console.log(pabResponse)
    if (failed(pabResponse)) {
      console.log(`Didn't get the unbalanced transaction from the PAB. Error: ${pabResponse.error}`);
      alert(`Didn't get the unbalanced transaction from the PAB. Error: ${pabResponse.error}`);
      return;
    }
    const etx = pabResponse.value;
    console.log(`Unbalanced tx: ${JSON.stringify(etx)}`);

    etx.transaction = await setMetadataMessage(etx.transaction, "Cancel Escrow");

    const walletInfo = await this.wallet.getWalletInfo();
    const balancerResult = await this.balancer.fullBalanceTx(
      etx,
      walletInfo,
      { feeUpperBound: 1000000, mergeSignerOutputs: false },
      this.txBudgetApi
    );
    if (failed(balancerResult)) {
      console.log(`Balancer failed with error: ${balancerResult.error}`);
      alert(`Balancer failed with error: ${balancerResult.error}`);
      return;
    }
    const balancedTx = balancerResult.value;
    console.log(`Balanced tx: ${balancedTx}`);

    const walletResponse = await this.wallet.signAndSubmit(balancedTx);
    if (failed(walletResponse)) {
      console.log(`Start failed when trying to submit it. Error: ${walletResponse.error}`);
      alert(`Start failed when trying to submit it. Error: ${walletResponse.error}`);
      return;
    }
    const txHash = walletResponse.value;
    console.log(`TX HASH: ${txHash}`)
    alert(`Cancel suceeded. Tx hash: ${txHash}`);
  }

  public async resolve(rp: ResolveParams) {
    const { failed, setMetadataMessage } = this.PABClient;

    const pabResponse = await this.endpoints.doOperation(
      { tag: "resolve", contents: rp }
    );
    if (failed(pabResponse)) {
      console.log(`Didn't get the unbalanced transaction from the PAB. Error: ${pabResponse.error}`);
      alert(`Didn't get the unbalanced transaction from the PAB. Error: ${pabResponse.error}`);
      return;
    }
    const etx = pabResponse.value;
    console.log(`Unbalanced tx: ${JSON.stringify(etx)}`);

    etx.transaction = await setMetadataMessage(etx.transaction, "Resolve Escrow");

    const walletInfo = await this.wallet.getWalletInfo();
    const balancerResult = await this.balancer.fullBalanceTx(
      etx,
      walletInfo,
      { feeUpperBound: 1000000, mergeSignerOutputs: false },
      this.txBudgetApi
    );
    if (failed(balancerResult)) {
      console.log(`Balancer failed with error: ${balancerResult.error}`);
      alert(`Balancer failed with error: ${balancerResult.error}`);
      return;
    }
    const balancedTx = balancerResult.value;

    const walletResponse = await this.wallet.signAndSubmit(balancedTx);
    if (failed(walletResponse)) {
      console.log(`Start failed when trying to submit it. Error: ${walletResponse.error}`);
      alert(`Start failed when trying to submit it. Error: ${walletResponse.error}`);
      return;
    }
    const txHash = walletResponse.value;
    console.log(`TX HASH: ${txHash}`)
    alert(`Resolve suceeded. Tx hash: ${txHash}`);
  }
}

type PABObservableState = Array<{
  escrowUtxo: Plutus.TxOutRef,
  escrowPayment: [Plutus.AssetClass, Number],
  escrowInfo: {
    sender: WalletAddress;
    rAssetClass: Plutus.AssetClass;
    rAmount: number;
  }
}>;

async function parsePABObservableState(escrows: PABObservableState): Promise<ObsState> {
  const { AssetClass, TxOutRef, Address } = await import("cardano-pab-client");
  return escrows.map(
    ({ escrowUtxo, escrowPayment, escrowInfo }) => ({
      // parse all the PAB structures
      escrowUtxo: TxOutRef.fromPlutusTxOutRef(escrowUtxo),
      escrowPayment: [AssetClass.fromPlutusAssetClass(escrowPayment[0]), escrowPayment[1]],
      escrowInfo: {
        sender: Address.fromWalletAddress(escrowInfo.sender),
        rAssetClass: AssetClass.fromPlutusAssetClass(escrowInfo.rAssetClass),
        rAmount: escrowInfo.rAmount,
      },
    })
  )
}
