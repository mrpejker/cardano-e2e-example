import type {
  ContractEndpoints,
  CIP30WalletWrapper,
  TxBudgetAPI,
  Balancer,
  AssetClass,
  TxOutRef,
  WalletAddress,
  Plutus,
} from "cardano-pab-client";
import { CancelParams, ResolveParams, StartParams } from "./parameters";

/**
 * The representation of the contract Utxo state
 */
export type EscrowInfo = {
  sender: string;
  rAssetClass: AssetClass;
  rAmount: number;
}

export type UtxoEscrowInfo = {
  escrowUtxo: TxOutRef
  escrowInfo: EscrowInfo
  escrowPayment: [AssetClass, number]
}

export type ObsState = {
  escrowsInfo: UtxoEscrowInfo[],
  networkId : 0 | 1
}

export class EscrowEndpoints {
  PABClient: typeof import("cardano-pab-client");

  endpoints: ContractEndpoints;

  wallet: CIP30WalletWrapper;

  txBudgetApi: TxBudgetAPI;

  balancer: Balancer;

  private constructor(
    PABClient: typeof import("cardano-pab-client"),
    endpoints: ContractEndpoints,
    wallet: CIP30WalletWrapper,
    txBudgetApi: TxBudgetAPI,
    balancer: Balancer,
  ) {
    this.PABClient = PABClient;
    this.endpoints = endpoints;
    this.wallet = wallet;
    this.txBudgetApi = txBudgetApi;
    this.balancer = balancer;
  }

  public static async connect(walletName: "nami" | "eternl"): Promise<EscrowEndpoints | null> {
    // load PAB Client just once!
    const PABClient = await import("cardano-pab-client");

    const { Balancer, CIP30WalletWrapper, ContractEndpoints, TxBudgetAPI,
      getWalletInitialAPI, getProtocolParamsFromBlockfrost,
    } = PABClient;

    const walletInitialAPI = getWalletInitialAPI(window, walletName);
    // this will ask the user to give to this dApp access to their wallet methods
    const walletInjectedFromBrowser = await walletInitialAPI.enable();

    // Initialize CIP30 wallet wrapper
    const wallet = await CIP30WalletWrapper.init(walletInjectedFromBrowser);

    const walletAddress = await wallet.getWalletAddress();
    console.log(`Connected Address: ${JSON.stringify(walletAddress)}`);

    const { pabUrl, budgetUrl, blockfrostUrl, blockfrostApiKey } = getEnvs();

    // Initialize ContractEndpoints
    const endpoints = await ContractEndpoints.connect(
      pabUrl,
      { contents: walletAddress },
    );

    // Initialize tx budget service API
    const txBudgetApi = new TxBudgetAPI({
      baseUrl: budgetUrl,
      timeout: 10000,
    })

    // Initialize Balancer
    const protocolParams = await getProtocolParamsFromBlockfrost(
      blockfrostUrl,
      blockfrostApiKey,
    );
    const balancer = await Balancer.init(protocolParams);

    return new EscrowEndpoints(PABClient, endpoints, wallet, txBudgetApi, balancer);
  }

  public async start(sp: StartParams): Promise<void | undefined> {
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

  public async reload(): Promise<ObsState | null> {
    const { failed } = this.PABClient;
    const response = await this.endpoints.reload({ tag: "reload", contents: [] })
    const network = await this.wallet.getNetworkId()
    if (failed(response)) {
      return null;
    }
    const escrows = response.value as PABObservableState;
    console.log(escrows)
    const observableState = parsePABObservableState(escrows, network as 0 | 1);
    console.log("Finishing Reload");
    return observableState;
  }

  public async cancel(cp: CancelParams): Promise<void> {
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

  public async resolve(rp: ResolveParams): Promise<void | undefined> {
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
  escrowPayment: [Plutus.AssetClass, number],
  escrowInfo: {
    sender: WalletAddress;
    rAssetClass: Plutus.AssetClass;
    rAmount: number;
  }
}>;

async function parsePABObservableState(escrows: PABObservableState, network: 0 | 1): Promise<ObsState> {
  const { AssetClass, TxOutRef, Address } = await import("cardano-pab-client");
  const escrowsMap = escrows.map(
    async ({escrowUtxo,escrowPayment,escrowInfo}) => ({
      escrowUtxo: TxOutRef.fromPlutusTxOutRef(escrowUtxo),
      escrowPayment: [AssetClass.fromPlutusAssetClass(escrowPayment[0]), escrowPayment[1]] as [AssetClass, number],
      escrowInfo: {
        sender: await Address.fromWalletAddress(escrowInfo.sender).toBech32(network),
        rAssetClass: AssetClass.fromPlutusAssetClass(escrowInfo.rAssetClass),
        rAmount: escrowInfo.rAmount
      }
    })
  )
  const utxosEscrows = Promise.all(escrowsMap).then()
  return { escrowsInfo: await utxosEscrows , networkId: network}
}

/**
 * Gets and returns the needed .env variables. Throws an error if someone of them is not defined.
 */
function getEnvs(): {
  pabUrl: string,
  budgetUrl: string,
  blockfrostUrl: string,
  blockfrostApiKey: string,
} {
  const pabUrl = process.env.REACT_APP_PAB_URL;
  if (!pabUrl) {
    throw new Error("REACT_APP_PAB_URL .env variable needed but not defined!");
  }
  const budgetUrl = process.env.REACT_APP_BUDGET_URL;
  if (!budgetUrl) {
    throw new Error("REACT_APP_BUDGET_URL .env variable needed but not defined!");
  }
  const blockfrostUrl = process.env.REACT_APP_BLOCKFROST_URL;
  if (!blockfrostUrl) {
    throw new Error("REACT_APP_BLOCKFROST_URL .env variable needed but not defined!");
  }
  const blockfrostApiKey = process.env.REACT_APP_BLOCKFROST_API_KEY;
  if (!blockfrostApiKey) {
    throw new Error("REACT_APP_BLOCKFROST_API_KEY .env variable needed but not defined!");
  }
  return { pabUrl, budgetUrl, blockfrostUrl, blockfrostApiKey };
}
