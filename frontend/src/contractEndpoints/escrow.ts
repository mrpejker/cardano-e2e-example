import type {
  ContractEndpoints,
  CIP30WalletWrapper,
  TxBudgetAPI,
  Balancer,
  Value,
  AssetClass,
  TxOutRef,
  WalletAddress,
  Plutus
} from "cardano-pab-client";
import { CancelParams, ResolveParams, StartParams } from "./parameters";

/**
 * The representation of the contract Utxo state
 */
export type EscrowInfo = {
  sender: WalletAddress;
  rAssetClass: AssetClass;
  rAmount: number;
}

export type UtxoEscrowInfo = {
  escrowUtxo: TxOutRef
  escrowInfo: EscrowInfo
  escrowValue: Value
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

    const { Balancer, CIP30WalletWrapper, ContractEndpoints, PABApi,
      TxBudgetAPI, WalletAddress, getWalletInitialAPI, failed,
      getProtocolParamsFromBlockfrost,
    } = this.PABClient;

    const walletInitialAPI = getWalletInitialAPI(window, walletName);
    // this will ask the user to give to this dApp access to their wallet methods
    const walletInjectedFromBrowser = await walletInitialAPI.enable();

    // then we can initialize the CIP30WalletWrapper class of the library
    this.wallet = await CIP30WalletWrapper.init(walletInjectedFromBrowser);
    const [addr] = await this.wallet.getUsedAddresses();
    const result = await WalletAddress.fromHexAddress(addr);
    if (failed(result)) {
      console.log(result.error)
      return null;
    }
    const walletAddr = result.value;
    console.log(`Connected Address: ${JSON.stringify(walletAddr)}`);

    const walletId = await this.wallet.getWalletId();
    const pabApi = new PABApi(process.env.REACT_APP_PAB_URL);

    this.endpoints = await ContractEndpoints.connect(
      walletId,
      { params: walletAddr.toPAB() },
      pabApi,
    );

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
    const { succeeded } = this.PABClient;
    console.log(`Start Params:`)
    console.log(sp)

    // Try to get unbalanced transaction from PAB
    const pabResponse = await this.endpoints.doOperation(
      { endpointTag: "start", params: sp }
    );

    if (!succeeded(pabResponse)) {
      alert(
        `Didn't got the unbalanced transaction from the PAB. Error: ${pabResponse.error}`
      );
    } else {
      // the pab yielded the unbalanced transaction. balance, sign and submit it.
      const etx = pabResponse.value;
      console.log(`Unbalanced tx:`);
      console.log(etx)
      const walletInfo = await this.wallet.getWalletInfo();

      const fullyBalancedTx = await this.balancer.fullBalanceTx(
        etx,
        walletInfo,
        // configuration for the balanceTx and rebalanceTx methods which are interally
        // used by this method
        { feeUpperBound: 1000000, mergeSignerOutputs: false },
        // a high-order function that exposes the balanced tx and the inputs info so to
        // calculate the executions units, which are then set in the transaction and
        // goes to the rebalancing step
        async (balancedTx, inputsInfo) => {
          const txBudgetResponse = await this.txBudgetApi.estimate(balancedTx, inputsInfo);
          if (succeeded(txBudgetResponse)) {
            const units = txBudgetResponse.value;
            return units;
          } else {
            console.log("BALANCER FAILED")
            console.log(txBudgetResponse.error)
            return []
          }
        }
      );
      // print to the console the fully balanced tx cbor for debugging purposes
      console.log(`Balanced tx: ${fullyBalancedTx}`);
      // now that the transaction is balanced, sign and submit it with the wallet
      const response = await this.wallet.signAndSubmit(fullyBalancedTx);
      if (succeeded(response)) {
        const txHash = response.value;
        console.log(`TX HASH: ${txHash}`)
        alert(`Start suceeded. Tx hash: ${txHash}`);
      } else {
        alert(`Start failed when trying to submit it. Error: ${response.error}`);
      }
    }
  }

  public async reload(): Promise<ObsState> {
    const { succeeded } = this.PABClient;
    console.log("Inside Reload")
    const response = await this.endpoints.reload({ endpointTag: "reload", params: [] })
    console.log(response)
    if (succeeded(response)) {
      const escrows = response.value as PABObservableState;
      console.log(escrows)
      const observableState = parsePABObservableState(escrows);
      console.log("Finishing Reload");
      return observableState;
      // let utxoInfo = parseReloadResponse(escrows)
    } else {
      alert(`Reload Failed. Error: ${response.error}`);
    }
  }

  public async cancel(cp: CancelParams){
    const { succeeded } = this.PABClient;
    console.log("Cancelling Escrow")
    console.log(cp)
    // Try to get unbalanced transaction from PAB
    const pabResponse = await this.endpoints.doOperation(
      { endpointTag: "cancel", params: cp }
    );
    console.log(pabResponse)
    if (!succeeded(pabResponse)) {
      alert(
        `Didn't got the unbalanced transaction from the PAB. Error: ${pabResponse.error}`
      );
    } else {
      // the pab yielded the unbalanced transaction. balance, sign and submit it.
      const etx = pabResponse.value;
      console.log(`Unbalanced tx:`);
      console.log(etx)
      const walletInfo = await this.wallet.getWalletInfo();

      const fullyBalancedTx = await this.balancer.fullBalanceTx(
        etx,
        walletInfo,
        // configuration for the balanceTx and rebalanceTx methods which are interally
        // used by this method
        { feeUpperBound: 1000000, mergeSignerOutputs: false },
        // a high-order function that exposes the balanced tx and the inputs info so to
        // calculate the executions units, which are then set in the transaction and
        // goes to the rebalancing step
        async (balancedTx, inputsInfo) => {
          const txBudgetResponse = await this.txBudgetApi.estimate(balancedTx, inputsInfo);
          if (succeeded(txBudgetResponse)) {
            const units = txBudgetResponse.value;
            return units;
          } else {
            console.log("BALANCER FAILED")
            console.log(txBudgetResponse.error)
            return []
          }
        }
      );
      // print to the console the fully balanced tx cbor for debugging purposes
      console.log(`Balanced tx: ${fullyBalancedTx}`);
      // now that the transaction is balanced, sign and submit it with the wallet
      const response = await this.wallet.signAndSubmit(fullyBalancedTx);
      if (succeeded(response)) {
        const txHash = response.value;
        console.log(`TX HASH: ${txHash}`)
        alert(`Cancel suceeded. Tx hash: ${txHash}`);
      } else {
        alert(`Cancel failed when trying to submit it. Error: ${response.error}`);
      }
    }
  }

  public async resolve(rp: ResolveParams) {
    const { succeeded } = this.PABClient;
    console.log(`Resolve Params:`)
    console.log(rp)

    // Try to get unbalanced transaction from PAB
    const pabResponse = await this.endpoints.doOperation(
      { endpointTag: "resolve", params: rp }
    );

    if (!succeeded(pabResponse)) {
      alert(
        `Didn't got the unbalanced transaction from the PAB. Error: ${pabResponse.error}`
      );
    } else {
      // the pab yielded the unbalanced transaction. balance, sign and submit it.
      const etx = pabResponse.value;
      console.log(`Unbalanced tx:`);
      console.log(etx)
      const walletInfo = await this.wallet.getWalletInfo();

      const fullyBalancedTx = await this.balancer.fullBalanceTx(
        etx,
        walletInfo,
        // configuration for the balanceTx and rebalanceTx methods which are interally
        // used by this method
        { feeUpperBound: 1000000, mergeSignerOutputs: false },
        // a high-order function that exposes the balanced tx and the inputs info so to
        // calculate the executions units, which are then set in the transaction and
        // goes to the rebalancing step
        async (balancedTx, inputsInfo) => {
          const txBudgetResponse = await this.txBudgetApi.estimate(balancedTx, inputsInfo);
          if (succeeded(txBudgetResponse)) {
            const units = txBudgetResponse.value;
            return units;
          } else {
            console.log("BALANCER FAILED")
            console.log(txBudgetResponse.error)
            return []
          }
        }
      );
      // print to the console the fully balanced tx cbor for debugging purposes
      console.log(`Balanced tx: ${fullyBalancedTx}`);
      // now that the transaction is balanced, sign and submit it with the wallet
      const response = await this.wallet.signAndSubmit(fullyBalancedTx);
      if (succeeded(response)) {
        const txHash = response.value;
        console.log(`TX HASH: ${txHash}`)
        alert(`Resolve suceeded. Tx hash: ${txHash}`);
      } else {
        alert(`Resolve failed when trying to submit it. Error: ${response.error}`);
      }
    }
  }
}

type PABObservableState = Array<{
  escrowUtxo: Plutus.TxOutRef,
  escrowValue: Plutus.Value,
  escrowInfo: {
    sender: ReturnType<WalletAddress["toPAB"]>;
    rAssetClass: Plutus.AssetClass;
    rAmount: number;
  }
}>;

async function parsePABObservableState(escrows: PABObservableState): Promise<ObsState> {
  const { AssetClass, TxOutRef, Value, WalletAddress } = await import("cardano-pab-client");
  return escrows.map(
    ({ escrowUtxo, escrowValue, escrowInfo }) => ({
      // parse all the PAB structures
      escrowUtxo: TxOutRef.fromPlutusTxOutRef(escrowUtxo),
      escrowValue: Value.fromPlutusValue(escrowValue),
      escrowInfo: {
        sender: WalletAddress.fromPAB(escrowInfo.sender),
        rAssetClass: AssetClass.fromPlutusAssetClass(escrowInfo.rAssetClass),
        rAmount: escrowInfo.rAmount,
      },
    })
  )
}