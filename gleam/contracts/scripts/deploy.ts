/**
 * VIBEE P2P Escrow Contract Deployment Script
 * Deploys to TON testnet or mainnet
 *
 * Usage:
 *   DEPLOYER_MNEMONIC="word1 word2 ..." npx ts-node scripts/deploy.ts --network testnet
 *   DEPLOYER_MNEMONIC="word1 word2 ..." npx ts-node scripts/deploy.ts --network mainnet
 */

import { TonClient, WalletContractV4, internal, toNano } from "@ton/ton";
import { mnemonicToPrivateKey } from "@ton/crypto";
import { P2PEscrow } from "../escrow.tact_P2PEscrow";
import * as fs from "fs";
import * as path from "path";

// Configuration
const TESTNET_ENDPOINT = "https://testnet.toncenter.com/api/v2/jsonRPC";
const MAINNET_ENDPOINT = "https://toncenter.com/api/v2/jsonRPC";

interface DeployResult {
  contractAddress: string;
  deployTxHash: string;
  network: string;
  oracleAddress: string;
}

async function deploy(network: "testnet" | "mainnet"): Promise<DeployResult> {
  console.log(`\n🚀 Deploying VIBEE P2P Escrow to ${network}...\n`);

  // 1. Get mnemonic from environment
  const mnemonic = process.env.DEPLOYER_MNEMONIC;
  if (!mnemonic) {
    throw new Error("DEPLOYER_MNEMONIC environment variable is required");
  }

  // 2. Create wallet from mnemonic
  const keyPair = await mnemonicToPrivateKey(mnemonic.split(" "));
  const wallet = WalletContractV4.create({
    workchain: 0,
    publicKey: keyPair.publicKey,
  });

  // 3. Connect to TON
  const endpoint = network === "testnet" ? TESTNET_ENDPOINT : MAINNET_ENDPOINT;
  const apiKey = process.env.TON_API_KEY || "";
  const client = new TonClient({
    endpoint,
    apiKey: apiKey || undefined,
  });

  // 4. Check wallet balance
  const walletAddress = wallet.address;
  console.log(`📍 Deployer wallet: ${walletAddress.toString()}`);

  const balance = await client.getBalance(walletAddress);
  console.log(`💰 Balance: ${Number(balance) / 1e9} TON`);

  if (balance < toNano("0.5")) {
    throw new Error(`Insufficient balance. Need at least 0.5 TON for deployment.`);
  }

  // 5. Open wallet contract
  const walletContract = client.open(wallet);

  // 6. Oracle address = deployer address in testnet (for testing)
  const oracleAddress = network === "testnet"
    ? walletAddress
    : walletAddress; // In production, use separate oracle address

  console.log(`🔮 Oracle address: ${oracleAddress.toString()}`);

  // 7. Initialize P2PEscrow contract using Tact-generated init
  const escrow = await P2PEscrow.fromInit(oracleAddress);
  const escrowContract = client.open(escrow);
  const escrowAddress = escrow.address;

  console.log(`📝 Contract address: ${escrowAddress.toString()}`);

  // 8. Check if already deployed
  const isDeployed = await client.isContractDeployed(escrowAddress);
  if (isDeployed) {
    console.log(`⚠️  Contract already deployed at ${escrowAddress.toString()}`);
    return {
      contractAddress: escrowAddress.toString(),
      deployTxHash: "already_deployed",
      network,
      oracleAddress: oracleAddress.toString(),
    };
  }

  // 9. Deploy
  console.log(`\n🚀 Sending deployment transaction...`);

  const seqno = await walletContract.getSeqno();

  await walletContract.sendTransfer({
    secretKey: keyPair.secretKey,
    seqno,
    messages: [
      internal({
        to: escrowAddress,
        value: toNano("0.3"), // Deploy value
        init: escrow.init,
        body: null,
      }),
    ],
  });

  // 10. Wait for deployment
  console.log(`⏳ Waiting for deployment confirmation...`);

  let attempts = 0;
  const maxAttempts = 30;

  while (attempts < maxAttempts) {
    await new Promise((resolve) => setTimeout(resolve, 2000));
    const deployed = await client.isContractDeployed(escrowAddress);
    if (deployed) {
      console.log(`\n✅ Contract deployed!`);
      break;
    }
    attempts++;
    process.stdout.write(".");
  }

  if (attempts >= maxAttempts) {
    throw new Error("Deployment confirmation timeout");
  }

  const result: DeployResult = {
    contractAddress: escrowAddress.toString(),
    deployTxHash: `seqno_${seqno}`,
    network,
    oracleAddress: oracleAddress.toString(),
  };

  console.log(`   Address: ${result.contractAddress}`);
  console.log(`   Network: ${result.network}`);
  console.log(`   Oracle: ${result.oracleAddress}`);

  // 11. Save deployment info
  const deploymentInfo = {
    ...result,
    deployedAt: new Date().toISOString(),
    feePercent: 0.5,
  };

  const buildPath = path.join(__dirname, "..");
  const deploymentPath = path.join(buildPath, `deployment-${network}.json`);
  fs.writeFileSync(deploymentPath, JSON.stringify(deploymentInfo, null, 2));
  console.log(`\n📄 Deployment info saved to: ${deploymentPath}`);

  return result;
}

// Parse command line args
const args = process.argv.slice(2);
const networkIdx = args.indexOf("--network");
const network = networkIdx >= 0 ? args[networkIdx + 1] : "testnet";

if (network !== "testnet" && network !== "mainnet") {
  console.error("Invalid network. Use --network testnet or --network mainnet");
  process.exit(1);
}

deploy(network as "testnet" | "mainnet")
  .then(() => {
    console.log("\n🎉 Deployment complete!");
    process.exit(0);
  })
  .catch((err) => {
    console.error("\n❌ Deployment failed:", err.message);
    process.exit(1);
  });
