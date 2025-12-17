/**
 * Generate a new TON wallet for testing
 * Outputs mnemonic and address
 */

import { mnemonicNew, mnemonicToPrivateKey } from "@ton/crypto";
import { WalletContractV4 } from "@ton/ton";

async function generateWallet() {
  console.log("\n🔐 Generating new TON wallet...\n");

  // Generate 24-word mnemonic
  const mnemonic = await mnemonicNew(24);
  console.log("📝 Mnemonic (SAVE THIS SECURELY!):");
  console.log(`   ${mnemonic.join(" ")}\n`);

  // Derive keypair
  const keyPair = await mnemonicToPrivateKey(mnemonic);

  // Create wallet contract
  const wallet = WalletContractV4.create({
    workchain: 0,
    publicKey: keyPair.publicKey,
  });

  const address = wallet.address;

  console.log("📍 Wallet Address:");
  console.log(`   Raw: ${address.toRawString()}`);
  console.log(`   Friendly (testnet): ${address.toString({ testOnly: true })}`);
  console.log(`   Friendly (mainnet): ${address.toString()}`);

  console.log("\n💧 Get testnet TON from faucet:");
  console.log("   https://t.me/testgiver_ton_bot");
  console.log(`   Send to: ${address.toString({ testOnly: true })}`);

  console.log("\n📋 To deploy, run:");
  console.log(`   DEPLOYER_MNEMONIC="${mnemonic.join(" ")}" bun scripts/deploy.ts --network testnet`);

  console.log("\n⚠️  WARNING: Never share your mnemonic! Keep it secret!");
}

generateWallet().catch(console.error);
