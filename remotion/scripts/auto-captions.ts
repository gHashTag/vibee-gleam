/**
 * Auto-generate captions for lipsync video before deploy
 *
 * Usage: npx tsx scripts/auto-captions.ts [--force]
 *
 * Checks if video changed (md5 hash) and regenerates captions.json
 * Use --force to skip hash check and always regenerate
 */

import fs from 'fs';
import path from 'path';
import crypto from 'crypto';
import { transcribeVideo } from '../src/lib/transcribe.js';

const LIPSYNC_VIDEO = 'public/lipsync/lipsync.mp4';
const CAPTIONS_FILE = 'public/lipsync/captions.json';
const HASH_FILE = 'public/lipsync/.video-hash';

function getFileHash(filePath: string): string {
  const content = fs.readFileSync(filePath);
  return crypto.createHash('md5').update(content).digest('hex');
}

async function main() {
  const forceRegenerate = process.argv.includes('--force');

  console.log('🎬 Auto-captions script');
  console.log('='.repeat(50));

  // Check if video exists
  if (!fs.existsSync(LIPSYNC_VIDEO)) {
    console.log('⚠️  No lipsync video found at:', LIPSYNC_VIDEO);
    console.log('   Skipping captions generation');
    return;
  }

  // Check if whisper is installed
  const whisperPath = path.join(process.cwd(), 'whisper.cpp');
  if (!fs.existsSync(whisperPath)) {
    console.error('❌ Whisper.cpp not installed!');
    console.error('   Run: npx tsx scripts/setup-whisper.ts');
    process.exit(1);
  }

  // Check if video changed (skip if --force)
  const currentHash = getFileHash(LIPSYNC_VIDEO);
  const previousHash = fs.existsSync(HASH_FILE)
    ? fs.readFileSync(HASH_FILE, 'utf-8').trim()
    : '';

  console.log(`📁 Video: ${LIPSYNC_VIDEO}`);
  console.log(`📝 Output: ${CAPTIONS_FILE}`);
  console.log(`🔑 Current hash: ${currentHash.slice(0, 8)}...`);
  console.log(`🔑 Previous hash: ${previousHash ? previousHash.slice(0, 8) + '...' : '(none)'}`);

  if (!forceRegenerate && currentHash === previousHash) {
    console.log('\n✅ Video unchanged, skipping transcription');
    return;
  }

  if (forceRegenerate) {
    console.log('\n⚡ Force mode: regenerating captions');
  } else {
    console.log('\n🔄 Video changed, generating new captions...');
  }

  // Transcribe using the working CLI-based transcriber
  console.log('\n🎤 Transcribing with Whisper (this may take a few minutes)...');
  const result = await transcribeVideo(LIPSYNC_VIDEO, 'ru', 30);

  const captions = result.captions;

  console.log(`\n📝 Found ${captions.length} caption segments`);

  // Preview first few captions
  console.log('\nPreview:');
  captions.slice(0, 5).forEach((c, i) => {
    const time = (c.startMs / 1000).toFixed(1);
    console.log(`  ${i + 1}. [${time}s] "${c.text}"`);
  });
  if (captions.length > 5) {
    console.log(`  ... and ${captions.length - 5} more`);
  }

  // Save captions
  fs.writeFileSync(CAPTIONS_FILE, JSON.stringify(captions, null, 2));
  console.log(`\n💾 Saved: ${CAPTIONS_FILE}`);

  // Save hash
  fs.writeFileSync(HASH_FILE, currentHash);
  console.log(`💾 Saved hash: ${HASH_FILE}`);

  console.log('\n' + '='.repeat(50));
  console.log(`✅ Generated ${captions.length} captions successfully!`);
}

main().catch((error) => {
  console.error('\n❌ Error:', error.message || error);
  process.exit(1);
});
