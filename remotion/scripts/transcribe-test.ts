/**
 * Transcribe test script
 * Run: npx tsx scripts/transcribe-test.ts
 */

import { transcribeVideo } from '../src/lib/transcribe';
import * as fs from 'fs';

async function main() {
  console.log('🎤 Starting transcription of lipsync.mp4...');

  try {
    const result = await transcribeVideo(
      '/Users/playra/vibee/remotion/public/lipsync/lipsync.mp4',
      'ru',
      30
    );

    console.log('\n✅ Transcription complete!');
    console.log(`📝 Found ${result.captions.length} caption segments\n`);

    // Print captions
    console.log('=== CAPTIONS ===');
    result.captions.forEach((cap, i) => {
      console.log(`${i}: [${cap.startMs}ms - ${cap.endMs}ms] "${cap.text}"`);
    });

    // Save to JSON
    const outputPath = '/Users/playra/vibee/remotion/public/lipsync/captions.json';
    fs.writeFileSync(outputPath, JSON.stringify(result.captions, null, 2));
    console.log(`\n💾 Saved to: ${outputPath}`);

    // Generate code for Root.tsx
    console.log('\n=== CODE FOR Root.tsx ===');
    console.log('captions: ' + JSON.stringify(result.captions.slice(0, 20), null, 2));

  } catch (error) {
    console.error('❌ Error:', error);
  }
}

main();
