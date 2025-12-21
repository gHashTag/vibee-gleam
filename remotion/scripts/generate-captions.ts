/**
 * Generate captions from video using whisper.cpp
 *
 * Usage: npx tsx scripts/generate-captions.ts <video-path> <language> [output-path]
 * Example: npx tsx scripts/generate-captions.ts public/lipsync/lipsync.mp4 ru
 */

import { execSync } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';

interface WhisperToken {
  timestamps: { from: string; to: string };
  offsets: { from: number; to: number };
  text: string;
}

interface Caption {
  text: string;
  startMs: number;
  endMs: number;
  timestampMs: number;
  confidence: number;
}

function parseWhisperJson(jsonPath: string): WhisperToken[] {
  const data = JSON.parse(fs.readFileSync(jsonPath, 'utf-8'));
  return data.transcription || [];
}

function combineTokensToWords(tokens: WhisperToken[]): Caption[] {
  const captions: Caption[] = [];
  let currentWord = '';
  let wordStart = 0;
  let wordEnd = 0;

  for (const token of tokens) {
    const text = token.text;

    // Skip empty tokens and punctuation-only
    if (!text || text.trim() === '' || /^[.,!?;:\-"'()]+$/.test(text.trim())) {
      // If we have accumulated a word, save it
      if (currentWord.trim()) {
        captions.push({
          text: currentWord.trim(),
          startMs: wordStart,
          endMs: wordEnd,
          timestampMs: wordStart,
          confidence: 1,
        });
        currentWord = '';
      }
      continue;
    }

    // Check if this token starts with space (new word)
    if (text.startsWith(' ') && currentWord.trim()) {
      // Save previous word
      captions.push({
        text: currentWord.trim(),
        startMs: wordStart,
        endMs: wordEnd,
        timestampMs: wordStart,
        confidence: 1,
      });
      currentWord = text.trim();
      wordStart = token.offsets.from;
      wordEnd = token.offsets.to;
    } else {
      // Continue building current word
      if (!currentWord) {
        wordStart = token.offsets.from;
      }
      currentWord += text;
      wordEnd = token.offsets.to;
    }
  }

  // Don't forget the last word
  if (currentWord.trim()) {
    captions.push({
      text: currentWord.trim(),
      startMs: wordStart,
      endMs: wordEnd,
      timestampMs: wordStart,
      confidence: 1,
    });
  }

  return captions;
}

async function main() {
  const args = process.argv.slice(2);

  if (args.length < 2) {
    console.log('Usage: npx tsx scripts/generate-captions.ts <video-path> <language> [output-path]');
    console.log('Example: npx tsx scripts/generate-captions.ts public/lipsync/lipsync.mp4 ru');
    process.exit(1);
  }

  const videoPath = path.resolve(args[0]);
  const language = args[1];
  const outputPath = args[2] || videoPath.replace(/\.[^.]+$/, '_captions.json');

  console.log(`🎬 Video: ${videoPath}`);
  console.log(`🌐 Language: ${language}`);
  console.log(`📝 Output: ${outputPath}`);

  // Check video exists
  if (!fs.existsSync(videoPath)) {
    console.error(`❌ Video not found: ${videoPath}`);
    process.exit(1);
  }

  // Check whisper.cpp
  const whisperDir = path.join(process.cwd(), 'whisper.cpp');
  const modelPath = language === 'en'
    ? path.join(whisperDir, 'models/ggml-small.en.bin')
    : path.join(whisperDir, 'models/ggml-small.bin');

  if (!fs.existsSync(modelPath)) {
    console.error(`❌ Model not found: ${modelPath}`);
    console.log('Run: cd whisper.cpp && bash models/download-ggml-model.sh small');
    process.exit(1);
  }

  // Extract audio
  const audioPath = '/tmp/audio_for_transcription.wav';
  console.log('\n🔊 Extracting audio...');
  execSync(`ffmpeg -i "${videoPath}" -ar 16000 -ac 1 -y "${audioPath}" 2>/dev/null`);

  // Transcribe
  const jsonOutput = '/tmp/whisper_output';
  console.log('🎤 Transcribing...');
  execSync(
    `cd "${whisperDir}" && ./main -m "${modelPath}" -f "${audioPath}" -l ${language} -ml 3 -oj --output-file "${jsonOutput}"`,
    { stdio: 'inherit' }
  );

  // Parse JSON
  console.log('\n📖 Parsing transcription...');
  const tokens = parseWhisperJson(`${jsonOutput}.json`);
  const captions = combineTokensToWords(tokens);

  console.log(`✅ Found ${captions.length} words`);

  // Save captions
  fs.writeFileSync(outputPath, JSON.stringify(captions, null, 2));
  console.log(`💾 Saved to: ${outputPath}`);

  // Show preview
  console.log('\n📋 Preview (first 10 words):');
  captions.slice(0, 10).forEach((c, i) => {
    console.log(`  ${i + 1}. [${c.startMs}-${c.endMs}ms] "${c.text}"`);
  });

  // Cleanup
  fs.unlinkSync(audioPath);
  fs.unlinkSync(`${jsonOutput}.json`);
}

main().catch(console.error);
