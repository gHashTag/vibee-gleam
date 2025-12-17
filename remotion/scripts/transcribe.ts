/**
 * Transcribe video to captions using local Whisper.cpp
 *
 * Supports Russian and other languages.
 *
 * Usage:
 *   npx tsx scripts/transcribe.ts <video_path> [language] [output_path]
 *
 * Examples:
 *   npx tsx scripts/transcribe.ts public/lipsync/lipsync.mp4 ru
 *   npx tsx scripts/transcribe.ts video.mp4 en captions.json
 */

import { transcribe, convertToCaptions } from '@remotion/install-whisper-cpp';
import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';

// Type definitions
interface Caption {
  text: string;
  startMs: number;
  endMs: number;
  confidence?: number;
}

interface Segment {
  type: 'split' | 'fullscreen';
  startFrame: number;
  durationFrames: number;
  caption: string;
  bRollUrl?: string;
  bRollType?: 'video' | 'image';
}

/**
 * Extract audio from video as 16KHz WAV (required by Whisper)
 */
async function extractAudio(videoPath: string): Promise<string> {
  const audioPath = videoPath.replace(/\.[^.]+$/, '_audio.wav');

  console.log('Извлекаю аудио из видео...');
  execSync(
    `ffmpeg -i "${videoPath}" -ar 16000 -ac 1 -y "${audioPath}" 2>/dev/null`,
    { stdio: 'pipe' }
  );

  return audioPath;
}

/**
 * Transcribe audio file to captions
 */
export async function transcribeVideo(
  videoPath: string,
  language: string = 'ru',
  fps: number = 30
): Promise<{ captions: Caption[]; segments: Segment[] }> {
  const whisperPath = path.join(process.cwd(), 'whisper.cpp');

  // Check if Whisper is installed
  if (!fs.existsSync(whisperPath)) {
    console.error('Whisper.cpp не установлен. Запустите:');
    console.error('  npx tsx scripts/setup-whisper.ts');
    process.exit(1);
  }

  // Extract audio
  const audioPath = await extractAudio(videoPath);

  console.log(`Транскрибирую на языке: ${language}...`);
  console.log('(Это может занять несколько минут)\n');

  // Transcribe with Whisper
  const result = await transcribe({
    inputPath: audioPath,
    whisperPath,
    model: 'medium',
    language,
    tokenLevelTimestamps: true,
  });

  // Convert to Remotion Caption format
  const captions = convertToCaptions({
    transcription: result.transcription,
  });

  console.log(`\nНайдено ${captions.length} фраз\n`);

  // Convert to segments for SplitTalkingHead
  const segments = captionsToSegments(captions, fps);

  // Cleanup temp audio file
  fs.unlinkSync(audioPath);

  return { captions, segments };
}

/**
 * Convert captions to SplitTalkingHead segments
 */
function captionsToSegments(captions: Caption[], fps: number): Segment[] {
  const brollVideos = [
    '/backgrounds/business/00.mp4',
    '/backgrounds/business/01.mp4',
    '/backgrounds/business/02.mp4',
    '/backgrounds/business/03.mp4',
    '/backgrounds/business/04.mp4',
  ];

  return captions.map((caption, i) => {
    // Alternate between split and fullscreen (every 3rd is fullscreen)
    const type: 'split' | 'fullscreen' = i % 3 === 0 ? 'fullscreen' : 'split';

    const startFrame = Math.floor((caption.startMs / 1000) * fps);
    const endFrame = Math.floor((caption.endMs / 1000) * fps);
    const durationFrames = Math.max(endFrame - startFrame, fps); // minimum 1 second

    return {
      type,
      startFrame,
      durationFrames,
      caption: caption.text.toUpperCase(), // Yellow captions are uppercase
      bRollUrl: type === 'split' ? brollVideos[i % brollVideos.length] : undefined,
      bRollType: 'video' as const,
    };
  });
}

/**
 * Main CLI entry point
 */
async function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.log('Использование:');
    console.log('  npx tsx scripts/transcribe.ts <video_path> [language] [output_path]');
    console.log('\nПримеры:');
    console.log('  npx tsx scripts/transcribe.ts public/lipsync/lipsync.mp4 ru');
    console.log('  npx tsx scripts/transcribe.ts video.mp4 en captions.json');
    console.log('\nПоддерживаемые языки: ru, en, de, fr, es, it, ja, ko, zh, ...');
    process.exit(0);
  }

  const videoPath = args[0];
  const language = args[1] || 'ru';
  const outputPath = args[2] || videoPath.replace(/\.[^.]+$/, '_captions.json');

  if (!fs.existsSync(videoPath)) {
    console.error(`Файл не найден: ${videoPath}`);
    process.exit(1);
  }

  console.log('='.repeat(50));
  console.log('Транскрипция видео с Whisper.cpp');
  console.log('='.repeat(50));
  console.log(`Видео: ${videoPath}`);
  console.log(`Язык: ${language}`);
  console.log(`Выход: ${outputPath}\n`);

  const { captions, segments } = await transcribeVideo(videoPath, language);

  // Save results
  const output = {
    videoPath,
    language,
    generatedAt: new Date().toISOString(),
    captions,
    segments,
  };

  fs.writeFileSync(outputPath, JSON.stringify(output, null, 2));

  console.log('='.repeat(50));
  console.log('Готово!');
  console.log(`Сохранено в: ${outputPath}`);
  console.log('='.repeat(50));

  // Print preview
  console.log('\nПревью первых 5 сегментов:');
  segments.slice(0, 5).forEach((seg, i) => {
    const time = (seg.startFrame / 30).toFixed(1);
    console.log(`  ${i + 1}. [${time}s] ${seg.type.padEnd(10)} "${seg.caption}"`);
  });

  if (segments.length > 5) {
    console.log(`  ... и ещё ${segments.length - 5} сегментов`);
  }
}

main().catch((error) => {
  console.error('Ошибка:', error);
  process.exit(1);
});
