/**
 * Setup Whisper.cpp for local transcription
 *
 * This script installs Whisper.cpp and downloads the 'medium' model
 * which supports Russian language (unlike 'medium.en' which is English-only).
 *
 * Usage: npx tsx scripts/setup-whisper.ts
 */

import { installWhisperCpp, downloadWhisperModel } from '@remotion/install-whisper-cpp';
import path from 'path';

async function setup() {
  const whisperPath = path.join(process.cwd(), 'whisper.cpp');

  console.log('='.repeat(50));
  console.log('Установка Whisper.cpp для локальной транскрипции');
  console.log('='.repeat(50));

  // Step 1: Install Whisper.cpp
  console.log('\n[1/2] Устанавливаю Whisper.cpp v1.5.5...');
  console.log('(Это может занять несколько минут при первом запуске)\n');

  await installWhisperCpp({
    to: whisperPath,
    version: '1.5.5', // This version supports token-level timestamps
  });

  console.log('✓ Whisper.cpp установлен');

  // Step 2: Download the medium model (supports Russian)
  console.log('\n[2/2] Скачиваю модель "medium" (~1.5GB)...');
  console.log('(Модель medium поддерживает русский язык)\n');

  await downloadWhisperModel({
    model: 'medium', // NOT 'medium.en' - that's English only!
    folder: whisperPath,
  });

  console.log('✓ Модель medium скачана');

  console.log('\n' + '='.repeat(50));
  console.log('Готово! Теперь можно транскрибировать видео:');
  console.log('  npx tsx scripts/transcribe.ts /path/to/video.mp4 ru');
  console.log('='.repeat(50));
}

setup().catch((error) => {
  console.error('Ошибка установки:', error);
  process.exit(1);
});
