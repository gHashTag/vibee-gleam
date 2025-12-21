/**
 * Transcription Library using whisper.cpp CLI
 *
 * Provides audio transcription using Whisper.cpp directly
 * Supports Russian and other languages
 */

import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';
import https from 'https';
import http from 'http';

// Type definitions
export interface Caption {
  text: string;
  startMs: number;
  endMs: number;
  timestampMs: number;
  confidence: number;
}

export interface Segment {
  type: 'split' | 'fullscreen';
  startFrame: number;
  durationFrames: number;
  caption: string;
  bRollUrl?: string;
  bRollType?: 'video' | 'image';
}

export interface TranscriptionResult {
  captions: Caption[];
  segments: Segment[];
}

interface WhisperToken {
  timestamps: { from: string; to: string };
  offsets: { from: number; to: number };
  text: string;
}

/**
 * Download file from URL to local path
 */
async function downloadFile(url: string, destPath: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const protocol = url.startsWith('https') ? https : http;
    const file = fs.createWriteStream(destPath);

    protocol
      .get(url, (response) => {
        if (response.statusCode === 301 || response.statusCode === 302) {
          const redirectUrl = response.headers.location;
          if (redirectUrl) {
            file.close();
            fs.unlinkSync(destPath);
            return downloadFile(redirectUrl, destPath).then(resolve).catch(reject);
          }
        }

        response.pipe(file);
        file.on('finish', () => {
          file.close();
          resolve();
        });
      })
      .on('error', (err) => {
        fs.unlink(destPath, () => {});
        reject(err);
      });
  });
}

/**
 * Extract audio from video as 16KHz WAV
 */
function extractAudio(videoPath: string): string {
  const audioPath = `/tmp/whisper_audio_${Date.now()}.wav`;

  console.log('Extracting audio from video...');
  execSync(
    `ffmpeg -i "${videoPath}" -ar 16000 -ac 1 -y "${audioPath}" 2>/dev/null`,
    { stdio: 'pipe' }
  );

  return audioPath;
}

/**
 * Parse whisper JSON output and combine tokens into words
 */
function parseWhisperOutput(jsonPath: string): Caption[] {
  const data = JSON.parse(fs.readFileSync(jsonPath, 'utf-8'));
  const tokens: WhisperToken[] = data.transcription || [];

  const captions: Caption[] = [];
  let currentWord = '';
  let wordStart = 0;
  let wordEnd = 0;

  for (const token of tokens) {
    const text = token.text;

    // Skip empty tokens and punctuation-only
    if (!text || text.trim() === '' || /^[.,!?;:\-"'()]+$/.test(text.trim())) {
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
    const type: 'split' | 'fullscreen' = i % 3 === 0 ? 'fullscreen' : 'split';

    const startFrame = Math.floor((caption.startMs / 1000) * fps);
    const endFrame = Math.floor((caption.endMs / 1000) * fps);
    const durationFrames = Math.max(endFrame - startFrame, fps);

    return {
      type,
      startFrame,
      durationFrames,
      caption: caption.text.toUpperCase(),
      bRollUrl: type === 'split' ? brollVideos[i % brollVideos.length] : undefined,
      bRollType: 'video' as const,
    };
  });
}

/**
 * Transcribe video to captions using Whisper.cpp CLI
 */
export async function transcribeVideo(
  videoPathOrUrl: string,
  language: string = 'ru',
  fps: number = 30
): Promise<TranscriptionResult> {
  const whisperDir = path.join(process.cwd(), 'whisper.cpp');

  // Select model based on language
  const modelPath = language === 'en'
    ? path.join(whisperDir, 'models/ggml-small.en.bin')
    : path.join(whisperDir, 'models/ggml-small.bin');

  // Check if Whisper and model exist
  if (!fs.existsSync(whisperDir)) {
    throw new Error('Whisper.cpp not installed. Run: git clone https://github.com/ggerganov/whisper.cpp');
  }

  if (!fs.existsSync(modelPath)) {
    throw new Error(`Model not found: ${modelPath}. Run: cd whisper.cpp && bash models/download-ggml-model.sh small`);
  }

  // Handle URLs - download to temp file
  let videoPath = videoPathOrUrl;
  let isTemp = false;

  if (videoPathOrUrl.startsWith('http://') || videoPathOrUrl.startsWith('https://')) {
    const ext = path.extname(new URL(videoPathOrUrl).pathname) || '.mp4';
    videoPath = `/tmp/whisper_video_${Date.now()}${ext}`;
    isTemp = true;

    console.log(`Downloading video from ${videoPathOrUrl}...`);
    await downloadFile(videoPathOrUrl, videoPath);
  }

  // Handle local paths in public directory
  if (videoPath.startsWith('/') && !videoPath.startsWith('/Users') && !videoPath.startsWith('/app') && !videoPath.startsWith('/tmp')) {
    videoPath = path.join(process.cwd(), 'public', videoPath);
  }

  if (!fs.existsSync(videoPath)) {
    throw new Error(`Video file not found: ${videoPath}`);
  }

  let audioPath: string | null = null;
  const jsonOutput = `/tmp/whisper_output_${Date.now()}`;

  try {
    // Extract audio
    audioPath = extractAudio(videoPath);

    console.log(`Transcribing with language: ${language}...`);

    // Run whisper.cpp CLI
    execSync(
      `cd "${whisperDir}" && ./main -m "${modelPath}" -f "${audioPath}" -l ${language} -ml 3 -oj --output-file "${jsonOutput}"`,
      { stdio: 'pipe' }
    );

    // Parse output
    const captions = parseWhisperOutput(`${jsonOutput}.json`);
    const segments = captionsToSegments(captions, fps);

    console.log(`Found ${captions.length} captions`);

    return { captions, segments };
  } finally {
    // Cleanup temp files
    if (audioPath && fs.existsSync(audioPath)) {
      fs.unlinkSync(audioPath);
    }
    if (fs.existsSync(`${jsonOutput}.json`)) {
      fs.unlinkSync(`${jsonOutput}.json`);
    }
    if (isTemp && fs.existsSync(videoPath)) {
      fs.unlinkSync(videoPath);
    }
  }
}
