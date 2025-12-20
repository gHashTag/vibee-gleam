/**
 * Transcription Library
 *
 * Provides audio transcription using Whisper.cpp
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
  timestampMs: number | null;
  confidence: number | null;
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

/**
 * Download file from URL to local path
 */
async function downloadFile(url: string, destPath: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const protocol = url.startsWith('https') ? https : http;
    const file = fs.createWriteStream(destPath);

    protocol
      .get(url, (response) => {
        // Handle redirects
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
 * Extract audio from video as 16KHz WAV (required by Whisper)
 */
async function extractAudio(videoPath: string): Promise<string> {
  const audioPath = videoPath.replace(/\.[^.]+$/, '_audio.wav');

  console.log('Extracting audio from video...');
  execSync(
    `ffmpeg -i "${videoPath}" -ar 16000 -ac 1 -y "${audioPath}" 2>/dev/null`,
    { stdio: 'pipe' }
  );

  return audioPath;
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
      caption: caption.text.toUpperCase(),
      bRollUrl: type === 'split' ? brollVideos[i % brollVideos.length] : undefined,
      bRollType: 'video' as const,
    };
  });
}

/**
 * Transcribe video to captions using Whisper.cpp
 *
 * @param videoPathOrUrl - Local path or URL to video file
 * @param language - Language code (ru, en, etc.)
 * @param fps - Frames per second for segment calculation
 */
export async function transcribeVideo(
  videoPathOrUrl: string,
  language: string = 'ru',
  fps: number = 30
): Promise<TranscriptionResult> {
  const whisperPath = path.join(process.cwd(), 'whisper.cpp');

  // Check if Whisper is installed
  if (!fs.existsSync(whisperPath)) {
    throw new Error(
      'Whisper.cpp not installed. Run: npx tsx scripts/setup-whisper.ts'
    );
  }

  // Handle URLs - download to temp file
  let videoPath = videoPathOrUrl;
  let isTemp = false;

  if (videoPathOrUrl.startsWith('http://') || videoPathOrUrl.startsWith('https://')) {
    const tempDir = path.join(process.cwd(), 'uploads');
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }

    const ext = path.extname(new URL(videoPathOrUrl).pathname) || '.mp4';
    videoPath = path.join(tempDir, `temp_${Date.now()}${ext}`);
    isTemp = true;

    console.log(`Downloading video from ${videoPathOrUrl}...`);
    await downloadFile(videoPathOrUrl, videoPath);
    console.log(`Downloaded to ${videoPath}`);
  }

  // Handle local paths in public directory
  if (videoPath.startsWith('/') && !videoPath.startsWith('/Users') && !videoPath.startsWith('/app')) {
    videoPath = path.join(process.cwd(), 'public', videoPath);
  }

  if (!fs.existsSync(videoPath)) {
    throw new Error(`Video file not found: ${videoPath}`);
  }

  try {
    // Extract audio
    const audioPath = await extractAudio(videoPath);

    console.log(`Transcribing with language: ${language}...`);

    // Dynamically import to avoid TypeScript issues
    const whisperModule = await import('@remotion/install-whisper-cpp');
    const { transcribe, convertToCaptions } = whisperModule;

    // Transcribe with Whisper
    const result = await (transcribe as Function)({
      inputPath: audioPath,
      whisperPath,
      model: 'medium',
      language: language === 'ru' ? 'ru' : language === 'en' ? 'en' : null,
      tokenLevelTimestamps: true,
    });

    // Convert to Remotion Caption format
    const rawCaptions = (convertToCaptions as Function)({
      transcription: result.transcription,
      combineTokensWithinMilliseconds: 1500,
    });

    // Map to our Caption interface (startInSeconds -> startMs)
    const captions: Caption[] = rawCaptions.captions.map((c: any, i: number, arr: any[]) => {
      const startMs = Math.round(c.startInSeconds * 1000);
      // Calculate endMs from next caption's start or add 2 seconds
      const nextCaption = arr[i + 1];
      const endMs = nextCaption
        ? Math.round(nextCaption.startInSeconds * 1000) - 100
        : startMs + 2000;

      return {
        text: c.text.trim(),
        startMs,
        endMs,
        timestampMs: startMs,
        confidence: null,
      };
    });

    console.log(`Found ${captions.length} captions`);

    // Convert to segments
    const segments = captionsToSegments(captions, fps);

    // Cleanup temp files
    fs.unlinkSync(audioPath);
    if (isTemp) {
      fs.unlinkSync(videoPath);
    }

    return { captions, segments };
  } catch (error) {
    // Cleanup on error
    if (isTemp && fs.existsSync(videoPath)) {
      fs.unlinkSync(videoPath);
    }
    throw error;
  }
}
