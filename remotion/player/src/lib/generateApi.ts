// ===============================
// AI Generation API Client
// Calls render-server endpoints which proxy to Gleam MCP
// ===============================

const RENDER_SERVER_URL = import.meta.env.VITE_RENDER_SERVER_URL || 'https://vibee-remotion.fly.dev';

export interface GenerateImageParams {
  model: string;       // 'flux-pro-1.1', 'flux-dev', 'flux-lora'
  prompt: string;
  aspectRatio: string; // '1:1', '16:9', '9:16', '4:3'
}

export interface GenerateVideoParams {
  model: string;       // 'kling-std', 'kling-pro', 'veo3-fast', 'veo3-quality'
  prompt: string;
  duration: string;    // '5s', '10s'
  aspectRatio: string;
}

export interface GenerateAudioParams {
  text: string;
  voiceId: string;     // 'sarah', 'rachel', 'josh', 'adam', 'bella'
  speed: number;       // 0.5 - 2.0
}

export interface GenerateLipsyncParams {
  audioUrl: string;
  imageUrl: string;
  resolution: string;  // '480p', '720p', '1080p'
  aspectRatio: string;
}

export interface GenerateResult {
  success: boolean;
  url?: string;
  id?: string;
  error?: string;
}

// Helper for aspect ratio to dimensions
function aspectRatioToDimensions(ratio: string): { width: number; height: number } {
  const ratios: Record<string, { width: number; height: number }> = {
    '1:1': { width: 1024, height: 1024 },
    '16:9': { width: 1280, height: 720 },
    '9:16': { width: 720, height: 1280 },
    '4:3': { width: 1024, height: 768 },
  };
  return ratios[ratio] || ratios['16:9'];
}

/**
 * Generate an image using FLUX AI
 */
export async function generateImage(params: GenerateImageParams): Promise<GenerateResult> {
  const { width, height } = aspectRatioToDimensions(params.aspectRatio);

  const response = await fetch(`${RENDER_SERVER_URL}/api/generate/image`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      model: params.model,
      prompt: params.prompt,
      width,
      height,
    }),
  });

  if (!response.ok) {
    const text = await response.text();
    return { success: false, error: text || 'Image generation failed' };
  }

  return response.json();
}

/**
 * Generate a video using Kling/Veo3 AI
 */
export async function generateVideo(params: GenerateVideoParams): Promise<GenerateResult> {
  const response = await fetch(`${RENDER_SERVER_URL}/api/generate/video`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      model: params.model,
      prompt: params.prompt,
      duration: params.duration,
      aspect_ratio: params.aspectRatio,
    }),
  });

  if (!response.ok) {
    const text = await response.text();
    return { success: false, error: text || 'Video generation failed' };
  }

  return response.json();
}

/**
 * Generate audio using ElevenLabs TTS
 */
export async function generateAudio(params: GenerateAudioParams): Promise<GenerateResult> {
  const response = await fetch(`${RENDER_SERVER_URL}/api/generate/audio`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      text: params.text,
      voice_id: params.voiceId,
      speed: params.speed,
    }),
  });

  if (!response.ok) {
    const text = await response.text();
    return { success: false, error: text || 'Audio generation failed' };
  }

  return response.json();
}

/**
 * Generate lipsync video using Hedra
 */
export async function generateLipsync(params: GenerateLipsyncParams): Promise<GenerateResult> {
  const response = await fetch(`${RENDER_SERVER_URL}/api/generate/lipsync`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      audio_url: params.audioUrl,
      image_url: params.imageUrl,
      resolution: params.resolution,
      aspect_ratio: params.aspectRatio,
    }),
  });

  if (!response.ok) {
    const text = await response.text();
    return { success: false, error: text || 'Lipsync generation failed' };
  }

  return response.json();
}
