// ===============================
// Captions Atoms - With AbortController for race condition fix
// ===============================

import { atom } from 'jotai';
import { captionsAtom, lipSyncVideoAtom } from './derived/templateProps';
import { projectAtom } from './project';
import { tracksAtom } from './tracks';
import { produce } from 'immer';
import type { CaptionItem } from '@/store/types';

// ===============================
// Loading State
// ===============================

export const captionsLoadingAtom = atom(false);
export const captionsErrorAtom = atom<string | null>(null);

// AbortController for canceling in-flight requests
const abortControllerAtom = atom<AbortController | null>(null);

// ===============================
// Load Captions Action
// ===============================

const RENDER_SERVER_URL = typeof window !== 'undefined'
  ? (import.meta.env?.VITE_RENDER_SERVER_URL || 'https://vibee-remotion.fly.dev')
  : 'https://vibee-remotion.fly.dev';

export const loadCaptionsAtom = atom(
  null,
  async (get, set, lipSyncVideo?: string) => {
    const videoUrl = lipSyncVideo ?? get(lipSyncVideoAtom);

    console.log('[loadCaptions] Starting for:', videoUrl);

    if (!videoUrl) {
      console.log('[loadCaptions] No video URL, clearing captions');
      set(captionsAtom, []);
      return;
    }

    // Cancel previous request (prevents race condition!)
    const prevController = get(abortControllerAtom);
    if (prevController) {
      console.log('[loadCaptions] Aborting previous request');
      prevController.abort();
    }

    // Create new AbortController
    const controller = new AbortController();
    set(abortControllerAtom, controller);
    set(captionsLoadingAtom, true);
    set(captionsErrorAtom, null);

    try {
      // Build captions URL (same directory as lipsync video)
      const videoDir = videoUrl.substring(0, videoUrl.lastIndexOf('/'));
      const captionsPath = `${videoDir}/captions.json`;
      const fullUrl = captionsPath.startsWith('/')
        ? `${RENDER_SERVER_URL}${captionsPath}`
        : captionsPath;

      // Cache-busting
      const urlWithCacheBust = `${fullUrl}?_=${Date.now()}`;
      console.log('[loadCaptions] Fetching:', urlWithCacheBust);

      const response = await fetch(urlWithCacheBust, {
        signal: controller.signal,
      });

      // Check if aborted during fetch
      if (controller.signal.aborted) {
        console.log('[loadCaptions] Request was aborted');
        return;
      }

      if (!response.ok) {
        console.warn('[loadCaptions] No captions.json found at:', fullUrl);
        set(captionsAtom, []);
        set(captionsLoadingAtom, false);
        return;
      }

      const captions: CaptionItem[] = await response.json();

      // Check if aborted during parsing
      if (controller.signal.aborted) {
        console.log('[loadCaptions] Request was aborted after parsing');
        return;
      }

      if (Array.isArray(captions) && captions.length > 0) {
        console.log(`[loadCaptions] Loaded ${captions.length} captions`);
        set(captionsAtom, captions);
      } else {
        console.warn('[loadCaptions] Empty or invalid captions');
        set(captionsAtom, []);
      }
    } catch (error) {
      // Ignore abort errors
      if (error instanceof Error && error.name === 'AbortError') {
        console.log('[loadCaptions] Request aborted (expected)');
        return;
      }

      console.error('[loadCaptions] Failed:', error);
      set(captionsErrorAtom, error instanceof Error ? error.message : 'Unknown error');
      set(captionsAtom, []);
    } finally {
      // Only update loading state if not aborted
      if (!controller.signal.aborted) {
        set(captionsLoadingAtom, false);
      }
    }
  }
);

// ===============================
// Transcribe Video Action
// ===============================

export const transcribingAtom = atom(false);

export const transcribeVideoAtom = atom(
  null,
  async (get, set, options?: { force?: boolean }) => {
    const lipSyncVideo = get(lipSyncVideoAtom);
    const project = get(projectAtom);

    // Skip default video
    if (!lipSyncVideo || lipSyncVideo === '/lipsync/lipsync.mp4') {
      console.log('[Transcribe] Skipping default video');
      return;
    }

    // Check localStorage to prevent re-transcription on page refresh
    const lastTranscribed = localStorage.getItem('vibee-last-transcribed-video');
    if (!options?.force && lastTranscribed === lipSyncVideo) {
      console.log('[Transcribe] Already transcribed this video, loading captions');
      set(loadCaptionsAtom, lipSyncVideo);
      return;
    }

    console.log('[Transcribe] Starting transcription for:', lipSyncVideo);
    set(transcribingAtom, true);
    set(captionsAtom, []); // Clear old captions

    try {
      const response = await fetch(`${RENDER_SERVER_URL}/transcribe`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          videoUrl: lipSyncVideo,
          language: 'ru',
          fps: project.fps,
        }),
      });

      const result = await response.json();

      if (result.success && result.captions) {
        set(captionsAtom, result.captions);
        localStorage.setItem('vibee-last-transcribed-video', lipSyncVideo);
        console.log(`[Transcribe] Loaded ${result.captions.length} captions`);
      } else {
        throw new Error(result.error || 'Transcription failed');
      }
    } catch (error) {
      console.error('[Transcribe] Failed:', error);
      set(captionsErrorAtom, error instanceof Error ? error.message : 'Unknown error');
    } finally {
      set(transcribingAtom, false);
    }
  }
);

// ===============================
// Update Video Duration Action
// ===============================

export const updateDurationFromLipSyncAtom = atom(
  null,
  async (get, set) => {
    const videoUrl = get(lipSyncVideoAtom);
    const project = get(projectAtom);

    if (!videoUrl) return;

    try {
      console.log('[Duration] Getting duration for:', videoUrl);

      const duration = await new Promise<number>((resolve, reject) => {
        const video = document.createElement('video');
        video.preload = 'metadata';

        video.onloadedmetadata = () => {
          resolve(video.duration);
          video.remove();
        };

        video.onerror = () => {
          reject(new Error(`Failed to load video: ${videoUrl}`));
          video.remove();
        };

        // Handle relative URLs
        if (videoUrl.startsWith('/')) {
          video.src = `${RENDER_SERVER_URL}${videoUrl}`;
        } else {
          video.src = videoUrl;
        }
      });

      const fps = project.fps;
      const durationInFrames = Math.ceil(duration * fps);

      console.log(`[Duration] Video: ${duration.toFixed(2)}s = ${durationInFrames} frames`);

      // Update project duration
      set(projectAtom, { ...project, durationInFrames });

      // Also update Avatar track duration
      set(tracksAtom, produce(get(tracksAtom), (draft) => {
        const avatarTrack = draft.find((t) => t.type === 'avatar');
        if (avatarTrack && avatarTrack.items.length > 0) {
          avatarTrack.items[0].durationInFrames = durationInFrames;
        }
      }));
    } catch (error) {
      console.error('[Duration] Failed to get video duration:', error);
    }
  }
);
