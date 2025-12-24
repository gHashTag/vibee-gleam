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
        // Don't clear existing captions on error (they might be from transcription)
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
        console.warn('[loadCaptions] Empty or invalid captions from server, keeping existing');
        // DON'T clear - persisted captions might exist from transcription
      }
    } catch (error) {
      // Ignore abort errors
      if (error instanceof Error && error.name === 'AbortError') {
        console.log('[loadCaptions] Request aborted (expected)');
        return;
      }

      console.error('[loadCaptions] Failed:', error);
      set(captionsErrorAtom, error instanceof Error ? error.message : 'Unknown error');
      // DON'T clear captions - they might be persisted from transcription
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
      console.log('[Transcribe] Already transcribed this video, using persisted captions');
      // Captions are already persisted via atomWithStorage - no need to fetch
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

      // Update project duration (Avatar is master!)
      const currentProject = get(projectAtom);
      console.log(`[Duration] OLD project.durationInFrames: ${currentProject.durationInFrames}`);

      const newProject: typeof currentProject = {
        id: currentProject.id,
        name: currentProject.name,
        fps: currentProject.fps,
        width: currentProject.width,
        height: currentProject.height,
        durationInFrames,  // NEW VALUE
      };
      console.log(`[Duration] NEW project.durationInFrames: ${newProject.durationInFrames}`);

      // Force update both atom AND localStorage directly
      set(projectAtom, newProject);

      // Also directly update localStorage as backup (atomWithStorage sometimes doesn't sync)
      try {
        localStorage.setItem('vibee-project-v15', JSON.stringify(newProject));
        console.log('[Duration] Forced localStorage update:', newProject.durationInFrames);

        // Dispatch storage event to force any listeners to re-read
        window.dispatchEvent(new StorageEvent('storage', {
          key: 'vibee-project-v15',
          newValue: JSON.stringify(newProject),
        }));
      } catch (e) {
        console.warn('[Duration] localStorage update failed:', e);
      }

      // Update ONLY Avatar track to match lipsync - NON-DESTRUCTIVE!
      // Other tracks (Video, Audio, Text) stay UNTOUCHED
      const currentTracks = get(tracksAtom);
      const newTracks = produce(currentTracks, (draft) => {
        const avatarTrack = draft.find((t) => t.type === 'avatar');
        if (avatarTrack && avatarTrack.items.length > 0) {
          console.log(`[Duration] Avatar OLD: ${avatarTrack.items[0].durationInFrames} frames`);
          avatarTrack.items[0].durationInFrames = durationInFrames;
          avatarTrack.items[0].startFrame = 0;
          console.log(`[Duration] Avatar NEW: ${avatarTrack.items[0].durationInFrames} frames`);
        }
        // NO deletion of other tracks - professional NLE behavior!
      });
      set(tracksAtom, newTracks);

      // Force localStorage update for tracks (atomWithStorage sometimes doesn't sync)
      try {
        localStorage.setItem('vibee-tracks-v16', JSON.stringify(newTracks));
        console.log('[Duration] Forced tracks localStorage update');
        window.dispatchEvent(new StorageEvent('storage', {
          key: 'vibee-tracks-v16',
          newValue: JSON.stringify(newTracks),
        }));
      } catch (e) {
        console.warn('[Duration] tracks localStorage update failed:', e);
      }

      // Verify the update worked
      const verifyProject = get(projectAtom);
      const verifyTracks = get(tracksAtom);
      const verifyAvatar = verifyTracks.find((t) => t.type === 'avatar');
      console.log(`[Duration] VERIFY project: ${verifyProject.durationInFrames} frames`);
      console.log(`[Duration] VERIFY avatar: ${verifyAvatar?.items[0]?.durationInFrames} frames`);
      console.log(`[Duration] Project: ${durationInFrames} frames (${duration.toFixed(1)}s) - non-destructive`);
    } catch (error) {
      console.error('[Duration] Failed to get video duration:', error);
    }
  }
);
