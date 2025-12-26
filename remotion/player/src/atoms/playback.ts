// ===============================
// Playback Atoms - Player state
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import type { PlayerRef } from '@remotion/player';
import type React from 'react';
import { STORAGE_KEYS } from './storageKeys';

// Current frame position
export const currentFrameAtom = atom(0);

// Playback state
export const isPlayingAtom = atom(false);

// Audio state
export const isMutedAtom = atom(false);
export const volumeAtom = atom(1); // 0-1

// Playback rate (persisted)
export const playbackRateAtom = atomWithStorage(STORAGE_KEYS.playbackRate, 1);

// Player ref for direct control
export const playerRefAtom = atom<React.RefObject<PlayerRef | null> | null>(null);

// Bounded current frame setter
export const setCurrentFrameAtom = atom(
  null,
  (get, set, frame: number) => {
    // Will be bounded by project duration in component
    set(currentFrameAtom, Math.max(0, frame));
  }
);

// Play action (with direct player control)
export const playAtom = atom(
  null,
  (get, set, event?: React.MouseEvent) => {
    const playerRef = get(playerRefAtom);
    if (playerRef?.current) {
      const player = playerRef.current;
      if (player.unmute) player.unmute();
      if (player.setVolume) player.setVolume(1);
      player.play(event);
    }
    set(isPlayingAtom, true);
  }
);

// Pause action
export const pauseAtom = atom(
  null,
  (get, set) => {
    const playerRef = get(playerRefAtom);
    if (playerRef?.current) {
      playerRef.current.pause();
    }
    set(isPlayingAtom, false);
  }
);

// Seek action
export const seekToAtom = atom(
  null,
  (get, set, frame: number) => {
    set(currentFrameAtom, Math.max(0, frame));
    set(isPlayingAtom, false);
  }
);

// Toggle play/pause
export const togglePlayAtom = atom(
  null,
  (get, set, event?: React.MouseEvent) => {
    const isPlaying = get(isPlayingAtom);
    if (isPlaying) {
      set(pauseAtom);
    } else {
      set(playAtom, event);
    }
  }
);
