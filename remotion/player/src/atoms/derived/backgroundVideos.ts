// ===============================
// Derived: backgroundVideos
// AUTO-COMPUTED from video track items
// This eliminates 8+ manual sync locations!
// ===============================

import { atom } from 'jotai';
import { videoTrackAtom } from '../tracks';
import { assetsAtom } from '../assets';

/**
 * backgroundVideosAtom - Derived atom that automatically computes
 * the list of background video URLs from the video track items.
 *
 * This replaces the manual sync code that was duplicated in:
 * - addItem
 * - updateItem
 * - deleteItems
 * - rippleDelete
 * - duplicateItems
 * - pasteItems
 * - moveItem
 * - reorderItems
 * - syncBackgroundVideosFromTimeline
 */
export const backgroundVideosAtom = atom((get) => {
  const videoTrack = get(videoTrackAtom);
  const assets = get(assetsAtom);

  if (!videoTrack || videoTrack.items.length === 0) {
    return [];
  }

  // Sort by startFrame and extract URLs
  return [...videoTrack.items]
    .sort((a, b) => a.startFrame - b.startFrame)
    .map((item) => {
      if (!item.assetId) return undefined;
      const asset = assets.find((a) => a.id === item.assetId);
      return asset?.url;
    })
    .filter((url): url is string => !!url);
});
