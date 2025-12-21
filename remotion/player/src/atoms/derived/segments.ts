// ===============================
// Derived: segments
// AUTO-COMPUTED from video track items
// ===============================

import { atom } from 'jotai';
import { videoTrackAtom, tracksAtom } from '../tracks';
import { assetsAtom } from '../assets';
import { projectAtom } from '../project';
import type { Segment } from '@/store/types';

/**
 * segmentsAtom - Derived atom that automatically computes
 * timeline segments (split/fullscreen) from video track items.
 *
 * Used by:
 * - InteractiveCanvas for rendering
 * - Render server for video export
 */
export const segmentsAtom = atom((get): Segment[] => {
  const videoTrack = get(videoTrackAtom);
  const assets = get(assetsAtom);
  const project = get(projectAtom);

  if (!videoTrack || videoTrack.items.length === 0) {
    return [{
      type: 'fullscreen',
      startFrame: 0,
      durationFrames: project.durationInFrames,
    }];
  }

  const segments: Segment[] = [];
  const sortedItems = [...videoTrack.items].sort((a, b) => a.startFrame - b.startFrame);
  let lastEndFrame = 0;

  for (const item of sortedItems) {
    // Add fullscreen gap before this item
    if (item.startFrame > lastEndFrame) {
      segments.push({
        type: 'fullscreen',
        startFrame: lastEndFrame,
        durationFrames: item.startFrame - lastEndFrame,
      });
    }

    // Add split segment for B-roll
    const asset = item.assetId ? assets.find((a) => a.id === item.assetId) : null;
    segments.push({
      type: 'split',
      startFrame: item.startFrame,
      durationFrames: item.durationInFrames,
      bRollUrl: asset?.url,
      bRollType: asset?.type === 'image' ? 'image' : 'video',
    });

    lastEndFrame = item.startFrame + item.durationInFrames;
  }

  // Add final fullscreen segment if needed
  if (lastEndFrame < project.durationInFrames) {
    segments.push({
      type: 'fullscreen',
      startFrame: lastEndFrame,
      durationFrames: project.durationInFrames - lastEndFrame,
    });
  }

  return segments;
});
