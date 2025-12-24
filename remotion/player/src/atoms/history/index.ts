// ===============================
// History Atoms - Undo/Redo support
// ===============================

import { atom } from 'jotai';
import { produce } from 'immer';
import { tracksAtom } from '../tracks';
import { assetsAtom } from '../assets';
import { projectAtom } from '../project';
import {
  captionsAtom,
  captionStyleAtom,
  showCaptionsAtom,
  lipSyncVideoAtom,
  backgroundMusicAtom,
  musicVolumeAtom,
  coverImageAtom,
  coverDurationAtom,
  vignetteStrengthAtom,
  colorCorrectionAtom,
  circleSizePercentAtom,
  circleBottomPercentAtom,
  circleLeftPxAtom,
  faceOffsetXAtom,
  faceOffsetYAtom,
  faceScaleAtom,
} from '../derived/templateProps';
import type { Track, Asset, Project, CaptionItem, CaptionStyle } from '@/store/types';

// ===============================
// Snapshot Types
// ===============================

interface TemplatePropsSnapshot {
  captions: CaptionItem[];
  captionStyle: CaptionStyle;
  showCaptions: boolean;
  lipSyncVideo: string;
  backgroundMusic: string;
  musicVolume: number;
  coverImage: string;
  coverDuration: number;
  vignetteStrength: number;
  colorCorrection: number;
  circleSizePercent: number;
  circleBottomPercent: number;
  circleLeftPx: number;
  faceOffsetX: number | undefined;
  faceOffsetY: number | undefined;
  faceScale: number | undefined;
}

interface HistorySnapshot {
  tracks: Track[];
  assets: Asset[];
  project: Project;
  templateProps: TemplatePropsSnapshot;
  timestamp: number;
}

// ===============================
// History State
// ===============================

const MAX_HISTORY = 50;

const pastAtom = atom<HistorySnapshot[]>([]);
const futureAtom = atom<HistorySnapshot[]>([]);
const isApplyingAtom = atom(false);

// Debounce timer for recording
let debounceTimer: ReturnType<typeof setTimeout> | null = null;

// ===============================
// Record Snapshot
// ===============================

// Helper to create templateProps snapshot
function createTemplatePropsSnapshot(get: any): TemplatePropsSnapshot {
  return {
    captions: JSON.parse(JSON.stringify(get(captionsAtom))),
    captionStyle: JSON.parse(JSON.stringify(get(captionStyleAtom))),
    showCaptions: get(showCaptionsAtom),
    lipSyncVideo: get(lipSyncVideoAtom),
    backgroundMusic: get(backgroundMusicAtom),
    musicVolume: get(musicVolumeAtom),
    coverImage: get(coverImageAtom),
    coverDuration: get(coverDurationAtom),
    vignetteStrength: get(vignetteStrengthAtom),
    colorCorrection: get(colorCorrectionAtom),
    circleSizePercent: get(circleSizePercentAtom),
    circleBottomPercent: get(circleBottomPercentAtom),
    circleLeftPx: get(circleLeftPxAtom),
    faceOffsetX: get(faceOffsetXAtom),
    faceOffsetY: get(faceOffsetYAtom),
    faceScale: get(faceScaleAtom),
  };
}

// Helper to apply templateProps snapshot
function applyTemplatePropsSnapshot(set: any, templateProps: TemplatePropsSnapshot) {
  set(captionsAtom, templateProps.captions);
  set(captionStyleAtom, templateProps.captionStyle);
  set(showCaptionsAtom, templateProps.showCaptions);
  set(lipSyncVideoAtom, templateProps.lipSyncVideo);
  set(backgroundMusicAtom, templateProps.backgroundMusic);
  set(musicVolumeAtom, templateProps.musicVolume);
  set(coverImageAtom, templateProps.coverImage);
  set(coverDurationAtom, templateProps.coverDuration);
  set(vignetteStrengthAtom, templateProps.vignetteStrength);
  set(colorCorrectionAtom, templateProps.colorCorrection);
  set(circleSizePercentAtom, templateProps.circleSizePercent);
  set(circleBottomPercentAtom, templateProps.circleBottomPercent);
  set(circleLeftPxAtom, templateProps.circleLeftPx);
  set(faceOffsetXAtom, templateProps.faceOffsetX);
  set(faceOffsetYAtom, templateProps.faceOffsetY);
  set(faceScaleAtom, templateProps.faceScale);
}

export const recordSnapshotAtom = atom(
  null,
  (get, set) => {
    if (get(isApplyingAtom)) return;

    const snapshot: HistorySnapshot = {
      tracks: JSON.parse(JSON.stringify(get(tracksAtom))),
      assets: JSON.parse(JSON.stringify(get(assetsAtom))),
      project: JSON.parse(JSON.stringify(get(projectAtom))),
      templateProps: createTemplatePropsSnapshot(get),
      timestamp: Date.now(),
    };

    const past = get(pastAtom);
    const lastSnapshot = past[past.length - 1];

    // Skip if identical to last snapshot (include templateProps)
    if (lastSnapshot &&
        JSON.stringify(lastSnapshot.tracks) === JSON.stringify(snapshot.tracks) &&
        JSON.stringify(lastSnapshot.assets) === JSON.stringify(snapshot.assets) &&
        JSON.stringify(lastSnapshot.templateProps) === JSON.stringify(snapshot.templateProps)) {
      return;
    }

    // For first snapshot or if enough time passed - save immediately
    const now = Date.now();
    const timeSinceLastSnapshot = lastSnapshot ? now - lastSnapshot.timestamp : Infinity;

    if (past.length === 0 || timeSinceLastSnapshot > 500) {
      // Record immediately for first snapshot or after 500ms gap
      set(pastAtom, produce(past, (draft) => {
        draft.push(snapshot);
        if (draft.length > MAX_HISTORY) draft.shift();
      }));
      set(futureAtom, []);
    } else {
      // Debounce rapid changes (update last snapshot instead of adding new)
      if (debounceTimer) clearTimeout(debounceTimer);

      debounceTimer = setTimeout(() => {
        const currentPast = get(pastAtom);
        set(pastAtom, produce(currentPast, (draft) => {
          // Update the last snapshot with current state
          if (draft.length > 0) {
            draft[draft.length - 1] = snapshot;
          } else {
            draft.push(snapshot);
          }
        }));
        set(futureAtom, []);
      }, 200);
    }
  }
);

// ===============================
// Undo Action
// ===============================

export const undoAtom = atom(
  null,
  (get, set) => {
    const past = get(pastAtom);
    if (past.length === 0) return;

    set(isApplyingAtom, true);

    // Save current state to future
    const currentSnapshot: HistorySnapshot = {
      tracks: JSON.parse(JSON.stringify(get(tracksAtom))),
      assets: JSON.parse(JSON.stringify(get(assetsAtom))),
      project: JSON.parse(JSON.stringify(get(projectAtom))),
      templateProps: createTemplatePropsSnapshot(get),
      timestamp: Date.now(),
    };
    set(futureAtom, produce(get(futureAtom), (draft) => {
      draft.unshift(currentSnapshot);
    }));

    // Pop from past and apply
    const newPast = [...past];
    const snapshot = newPast.pop()!;
    set(pastAtom, newPast);

    set(tracksAtom, snapshot.tracks);
    set(assetsAtom, snapshot.assets);
    set(projectAtom, snapshot.project);

    // Restore template props if available (for backwards compatibility)
    if (snapshot.templateProps) {
      applyTemplatePropsSnapshot(set, snapshot.templateProps);
    }

    set(isApplyingAtom, false);
  }
);

// ===============================
// Redo Action
// ===============================

export const redoAtom = atom(
  null,
  (get, set) => {
    const future = get(futureAtom);
    if (future.length === 0) return;

    set(isApplyingAtom, true);

    // Save current state to past
    const currentSnapshot: HistorySnapshot = {
      tracks: JSON.parse(JSON.stringify(get(tracksAtom))),
      assets: JSON.parse(JSON.stringify(get(assetsAtom))),
      project: JSON.parse(JSON.stringify(get(projectAtom))),
      templateProps: createTemplatePropsSnapshot(get),
      timestamp: Date.now(),
    };
    set(pastAtom, produce(get(pastAtom), (draft) => {
      draft.push(currentSnapshot);
    }));

    // Pop from future and apply
    const newFuture = [...future];
    const snapshot = newFuture.shift()!;
    set(futureAtom, newFuture);

    set(tracksAtom, snapshot.tracks);
    set(assetsAtom, snapshot.assets);
    set(projectAtom, snapshot.project);

    // Restore template props if available (for backwards compatibility)
    if (snapshot.templateProps) {
      applyTemplatePropsSnapshot(set, snapshot.templateProps);
    }

    set(isApplyingAtom, false);
  }
);

// ===============================
// Can Undo/Redo Selectors
// ===============================

export const canUndoAtom = atom((get) => get(pastAtom).length > 0);
export const canRedoAtom = atom((get) => get(futureAtom).length > 0);

// ===============================
// Clear History
// ===============================

export const clearHistoryAtom = atom(
  null,
  (get, set) => {
    set(pastAtom, []);
    set(futureAtom, []);
  }
);

export { isApplyingAtom };
