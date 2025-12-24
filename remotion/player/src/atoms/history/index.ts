// ===============================
// History Atoms - Undo/Redo support
// ===============================

import { atom } from 'jotai';
import { produce } from 'immer';
import { tracksAtom } from '../tracks';
import { assetsAtom } from '../assets';
import { projectAtom } from '../project';
import { timelineZoomAtom, canvasZoomAtom } from '../ui';
import { currentFrameAtom } from '../playback';
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

interface UIStateSnapshot {
  timelineZoom: number;
  canvasZoom: number;
  currentFrame: number;
}

interface HistorySnapshot {
  tracks: Track[];
  assets: Asset[];
  project: Project;
  templateProps: TemplatePropsSnapshot;
  uiState?: UIStateSnapshot;
  timestamp: number;
}

// ===============================
// History State
// ===============================

const MAX_HISTORY = 50;

const pastAtom = atom<HistorySnapshot[]>([]);
const futureAtom = atom<HistorySnapshot[]>([]);
const isApplyingAtom = atom(false);

// ===============================
// Snapshot Helpers
// ===============================

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

function createUIStateSnapshot(get: any): UIStateSnapshot {
  return {
    timelineZoom: get(timelineZoomAtom),
    canvasZoom: get(canvasZoomAtom),
    currentFrame: get(currentFrameAtom),
  };
}

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

function applyUIStateSnapshot(set: any, uiState: UIStateSnapshot) {
  set(timelineZoomAtom, uiState.timelineZoom);
  set(canvasZoomAtom, uiState.canvasZoom);
  set(currentFrameAtom, uiState.currentFrame);
}

// Check if snapshots are identical (for skip logic)
function isSnapshotIdentical(a: HistorySnapshot, b: HistorySnapshot): boolean {
  return JSON.stringify(a.tracks) === JSON.stringify(b.tracks) &&
         JSON.stringify(a.assets) === JSON.stringify(b.assets) &&
         JSON.stringify(a.project) === JSON.stringify(b.project) &&
         JSON.stringify(a.templateProps) === JSON.stringify(b.templateProps);
}

// ===============================
// Record Snapshot
// ===============================

export const recordSnapshotAtom = atom(
  null,
  (get, set) => {
    if (get(isApplyingAtom)) {
      console.log('[History] Skipping record - applying in progress');
      return;
    }

    const snapshot: HistorySnapshot = {
      tracks: JSON.parse(JSON.stringify(get(tracksAtom))),
      assets: JSON.parse(JSON.stringify(get(assetsAtom))),
      project: JSON.parse(JSON.stringify(get(projectAtom))),
      templateProps: createTemplatePropsSnapshot(get),
      uiState: createUIStateSnapshot(get),
      timestamp: Date.now(),
    };

    const past = get(pastAtom);
    const lastSnapshot = past[past.length - 1];

    // Skip if COMPLETELY identical to last snapshot
    if (lastSnapshot && isSnapshotIdentical(lastSnapshot, snapshot)) {
      return;
    }

    console.log('[History] Recording snapshot', {
      pastLength: past.length + 1,
      tracks: snapshot.tracks.length,
      templateProps: {
        captionStyle: snapshot.templateProps.captionStyle.fontSize,
        vignetteStrength: snapshot.templateProps.vignetteStrength,
      }
    });

    // ALWAYS create new snapshot (no debounce overwrite!)
    set(pastAtom, produce(past, (draft) => {
      draft.push(snapshot);
      if (draft.length > MAX_HISTORY) draft.shift();
    }));
    set(futureAtom, []);
  }
);

// ===============================
// Undo Action
// ===============================

export const undoAtom = atom(
  null,
  (get, set) => {
    const past = get(pastAtom);
    console.log('[History] Undo called', { pastLength: past.length });

    if (past.length === 0) {
      console.log('[History] Nothing to undo');
      return;
    }

    set(isApplyingAtom, true);

    // Save current state to future
    const currentSnapshot: HistorySnapshot = {
      tracks: JSON.parse(JSON.stringify(get(tracksAtom))),
      assets: JSON.parse(JSON.stringify(get(assetsAtom))),
      project: JSON.parse(JSON.stringify(get(projectAtom))),
      templateProps: createTemplatePropsSnapshot(get),
      uiState: createUIStateSnapshot(get),
      timestamp: Date.now(),
    };
    set(futureAtom, produce(get(futureAtom), (draft) => {
      draft.unshift(currentSnapshot);
    }));

    // Pop from past and apply
    const newPast = [...past];
    const snapshot = newPast.pop()!;
    set(pastAtom, newPast);

    console.log('[History] Applying snapshot', {
      tracksCount: snapshot.tracks.length,
      templateProps: {
        captionStyle: snapshot.templateProps?.captionStyle?.fontSize,
        vignetteStrength: snapshot.templateProps?.vignetteStrength,
      }
    });

    set(tracksAtom, snapshot.tracks);
    set(assetsAtom, snapshot.assets);
    set(projectAtom, snapshot.project);

    // Restore template props
    if (snapshot.templateProps) {
      applyTemplatePropsSnapshot(set, snapshot.templateProps);
    }

    // Restore UI state (optional - for backwards compatibility)
    if (snapshot.uiState) {
      applyUIStateSnapshot(set, snapshot.uiState);
    }

    set(isApplyingAtom, false);
    console.log('[History] Undo applied');
  }
);

// ===============================
// Redo Action
// ===============================

export const redoAtom = atom(
  null,
  (get, set) => {
    const future = get(futureAtom);
    console.log('[History] Redo called', { futureLength: future.length });

    if (future.length === 0) {
      console.log('[History] Nothing to redo');
      return;
    }

    set(isApplyingAtom, true);

    // Save current state to past
    const currentSnapshot: HistorySnapshot = {
      tracks: JSON.parse(JSON.stringify(get(tracksAtom))),
      assets: JSON.parse(JSON.stringify(get(assetsAtom))),
      project: JSON.parse(JSON.stringify(get(projectAtom))),
      templateProps: createTemplatePropsSnapshot(get),
      uiState: createUIStateSnapshot(get),
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

    // Restore template props
    if (snapshot.templateProps) {
      applyTemplatePropsSnapshot(set, snapshot.templateProps);
    }

    // Restore UI state (optional)
    if (snapshot.uiState) {
      applyUIStateSnapshot(set, snapshot.uiState);
    }

    set(isApplyingAtom, false);
    console.log('[History] Redo applied');
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
    console.log('[History] Clearing history');
    set(pastAtom, []);
    set(futureAtom, []);
  }
);

export { isApplyingAtom };
