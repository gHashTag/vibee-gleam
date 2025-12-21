// ===============================
// VIBEE Editor - Jotai Atoms
// Single Source of Truth Architecture
// ===============================

// Core Atoms
export { projectAtom } from './project';

export {
  tracksAtom,
  videoTrackAtom,
  avatarTrackAtom,
  audioTrackAtom,
  textTrackAtom,
  getTrackByIdAtom,
  getItemByIdAtom,
  // Track Actions
  addTrackAtom,
  removeTrackAtom,
  updateTrackAtom,
  // Item Actions
  addItemAtom,
  updateItemAtom,
  deleteItemsAtom,
  moveItemAtom,
  resizeItemAtom,
  splitItemAtom,
  duplicateItemsAtom,
  moveItemToTrackAtom,
  rippleDeleteAtom,
  reorderItemsAtom,
  resetTracksAtom,
} from './tracks';

export {
  assetsAtom,
  addAssetAtom,
  removeAssetAtom,
  getAssetByIdAtom,
  DEFAULT_ASSETS,
  DEFAULT_ASSET_IDS,
} from './assets';

// Playback Atoms
export {
  currentFrameAtom,
  isPlayingAtom,
  isMutedAtom,
  volumeAtom,
  playbackRateAtom,
  playerRefAtom,
  setCurrentFrameAtom,
  playAtom,
  pauseAtom,
  seekToAtom,
  togglePlayAtom,
} from './playback';

// Selection Atoms
export {
  selectedItemIdsAtom,
  selectionAnchorAtom,
  clipboardAtom,
  selectItemsAtom,
  clearSelectionAtom,
  copyItemsAtom,
  // New atoms
  getSelectedItemsAtom,
  selectAllAtom,
  selectRangeAtom,
  pasteItemsAtom,
} from './selection';

// UI Atoms
export {
  sidebarTabAtom,
  canvasZoomAtom,
  timelineZoomAtom,
  snapSettingsAtom,
  inPointAtom,
  outPointAtom,
  markersAtom,
  isExportingAtom,
  exportProgressAtom,
  setSnapEnabledAtom,
  setSnapIntervalAtom,
  addMarkerAtom,
  removeMarkerAtom,
  clearInOutPointsAtom,
  setExportingAtom,
  // Marker navigation
  goToNextMarkerAtom,
  goToPrevMarkerAtom,
} from './ui';

// Derived Atoms (THE KEY INNOVATION)
export {
  backgroundVideosAtom,
  segmentsAtom,
  templatePropsAtom,
  updateTemplatePropAtom,
  // Primitive template props
  lipSyncVideoAtom,
  coverImageAtom,
  backgroundMusicAtom,
  musicVolumeAtom,
  coverDurationAtom,
  vignetteStrengthAtom,
  colorCorrectionAtom,
  circleSizePercentAtom,
  circleBottomPercentAtom,
  circleLeftPxAtom,
  faceOffsetXAtom,
  faceOffsetYAtom,
  faceScaleAtom,
  // Captions
  captionsAtom,
  captionStyleAtom,
  showCaptionsAtom,
} from './derived';

// History Atoms
export {
  undoAtom,
  redoAtom,
  canUndoAtom,
  canRedoAtom,
  recordSnapshotAtom,
  clearHistoryAtom,
} from './history';

// Caption Loading (with AbortController fix)
export {
  loadCaptionsAtom,
  captionsLoadingAtom,
  captionsErrorAtom,
  updateDurationFromLipSyncAtom,
} from './captions';
