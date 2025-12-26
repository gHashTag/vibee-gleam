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
  imageTrackAtom,
  getTrackByIdAtom,
  getItemByIdAtom,
  // Track Migration
  ensureAudioTrackAtom,
  ensureImageTrackAtom,
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
  updateItemLayoutAtom,
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
  type SidebarTab,
  canvasZoomAtom,
  timelineZoomAtom,
  snapSettingsAtom,
  inPointAtom,
  outPointAtom,
  markersAtom,
  isExportingAtom,
  exportProgressAtom,
  volumePopupItemIdAtom,
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
  forceRefreshAtom, // Force re-render when agent updates props
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
  circleLeftPercentAtom,
  faceOffsetXAtom,
  faceOffsetYAtom,
  faceScaleAtom,
  // Circle avatar
  isCircleAvatarAtom,
  avatarBorderRadiusAtom,
  // Split/Fullscreen mode settings
  avatarSettingsTabAtom,
  splitCircleSizeAtom,
  splitPositionXAtom,
  splitPositionYAtom,
  splitFaceScaleAtom,
  splitIsCircleAtom,
  splitBorderRadiusAtom,
  fullscreenCircleSizeAtom,
  fullscreenPositionXAtom,
  fullscreenPositionYAtom,
  fullscreenFaceScaleAtom,
  fullscreenIsCircleAtom,
  fullscreenBorderRadiusAtom,
  // Animation
  avatarAnimationAtom,
  // Border effect
  avatarBorderEffectAtom,
  avatarBorderColorAtom,
  avatarBorderColor2Atom,
  avatarBorderWidthAtom,
  avatarBorderIntensityAtom,
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
  transcribeVideoAtom,
  transcribingAtom,
} from './captions';

// Templates Atoms
export {
  templatesAtom,
  selectedTemplateIdAtom,
  selectedTemplateAtom,
  selectTemplateAtom,
  type Template,
} from './templates';

// User & Auth Atoms (Freemium)
export {
  userAtom,
  renderQuotaAtom,
  quotaLoadingAtom,
  showPaywallAtom,
  showLoginModalAtom,
  fetchQuotaAtom,
  logRenderAtom,
  canRenderAtom,
  logoutAtom,
  type TelegramUser,
  type RenderQuota,
  type SubscriptionInfo,
} from './user';
