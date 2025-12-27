// ===============================
// VIBEE Editor - Jotai Atoms
// Single Source of Truth Architecture
// ===============================
//
// 📁 Structure:
// ├── Core: project, tracks, assets, playback
// ├── UI: sidebar, selection, zoom, markers
// ├── Template: media, effects, avatar, captions
// ├── History: undo/redo
// ├── User: auth, quota
// └── Features: templates, feed, voices
//
// 🔧 Key patterns:
// - atomWithStorage for persistence
// - Derived atoms for computed values
// - Action atoms for complex updates
// ===============================

// ===============================
// 🎬 CORE - Editor fundamentals
// ===============================

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
  reorderTracksAtom,
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
  // Batch selection
  assetSelectionModeAtom,
  selectedAssetIdsAtom,
  toggleSelectionModeAtom,
  toggleAssetSelectionAtom,
  clearAssetSelectionAtom,
} from './assets';

// ===============================
// ▶️ PLAYBACK - Player controls
// ===============================

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

// ===============================
// 🎯 SELECTION - Item selection
// ===============================

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

// ===============================
// 🖥️ UI - Interface state
// ===============================

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

// ===============================
// 🎨 TEMPLATE - Video composition props
// (Auto-derived from atoms, passed to Remotion)
// ===============================

export {
  backgroundVideosAtom,
  segmentsAtom,
  templatePropsAtom,
  updateTemplatePropAtom,
  type TemplatePropKey, // Type-safe keys for updateTemplatePropAtom
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
  // Split/Fullscreen mode settings (CONSOLIDATED)
  avatarSettingsTabAtom,
  type AvatarModeSettings,
  splitAvatarSettingsAtom,
  fullscreenAvatarSettingsAtom,
  // Derived selector atoms (for UI compatibility)
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

// ===============================
// ↩️ HISTORY - Undo/Redo
// ===============================

export {
  undoAtom,
  redoAtom,
  canUndoAtom,
  canRedoAtom,
  recordSnapshotAtom,
  clearHistoryAtom,
} from './history';

// ===============================
// 💬 CAPTIONS - Transcription & loading
// ===============================

export {
  loadCaptionsAtom,
  captionsLoadingAtom,
  captionsErrorAtom,
  updateDurationFromLipSyncAtom,
  transcribeVideoAtom,
  transcribingAtom,
} from './captions';

// ===============================
// 📋 TEMPLATES - Saved presets
// ===============================

export {
  templatesAtom,
  selectedTemplateIdAtom,
  selectedTemplateAtom,
  selectTemplateAtom,
  addTemplateAtom,
  removeTemplateAtom,
  // Per-template settings
  templateSettingsAtom,
  saveCurrentSettingsAtom,
  type Template,
  type TemplateSettings,
} from './templates';

// ===============================
// 👤 USER - Auth & subscription
// ===============================

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

// ===============================
// 📱 FEED - Social templates
// ===============================

export {
  feedTemplatesAtom,
  feedLoadingAtom,
  feedErrorAtom,
  feedPageAtom,
  feedHasMoreAtom,
  feedSortAtom,
  feedMutedAtom,
  loadFeedAtom,
  loadMoreFeedAtom,
  changeFeedSortAtom,
  likeTemplateAtom,
  trackViewAtom,
  deleteTemplateAtom,
  useTemplateAtom,
  publishToFeedAtom,
  currentRemixSourceAtom,
  type FeedTemplate,
  type FeedSort,
  type PublishData,
  type RemixSource,
} from './feed';

// ===============================
// 👥 PROFILE - User profiles & follows
// ===============================

export {
  // State
  viewedProfileAtom,
  profileLoadingAtom,
  profileErrorAtom,
  myProfileAtom,
  followersAtom,
  followersLoadingAtom,
  followingAtom,
  followingLoadingAtom,
  followingFeedAtom,
  followingFeedLoadingAtom,
  // Actions
  loadProfileAtom,
  fetchMyProfileAtom,
  updateProfileAtom,
  followUserAtom,
  unfollowUserAtom,
  loadFollowersAtom,
  loadFollowingAtom,
  loadFollowingFeedAtom,
  clearProfileAtom,
  // Types
  type UserProfile,
  type FollowUser,
  type SocialLink,
} from './profile';

// ===============================
// 📦 ASSET BROWSER - Horizontal browser state
// ===============================

export {
  browserCategoryAtom,
  browserSearchAtom,
  browserUploadingAtom,
  browserUploadProgressAtom,
  filteredAssetsAtom,
  setBrowserCategoryAtom,
  setBrowserSearchAtom,
  clearBrowserFiltersAtom,
  categoryCounts,
  CATEGORY_CONFIG,
  type AssetCategory,
} from './assetBrowser';
