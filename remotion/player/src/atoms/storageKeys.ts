// ===============================
// Storage Keys Registry
// Centralized localStorage key management
// ===============================

/**
 * All localStorage keys used by the app.
 * IMPORTANT: When changing data structure, increment version suffix (e.g., -v2 → -v3)
 *
 * Naming convention:
 * - Prefix: vibee-
 * - Category: lipsync-, avatar-, split-, fullscreen-, caption-, etc.
 * - Version suffix: -v2, -v3 (only when breaking changes)
 */
export const STORAGE_KEYS = {
  // ===============================
  // Media
  // ===============================
  lipSyncVideo: 'vibee-lipsync-video',
  coverImage: 'vibee-cover-image',
  backgroundMusic: 'vibee-background-music',
  musicVolume: 'vibee-music-volume',
  coverDuration: 'vibee-cover-duration',

  // ===============================
  // Effects
  // ===============================
  vignetteStrength: 'vibee-vignette-strength',
  colorCorrection: 'vibee-color-correction',

  // ===============================
  // Legacy Circle Avatar (deprecated, kept for compatibility)
  // ===============================
  circleSize: 'vibee-circle-size',
  circleBottom: 'vibee-circle-bottom-v2',
  circleLeft: 'vibee-circle-left-percent',
  faceOffsetX: 'vibee-face-offset-x',
  faceOffsetY: 'vibee-face-offset-y',
  faceScale: 'vibee-face-scale',
  isCircleAvatar: 'vibee-is-circle-avatar',
  avatarBorderRadius: 'vibee-avatar-border-radius',

  // ===============================
  // Avatar Mode Settings (CONSOLIDATED)
  // ===============================
  splitAvatarSettings: 'vibee-split-avatar-settings-v1',
  fullscreenAvatarSettings: 'vibee-fullscreen-avatar-settings-v1',

  // ===============================
  // Avatar Animation & Border
  // ===============================
  avatarAnimation: 'vibee-avatar-animation',
  avatarBorderEffect: 'vibee-avatar-border-effect',
  avatarBorderColor: 'vibee-avatar-border-color',
  avatarBorderColor2: 'vibee-avatar-border-color2',
  avatarBorderWidth: 'vibee-avatar-border-width',
  avatarBorderIntensity: 'vibee-avatar-border-intensity',

  // ===============================
  // Captions
  // ===============================
  captions: 'vibee-captions-v2',
  captionStyle: 'vibee-caption-style-v3',
  showCaptions: 'vibee-show-captions',

  // ===============================
  // Project & Tracks
  // ===============================
  project: 'vibee-project-v15',
  tracks: 'vibee-tracks-v17',
  assets: 'vibee-assets-v14',

  // ===============================
  // Templates
  // ===============================
  templates: 'vibee-templates',
  selectedTemplate: 'vibee-selected-template',
  templateSettings: 'vibee-template-settings-v1',

  // ===============================
  // User & Auth
  // ===============================
  user: 'vibee-user',
  language: 'vibee-lang',

  // ===============================
  // UI Settings
  // ===============================
  timelineZoom: 'vibee-timeline-zoom',
  snapSettings: 'vibee-snap-settings',
  playbackRate: 'vibee-playback-rate',

  // ===============================
  // AI Generation
  // ===============================
  voices: 'vibee-voices-v1',
  selectedVoice: 'vibee-selected-voice',
  generatedResults: 'vibee-generated-results-v1',
} as const;

// Type for storage key names
export type StorageKey = keyof typeof STORAGE_KEYS;

// Type for storage key values
export type StorageKeyValue = (typeof STORAGE_KEYS)[StorageKey];
