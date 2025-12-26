// ===============================
// Derived: templateProps
// Composite atom combining all template properties
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import { backgroundVideosAtom } from './backgroundVideos';
import { CAPTION_DEFAULTS } from '@/constants/captions';
import { STORAGE_KEYS } from '../storageKeys';
import type { CaptionItem, CaptionStyle, LipSyncMainProps } from '@/store/types';
import type { AvatarAnimation, AvatarBorderEffect } from '@/shared/types';

// ===============================
// Primitive Template Props Atoms
// ===============================

export const lipSyncVideoAtom = atomWithStorage(
  STORAGE_KEYS.lipSyncVideo,
  '/lipsync/lipsync.mp4'
);

export const coverImageAtom = atomWithStorage(
  STORAGE_KEYS.coverImage,
  '/covers/cover.jpeg'
);

export const backgroundMusicAtom = atomWithStorage(
  STORAGE_KEYS.backgroundMusic,
  '/audio/music/phonk_01.mp3'
);

export const musicVolumeAtom = atomWithStorage(
  STORAGE_KEYS.musicVolume,
  0.06
);

export const coverDurationAtom = atomWithStorage(
  STORAGE_KEYS.coverDuration,
  0.5
);

export const vignetteStrengthAtom = atomWithStorage(
  STORAGE_KEYS.vignetteStrength,
  0.7
);

export const colorCorrectionAtom = atomWithStorage(
  STORAGE_KEYS.colorCorrection,
  1.2
);

export const circleSizePercentAtom = atomWithStorage(
  STORAGE_KEYS.circleSize,
  25.2
);

export const circleBottomPercentAtom = atomWithStorage(
  STORAGE_KEYS.circleBottom,
  0  // Center by default
);

export const circleLeftPercentAtom = atomWithStorage(
  STORAGE_KEYS.circleLeft,
  0  // Center by default (0 = centered)
);

// Face centering (from /analyze-face endpoint or manual adjustment)
export const faceOffsetXAtom = atomWithStorage(STORAGE_KEYS.faceOffsetX, 0);
export const faceOffsetYAtom = atomWithStorage(STORAGE_KEYS.faceOffsetY, 0);
export const faceScaleAtom = atomWithStorage(STORAGE_KEYS.faceScale, 1.0);

// Circle avatar mode (legacy - kept for compatibility)
export const isCircleAvatarAtom = atomWithStorage(STORAGE_KEYS.isCircleAvatar, false);
export const avatarBorderRadiusAtom = atomWithStorage(STORAGE_KEYS.avatarBorderRadius, 50); // 0-50%

// ===============================
// Split/Fullscreen Mode Settings
// ===============================

/**
 * AvatarModeSettings - Consolidated avatar settings for a mode
 * Reduces 12 atoms to 2 objects
 */
export interface AvatarModeSettings {
  circleSize: number;     // 10-100%
  positionX: number;      // -50 to 50%
  positionY: number;      // -50 to 50%
  faceScale: number;      // 0.5-2x
  isCircle: boolean;      // Circle mask toggle
  borderRadius: number;   // 0-100% (when isCircle=true)
}

// Default settings for both modes
const DEFAULT_AVATAR_SETTINGS: AvatarModeSettings = {
  circleSize: 100,
  positionX: 0,
  positionY: 0,
  faceScale: 1.0,
  isCircle: false,
  borderRadius: 50,
};

// Tab selection in UI
export const avatarSettingsTabAtom = atom<'split' | 'fullscreen'>('fullscreen');

// Consolidated avatar settings atoms
export const splitAvatarSettingsAtom = atomWithStorage<AvatarModeSettings>(
  STORAGE_KEYS.splitAvatarSettings,
  DEFAULT_AVATAR_SETTINGS
);

export const fullscreenAvatarSettingsAtom = atomWithStorage<AvatarModeSettings>(
  STORAGE_KEYS.fullscreenAvatarSettings,
  DEFAULT_AVATAR_SETTINGS
);

// ===============================
// Derived Selector Atoms (for UI compatibility)
// ===============================

// Helper to create getter/setter atoms for a specific property
function createAvatarPropAtom<K extends keyof AvatarModeSettings>(
  settingsAtom: typeof splitAvatarSettingsAtom,
  key: K
) {
  return atom(
    (get) => get(settingsAtom)[key],
    (get, set, value: AvatarModeSettings[K]) => {
      const current = get(settingsAtom);
      set(settingsAtom, { ...current, [key]: value });
    }
  );
}

// Split mode derived atoms
export const splitCircleSizeAtom = createAvatarPropAtom(splitAvatarSettingsAtom, 'circleSize');
export const splitPositionXAtom = createAvatarPropAtom(splitAvatarSettingsAtom, 'positionX');
export const splitPositionYAtom = createAvatarPropAtom(splitAvatarSettingsAtom, 'positionY');
export const splitFaceScaleAtom = createAvatarPropAtom(splitAvatarSettingsAtom, 'faceScale');
export const splitIsCircleAtom = createAvatarPropAtom(splitAvatarSettingsAtom, 'isCircle');
export const splitBorderRadiusAtom = createAvatarPropAtom(splitAvatarSettingsAtom, 'borderRadius');

// Fullscreen mode derived atoms
export const fullscreenCircleSizeAtom = createAvatarPropAtom(fullscreenAvatarSettingsAtom, 'circleSize');
export const fullscreenPositionXAtom = createAvatarPropAtom(fullscreenAvatarSettingsAtom, 'positionX');
export const fullscreenPositionYAtom = createAvatarPropAtom(fullscreenAvatarSettingsAtom, 'positionY');
export const fullscreenFaceScaleAtom = createAvatarPropAtom(fullscreenAvatarSettingsAtom, 'faceScale');
export const fullscreenIsCircleAtom = createAvatarPropAtom(fullscreenAvatarSettingsAtom, 'isCircle');
export const fullscreenBorderRadiusAtom = createAvatarPropAtom(fullscreenAvatarSettingsAtom, 'borderRadius');

// ===============================
// Animation Settings
// ===============================

export const avatarAnimationAtom = atomWithStorage<AvatarAnimation>(
  STORAGE_KEYS.avatarAnimation,
  'pop' // Default: spring pop-in effect
);

// ===============================
// Avatar Border Effect Settings
// ===============================

export const avatarBorderEffectAtom = atomWithStorage<AvatarBorderEffect>(
  STORAGE_KEYS.avatarBorderEffect,
  'none' // Default: no border effect
);

export const avatarBorderColorAtom = atomWithStorage(
  STORAGE_KEYS.avatarBorderColor,
  '#FFD700' // Gold by default
);

export const avatarBorderColor2Atom = atomWithStorage(
  STORAGE_KEYS.avatarBorderColor2,
  '#FF6B6B' // Coral for gradient
);

export const avatarBorderWidthAtom = atomWithStorage(
  STORAGE_KEYS.avatarBorderWidth,
  4 // 4px default
);

export const avatarBorderIntensityAtom = atomWithStorage(
  STORAGE_KEYS.avatarBorderIntensity,
  1.0 // 1x default
);

// ===============================
// Captions Atoms
// ===============================

// Captions data - PERSISTED to survive page refresh
export const captionsAtom = atomWithStorage<CaptionItem[]>(STORAGE_KEYS.captions, []);

// Caption style (persisted)
export const captionStyleAtom = atomWithStorage<CaptionStyle>(
  STORAGE_KEYS.captionStyle,
  {
    fontSize: CAPTION_DEFAULTS.fontSize,
    textColor: CAPTION_DEFAULTS.textColor,
    highlightColor: CAPTION_DEFAULTS.highlightColor,
    backgroundColor: CAPTION_DEFAULTS.backgroundColor,
    bottomPercent: CAPTION_DEFAULTS.bottomPercent,
    maxWidthPercent: CAPTION_DEFAULTS.maxWidthPercent,
    fontWeight: CAPTION_DEFAULTS.fontWeight,
    showShadow: CAPTION_DEFAULTS.showShadow,
    fontFamily: 'Montserrat',
    animation: 'pop', // Default caption animation
  }
);

export const showCaptionsAtom = atomWithStorage(STORAGE_KEYS.showCaptions, true);

// ===============================
// Force Refresh Atom
// ===============================

/**
 * forceRefreshAtom - Used to force re-render when atomWithStorage
 * doesn't trigger React subscriptions properly via editorStore.set()
 */
export const forceRefreshAtom = atom(0);

// ===============================
// Composite templateProps Atom
// ===============================

/**
 * templatePropsAtom - Combines all template props into LipSyncMainProps
 *
 * backgroundVideos is AUTO-DERIVED from video track!
 */
export const templatePropsAtom = atom((get): LipSyncMainProps => {
  // Subscribe to forceRefresh to ensure re-render when agent updates props
  get(forceRefreshAtom);

  return {
    lipSyncVideo: get(lipSyncVideoAtom),
    coverImage: get(coverImageAtom),
    backgroundMusic: get(backgroundMusicAtom),
    backgroundVideos: get(backgroundVideosAtom), // AUTO-DERIVED!
    musicVolume: get(musicVolumeAtom),
    coverDuration: get(coverDurationAtom),
    vignetteStrength: get(vignetteStrengthAtom),
    colorCorrection: get(colorCorrectionAtom),
    circleSizePercent: get(circleSizePercentAtom),
    circleBottomPercent: get(circleBottomPercentAtom),
    circleLeftPercent: get(circleLeftPercentAtom),
    captions: get(captionsAtom),
    captionStyle: get(captionStyleAtom),
    showCaptions: get(showCaptionsAtom),
    faceOffsetX: get(faceOffsetXAtom),
    faceOffsetY: get(faceOffsetYAtom),
    faceScale: get(faceScaleAtom),
    isCircleAvatar: get(isCircleAvatarAtom),
    avatarBorderRadius: get(avatarBorderRadiusAtom),
    // Split mode settings
    splitCircleSize: get(splitCircleSizeAtom),
    splitPositionX: get(splitPositionXAtom),
    splitPositionY: get(splitPositionYAtom),
    splitFaceScale: get(splitFaceScaleAtom),
    splitIsCircle: get(splitIsCircleAtom),
    splitBorderRadius: get(splitBorderRadiusAtom),
    // Fullscreen mode settings
    fullscreenCircleSize: get(fullscreenCircleSizeAtom),
    fullscreenPositionX: get(fullscreenPositionXAtom),
    fullscreenPositionY: get(fullscreenPositionYAtom),
    fullscreenFaceScale: get(fullscreenFaceScaleAtom),
    fullscreenIsCircle: get(fullscreenIsCircleAtom),
    fullscreenBorderRadius: get(fullscreenBorderRadiusAtom),
    // Avatar animation
    avatarAnimation: get(avatarAnimationAtom),
    // Avatar border effect
    avatarBorderEffect: get(avatarBorderEffectAtom),
    avatarBorderColor: get(avatarBorderColorAtom),
    avatarBorderColor2: get(avatarBorderColor2Atom),
    avatarBorderWidth: get(avatarBorderWidthAtom),
    avatarBorderIntensity: get(avatarBorderIntensityAtom),
  };
});

// ===============================
// Update Template Prop Action
// ===============================

// Type-safe prop atom map - ensures keys match LipSyncMainProps
const propAtomMap = {
  lipSyncVideo: lipSyncVideoAtom,
  coverImage: coverImageAtom,
  backgroundMusic: backgroundMusicAtom,
  musicVolume: musicVolumeAtom,
  coverDuration: coverDurationAtom,
  vignetteStrength: vignetteStrengthAtom,
  colorCorrection: colorCorrectionAtom,
  circleSizePercent: circleSizePercentAtom,
  circleBottomPercent: circleBottomPercentAtom,
  circleLeftPercent: circleLeftPercentAtom,
  captions: captionsAtom,
  captionStyle: captionStyleAtom,
  showCaptions: showCaptionsAtom,
  faceOffsetX: faceOffsetXAtom,
  faceOffsetY: faceOffsetYAtom,
  faceScale: faceScaleAtom,
  isCircleAvatar: isCircleAvatarAtom,
  avatarBorderRadius: avatarBorderRadiusAtom,
  // Split mode
  splitCircleSize: splitCircleSizeAtom,
  splitPositionX: splitPositionXAtom,
  splitPositionY: splitPositionYAtom,
  splitFaceScale: splitFaceScaleAtom,
  splitIsCircle: splitIsCircleAtom,
  splitBorderRadius: splitBorderRadiusAtom,
  // Fullscreen mode
  fullscreenCircleSize: fullscreenCircleSizeAtom,
  fullscreenPositionX: fullscreenPositionXAtom,
  fullscreenPositionY: fullscreenPositionYAtom,
  fullscreenFaceScale: fullscreenFaceScaleAtom,
  fullscreenIsCircle: fullscreenIsCircleAtom,
  fullscreenBorderRadius: fullscreenBorderRadiusAtom,
  // Animation
  avatarAnimation: avatarAnimationAtom,
  // Border effect
  avatarBorderEffect: avatarBorderEffectAtom,
  avatarBorderColor: avatarBorderColorAtom,
  avatarBorderColor2: avatarBorderColor2Atom,
  avatarBorderWidth: avatarBorderWidthAtom,
  avatarBorderIntensity: avatarBorderIntensityAtom,
} as const;

// Extract valid keys from the map (type-safe)
export type TemplatePropKey = keyof typeof propAtomMap;

export const updateTemplatePropAtom = atom(
  null,
  (
    _get,
    set,
    { key, value }: { key: TemplatePropKey; value: unknown }
  ) => {
    const propAtom = propAtomMap[key];
    if (propAtom) {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      set(propAtom as any, value);
    } else {
      console.warn(`[updateTemplateProp] Unknown key: ${String(key)}`);
    }
  }
);
