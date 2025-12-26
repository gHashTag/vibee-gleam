// ===============================
// Derived: templateProps
// Composite atom combining all template properties
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import { backgroundVideosAtom } from './backgroundVideos';
import { CAPTION_DEFAULTS } from '@/constants/captions';
import type { CaptionItem, CaptionStyle, LipSyncMainProps } from '@/store/types';
import type { AvatarAnimation, AvatarBorderEffect } from '@/shared/types';

// ===============================
// Primitive Template Props Atoms
// ===============================

export const lipSyncVideoAtom = atomWithStorage(
  'vibee-lipsync-video',
  '/lipsync/lipsync.mp4'
);

export const coverImageAtom = atomWithStorage(
  'vibee-cover-image',
  '/covers/cover.jpeg'
);

export const backgroundMusicAtom = atomWithStorage(
  'vibee-background-music',
  '/audio/music/phonk_01.mp3'
);

export const musicVolumeAtom = atomWithStorage(
  'vibee-music-volume',
  0.06
);

export const coverDurationAtom = atomWithStorage(
  'vibee-cover-duration',
  0.5
);

export const vignetteStrengthAtom = atomWithStorage(
  'vibee-vignette-strength',
  0.7
);

export const colorCorrectionAtom = atomWithStorage(
  'vibee-color-correction',
  1.2
);

export const circleSizePercentAtom = atomWithStorage(
  'vibee-circle-size',
  25.2
);

export const circleBottomPercentAtom = atomWithStorage(
  'vibee-circle-bottom-v2',  // New key to reset old values
  0  // Center by default
);

export const circleLeftPercentAtom = atomWithStorage(
  'vibee-circle-left-percent',
  0  // Center by default (0 = centered)
);

// Face centering (from /analyze-face endpoint or manual adjustment)
export const faceOffsetXAtom = atomWithStorage('vibee-face-offset-x', 0);
export const faceOffsetYAtom = atomWithStorage('vibee-face-offset-y', 0);
export const faceScaleAtom = atomWithStorage('vibee-face-scale', 1.0);

// Circle avatar mode (legacy - kept for compatibility)
export const isCircleAvatarAtom = atomWithStorage('vibee-is-circle-avatar', false);
export const avatarBorderRadiusAtom = atomWithStorage('vibee-avatar-border-radius', 50); // 0-50%

// ===============================
// Split/Fullscreen Mode Settings
// ===============================

// Tab selection in UI
export const avatarSettingsTabAtom = atom<'split' | 'fullscreen'>('fullscreen');

// Split mode settings (when video background is shown)
// NOTE: -v3 keys to reset old localStorage values (formula changed from size*4 to size)
export const splitCircleSizeAtom = atomWithStorage('vibee-split-circle-size-v3', 100);
export const splitPositionXAtom = atomWithStorage('vibee-split-position-x-v2', 0);
export const splitPositionYAtom = atomWithStorage('vibee-split-position-y-v2', 0);
export const splitFaceScaleAtom = atomWithStorage('vibee-split-face-scale-v2', 1.0);
export const splitIsCircleAtom = atomWithStorage('vibee-split-is-circle-v2', false);
export const splitBorderRadiusAtom = atomWithStorage('vibee-split-border-radius-v2', 50);

// Fullscreen mode settings (avatar fills screen)
// NOTE: -v3 keys to reset old localStorage values (formula changed from size*4 to size)
export const fullscreenCircleSizeAtom = atomWithStorage('vibee-fullscreen-circle-size-v3', 100);
export const fullscreenPositionXAtom = atomWithStorage('vibee-fullscreen-position-x-v2', 0);
export const fullscreenPositionYAtom = atomWithStorage('vibee-fullscreen-position-y-v2', 0);
export const fullscreenFaceScaleAtom = atomWithStorage('vibee-fullscreen-face-scale-v2', 1.0);
export const fullscreenIsCircleAtom = atomWithStorage('vibee-fullscreen-is-circle-v2', false);
export const fullscreenBorderRadiusAtom = atomWithStorage('vibee-fullscreen-border-radius-v2', 50);

// ===============================
// Animation Settings
// ===============================

export const avatarAnimationAtom = atomWithStorage<AvatarAnimation>(
  'vibee-avatar-animation',
  'pop' // Default: spring pop-in effect
);

// ===============================
// Avatar Border Effect Settings
// ===============================

export const avatarBorderEffectAtom = atomWithStorage<AvatarBorderEffect>(
  'vibee-avatar-border-effect',
  'none' // Default: no border effect
);

export const avatarBorderColorAtom = atomWithStorage(
  'vibee-avatar-border-color',
  '#FFD700' // Gold by default
);

export const avatarBorderColor2Atom = atomWithStorage(
  'vibee-avatar-border-color2',
  '#FF6B6B' // Coral for gradient
);

export const avatarBorderWidthAtom = atomWithStorage(
  'vibee-avatar-border-width',
  4 // 4px default
);

export const avatarBorderIntensityAtom = atomWithStorage(
  'vibee-avatar-border-intensity',
  1.0 // 1x default
);

// ===============================
// Captions Atoms
// ===============================

// Captions data - PERSISTED to survive page refresh
export const captionsAtom = atomWithStorage<CaptionItem[]>('vibee-captions-v2', []);

// Caption style (persisted)
// NOTE: Changed key to -v2 to reset old localStorage with fontId -> fontFamily
export const captionStyleAtom = atomWithStorage<CaptionStyle>(
  'vibee-caption-style-v3', // v3 to reset with animation default
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

export const showCaptionsAtom = atomWithStorage('vibee-show-captions', true);

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

type TemplatePropKey = keyof Omit<LipSyncMainProps, 'backgroundVideos'>;

const propAtomMap: Record<string, any> = {
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
};

export const updateTemplatePropAtom = atom(
  null,
  (get, set, { key, value }: { key: TemplatePropKey; value: any }) => {
    const propAtom = propAtomMap[key];
    if (propAtom) {
      set(propAtom, value);
    } else {
      console.warn(`[updateTemplateProp] Unknown key: ${key}`);
    }
  }
);
