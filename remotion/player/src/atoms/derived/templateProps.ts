// ===============================
// Derived: templateProps
// Composite atom combining all template properties
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import { backgroundVideosAtom } from './backgroundVideos';
import { CAPTION_DEFAULTS } from '@/constants/captions';
import type { CaptionItem, CaptionStyle, LipSyncMainProps } from '@/store/types';

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
  'vibee-circle-bottom',
  15
);

export const circleLeftPxAtom = atomWithStorage(
  'vibee-circle-left',
  40
);

// Face centering (from /analyze-face endpoint)
export const faceOffsetXAtom = atom<number | undefined>(undefined);
export const faceOffsetYAtom = atom<number | undefined>(undefined);
export const faceScaleAtom = atom<number | undefined>(undefined);

// ===============================
// Captions Atoms
// ===============================

// Captions data (not persisted - loaded fresh from server)
export const captionsAtom = atom<CaptionItem[]>([]);

// Caption style (persisted)
export const captionStyleAtom = atomWithStorage<CaptionStyle>(
  'vibee-caption-style',
  {
    fontSize: CAPTION_DEFAULTS.fontSize,
    textColor: CAPTION_DEFAULTS.textColor,
    highlightColor: CAPTION_DEFAULTS.highlightColor,
    backgroundColor: CAPTION_DEFAULTS.backgroundColor,
    bottomPercent: CAPTION_DEFAULTS.bottomPercent,
    maxWidthPercent: CAPTION_DEFAULTS.maxWidthPercent,
    fontWeight: CAPTION_DEFAULTS.fontWeight,
    showShadow: CAPTION_DEFAULTS.showShadow,
  }
);

export const showCaptionsAtom = atomWithStorage('vibee-show-captions', true);

// ===============================
// Composite templateProps Atom
// ===============================

/**
 * templatePropsAtom - Combines all template props into LipSyncMainProps
 *
 * backgroundVideos is AUTO-DERIVED from video track!
 */
export const templatePropsAtom = atom((get): LipSyncMainProps => ({
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
  circleLeftPx: get(circleLeftPxAtom),
  captions: get(captionsAtom),
  captionStyle: get(captionStyleAtom),
  showCaptions: get(showCaptionsAtom),
  faceOffsetX: get(faceOffsetXAtom),
  faceOffsetY: get(faceOffsetYAtom),
  faceScale: get(faceScaleAtom),
}));

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
  circleLeftPx: circleLeftPxAtom,
  captions: captionsAtom,
  captionStyle: captionStyleAtom,
  showCaptions: showCaptionsAtom,
  faceOffsetX: faceOffsetXAtom,
  faceOffsetY: faceOffsetYAtom,
  faceScale: faceScaleAtom,
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
