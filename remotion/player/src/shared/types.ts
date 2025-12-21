/**
 * Shared Types for VIBEE Remotion
 *
 * Single source of truth for types used by both
 * Remotion compositions and Player UI.
 */

import type { Caption } from '@remotion/captions';

// ============================================================
// Caption Types
// ============================================================

export interface CaptionStyle {
  fontSize?: number;
  textColor?: string;
  highlightColor?: string;
  backgroundColor?: string;
  bottomPercent?: number;
  maxWidthPercent?: number;
  fontId?: string;
  fontFamily?: string;
  fontWeight?: number;
  showShadow?: boolean;
}

// ============================================================
// LipSync Composition Types
// ============================================================

export interface LipSyncMainProps {
  // Media
  lipSyncVideo: string;
  coverImage?: string;
  backgroundMusic?: string;
  musicVolume?: number;
  backgroundVideos?: string[];

  // Effects
  coverDuration?: number;
  vignetteStrength?: number;
  colorCorrection?: number;

  // Avatar circle position
  circleSizePercent?: number;
  circleBottomPercent?: number;
  circleLeftPx?: number;

  // Captions
  showCaptions?: boolean;
  captions?: Caption[];
  captionStyle?: CaptionStyle;
}

// Re-export Caption type for convenience
export type { Caption };
