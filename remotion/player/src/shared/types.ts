/**
 * Shared Types for VIBEE Remotion
 *
 * Single source of truth for types used by both
 * Remotion compositions and Player UI.
 */

import type { Caption } from '@remotion/captions';

// ============================================================
// Animation Types
// ============================================================

export type AvatarAnimation = 'fade' | 'scale' | 'pop' | 'slide' | 'bounce' | 'none';
export type CaptionAnimation = 'pop' | 'fade' | 'slide' | 'bounce' | 'scaleRotate';

// ============================================================
// Avatar Border Effect Types
// ============================================================

export type AvatarBorderEffect =
  | 'none'
  | 'solid'
  | 'neon'
  | 'rainbow'
  | 'glass'
  | 'gradient'
  | 'pulse'
  // New effects
  | 'glow'
  | 'double'
  | 'neonPulse'
  | 'fire'
  | 'ocean'
  | 'sunset'
  | 'electric'
  | 'holographic';

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
  animation?: CaptionAnimation;
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

  // Avatar circle position (legacy)
  circleSizePercent?: number;
  circleBottomPercent?: number;
  circleLeftPercent?: number;

  // Face centering
  faceOffsetX?: number;
  faceOffsetY?: number;
  faceScale?: number;

  // Circle avatar (legacy)
  isCircleAvatar?: boolean;
  avatarBorderRadius?: number;

  // Split mode settings
  splitCircleSize?: number;
  splitPositionX?: number;
  splitPositionY?: number;
  splitFaceScale?: number;
  splitIsCircle?: boolean;
  splitBorderRadius?: number;

  // Fullscreen mode settings
  fullscreenCircleSize?: number;
  fullscreenPositionX?: number;
  fullscreenPositionY?: number;
  fullscreenFaceScale?: number;
  fullscreenIsCircle?: boolean;
  fullscreenBorderRadius?: number;

  // Avatar animation
  avatarAnimation?: AvatarAnimation;

  // Avatar border effect
  avatarBorderEffect?: AvatarBorderEffect;
  avatarBorderColor?: string;
  avatarBorderColor2?: string;
  avatarBorderWidth?: number;
  avatarBorderIntensity?: number;

  // Captions
  showCaptions?: boolean;
  captions?: Caption[];
  captionStyle?: CaptionStyle;
}

// Re-export Caption type for convenience
export type { Caption };
