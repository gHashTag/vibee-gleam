/**
 * Shared Constants for VIBEE Remotion
 *
 * Single source of truth for colors, defaults, and animation values
 * used by both Remotion compositions and Player UI.
 */

// ============================================================
// Brand Colors
// ============================================================

export const VIBEE_AMBER = '#f59e0b';

export const COLORS = {
  amber: '#f59e0b',
  white: '#ffffff',
  black: '#000000',
  transparent: 'transparent',
} as const;

// ============================================================
// Avatar Defaults
// ============================================================

export const AVATAR_DEFAULTS = {
  circleSizePercent: 25.2,
  circleBottomPercent: 15,
  circleLeftPx: 40,
} as const;

// ============================================================
// Effect Defaults
// ============================================================

export const EFFECT_DEFAULTS = {
  coverDuration: 0.5,
  vignetteStrength: 0.7,
  colorCorrection: 1.2,
  musicVolume: 0.3,
} as const;

// ============================================================
// Animation
// ============================================================

export const SMOOTH_EASING = [0.4, 0, 0.2, 1] as const;

// Bezier easing for Remotion
export const SMOOTH_BEZIER = {
  x1: 0.4,
  y1: 0,
  x2: 0.2,
  y2: 1,
} as const;
