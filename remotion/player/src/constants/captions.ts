/**
 * Single source of truth for caption styles
 * All caption-related components should import from here
 */

export const CAPTION_DEFAULTS = {
  fontSize: 70,
  textColor: '#FFFF00', // Bright yellow TikTok-style
  highlightColor: '#f59e0b', // VIBEE amber
  backgroundColor: 'rgba(0, 0, 0, 0.6)',
  bottomPercent: 20,
  maxWidthPercent: 85,
  fontWeight: 900,
  fontFamily: '"Inter", "Arial", sans-serif',
  showShadow: true,
  maxWords: 1, // One word at a time
} as const;

export type CaptionDefaults = typeof CAPTION_DEFAULTS;
