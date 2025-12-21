/**
 * Single source of truth for composition configuration
 * Used by: renderApi.ts, Header.tsx
 */

export const COMPOSITIONS = {
  SPLIT_TALKING_HEAD: 'SplitTalkingHead',
} as const;

export type CompositionId = typeof COMPOSITIONS[keyof typeof COMPOSITIONS];

// Default composition for rendering
export const DEFAULT_COMPOSITION_ID: CompositionId = COMPOSITIONS.SPLIT_TALKING_HEAD;

// Composition metadata
export const COMPOSITION_CONFIG = {
  [COMPOSITIONS.SPLIT_TALKING_HEAD]: {
    id: COMPOSITIONS.SPLIT_TALKING_HEAD,
    name: 'Split Talking Head',
    description: 'TikTok-style split screen with B-roll and talking head',
    fps: 30,
    width: 1080,
    height: 1920,
    durationInFrames: 1020,
  },
} as const;
