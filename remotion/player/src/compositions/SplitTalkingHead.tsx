/**
 * Split Talking Head Composition
 *
 * Recreates the layout style from reel_01.mp4:
 * - Split horizontal (50/50): Top B-roll, Bottom talking head
 * - TikTok-style word-by-word captions with highlighting
 * - Alternating between split and fullscreen modes
 *
 * IMPORTANT: LipSync video plays CONTINUOUSLY throughout the entire composition.
 * Segments only control the visual layout, not the video playback.
 *
 * Captions: Uses @remotion/captions with word-by-word highlighting,
 * Montserrat Bold font, and smooth animations.
 */

import React, { useEffect } from 'react';
import {
  AbsoluteFill,
  Video,
  Img,
  Audio,
  Sequence,
  prefetch,
  useCurrentFrame,
  useVideoConfig,
  spring,
  interpolate,
} from 'remotion';
import { z } from 'zod';
import { Captions, type Caption } from './Captions';
import { resolveMediaPath } from '@/shared/mediaPath';
import { CAPTION_DEFAULTS } from '@/constants/captions';
import type { AvatarAnimation, AvatarBorderEffect } from '@/shared/types';

// ============================================================
// Video Layout Types
// ============================================================

export type VideoLayout =
  | 'top-half'          // B-roll top 50% (DEFAULT)
  | 'top-2-3'           // B-roll top 66%
  | 'top-3-4'           // B-roll top 75%
  | 'bottom-half'       // B-roll bottom 50%
  | 'bottom-2-3'        // B-roll bottom 66%
  | 'side-left'         // B-roll left 50%
  | 'side-right'        // B-roll right 50%
  | 'fullscreen'        // B-roll fullscreen only
  | 'pip-top-left'      // B-roll fullscreen, avatar top-left
  | 'pip-top-right'     // B-roll fullscreen, avatar top-right
  | 'pip-center-left'   // B-roll fullscreen, avatar center-left
  | 'pip-center-right'  // B-roll fullscreen, avatar center-right
  | 'pip-bottom-left'   // B-roll fullscreen, avatar bottom-left
  | 'pip-bottom-right'; // B-roll fullscreen, avatar bottom-right

// Layout dimensions calculator
interface LayoutDimensions {
  bRoll: { top: number; left: number; width: number; height: number };
  avatar: { top: number; left: number; width: number; height: number };
  isPip?: boolean;
  pipPosition?: 'top-left' | 'top-right' | 'center-left' | 'center-right' | 'bottom-left' | 'bottom-right';
  hideAvatar?: boolean;
  captionTopPercent: number;
}

const getLayoutDimensions = (layout: VideoLayout, width: number, height: number): LayoutDimensions => {
  switch (layout) {
    // Horizontal splits - B-roll on top
    case 'top-half':
      return {
        bRoll: { top: 0, left: 0, width, height: height * 0.5 },
        avatar: { top: height * 0.5, left: 0, width, height: height * 0.5 },
        captionTopPercent: 50,
      };
    case 'top-2-3':
      return {
        bRoll: { top: 0, left: 0, width, height: height * 0.66 },
        avatar: { top: height * 0.66, left: 0, width, height: height * 0.34 },
        captionTopPercent: 66,
      };
    case 'top-3-4':
      return {
        bRoll: { top: 0, left: 0, width, height: height * 0.75 },
        avatar: { top: height * 0.75, left: 0, width, height: height * 0.25 },
        captionTopPercent: 75,
      };
    // Horizontal splits - B-roll on bottom
    case 'bottom-half':
      return {
        bRoll: { top: height * 0.5, left: 0, width, height: height * 0.5 },
        avatar: { top: 0, left: 0, width, height: height * 0.5 },
        captionTopPercent: 50,
      };
    case 'bottom-2-3':
      return {
        bRoll: { top: height * 0.34, left: 0, width, height: height * 0.66 },
        avatar: { top: 0, left: 0, width, height: height * 0.34 },
        captionTopPercent: 34,
      };
    // Vertical splits
    case 'side-left':
      return {
        bRoll: { top: 0, left: 0, width: width * 0.5, height },
        avatar: { top: 0, left: width * 0.5, width: width * 0.5, height },
        captionTopPercent: 75,
      };
    case 'side-right':
      return {
        bRoll: { top: 0, left: width * 0.5, width: width * 0.5, height },
        avatar: { top: 0, left: 0, width: width * 0.5, height },
        captionTopPercent: 75,
      };
    // B-roll fullscreen only
    case 'fullscreen':
      return {
        bRoll: { top: 0, left: 0, width, height },
        avatar: { top: 0, left: 0, width: 0, height: 0 },
        hideAvatar: true,
        captionTopPercent: 75,
      };
    // PiP modes - avatar in corner
    case 'pip-top-left':
      return {
        bRoll: { top: 0, left: 0, width, height },
        avatar: { top: 0, left: 0, width, height },
        isPip: true,
        pipPosition: 'top-left',
        captionTopPercent: 55, // Captions higher up
      };
    case 'pip-top-right':
      return {
        bRoll: { top: 0, left: 0, width, height },
        avatar: { top: 0, left: 0, width, height },
        isPip: true,
        pipPosition: 'top-right',
        captionTopPercent: 55, // Captions higher up
      };
    case 'pip-center-left':
      return {
        bRoll: { top: 0, left: 0, width, height },
        avatar: { top: 0, left: 0, width, height },
        isPip: true,
        pipPosition: 'center-left',
        captionTopPercent: 70, // Higher than before to avoid center avatar
      };
    case 'pip-center-right':
      return {
        bRoll: { top: 0, left: 0, width, height },
        avatar: { top: 0, left: 0, width, height },
        isPip: true,
        pipPosition: 'center-right',
        captionTopPercent: 70, // Higher than before to avoid center avatar
      };
    case 'pip-bottom-left':
      return {
        bRoll: { top: 0, left: 0, width, height },
        avatar: { top: 0, left: 0, width, height },
        isPip: true,
        pipPosition: 'bottom-left',
        captionTopPercent: 50, // Captions centered
      };
    case 'pip-bottom-right':
      return {
        bRoll: { top: 0, left: 0, width, height },
        avatar: { top: 0, left: 0, width, height },
        isPip: true,
        pipPosition: 'bottom-right',
        captionTopPercent: 50, // Captions centered
      };
    default:
      return {
        bRoll: { top: 0, left: 0, width, height: height * 0.5 },
        avatar: { top: height * 0.5, left: 0, width, height: height * 0.5 },
        captionTopPercent: 50,
      };
  }
};

// ============================================================
// Schema
// ============================================================

const VideoLayoutSchema = z.enum([
  'top-half', 'top-2-3', 'top-3-4',
  'bottom-half', 'bottom-2-3',
  'side-left', 'side-right',
  'fullscreen',
  'pip-top-left', 'pip-top-right',
  'pip-center-left', 'pip-center-right',
  'pip-bottom-left', 'pip-bottom-right'
]);

export const SegmentSchema = z.object({
  type: z.enum(['split', 'fullscreen']),
  startFrame: z.number(),
  durationFrames: z.number(),
  bRollUrl: z.string().optional(),
  bRollType: z.enum(['video', 'image']).optional(),
  caption: z.string().optional(),
  layout: VideoLayoutSchema.optional(),
});

export const CaptionStyleSchema = z.object({
  fontSize: z.number().optional(),
  textColor: z.string().optional(),
  highlightColor: z.string().optional(),
  backgroundColor: z.string().optional(),
  bottomPercent: z.number().optional(),
  fontFamily: z.string().optional(),
  fontWeight: z.number().optional(),
  showShadow: z.boolean().optional(),
  animation: z.enum(['pop', 'fade', 'slide', 'bounce', 'scaleRotate']).optional(),
});

export const SplitTalkingHeadSchema = z.object({
  lipSyncVideo: z.string(),
  segments: z.array(SegmentSchema),
  captionColor: z.string().default('#FFFF00'),
  splitRatio: z.number().default(0.5),
  backgroundMusic: z.string().optional(),
  musicVolume: z.number().default(0.10),
  videoVolume: z.number().default(1),  // LipSync video volume (0-1)
  ctaText: z.string().optional(),
  ctaHighlight: z.string().optional(),
  // 📝 TikTok-style Captions
  captions: z.array(z.any()).default([]),
  showCaptions: z.boolean().default(true),
  captionStyle: CaptionStyleSchema.default({}),
  // 👤 Face centering (from /analyze-face endpoint)
  faceOffsetX: z.number().default(0),  // -50 to 50 (%)
  faceOffsetY: z.number().default(0),  // -50 to 50 (%)
  faceScale: z.number().default(1),    // 1.0 = no zoom
  // 🔵 Circle/Avatar styling (for rounded avatar display)
  circleSizePercent: z.number().default(25.2),  // Size as % of height
  circleBottomPercent: z.number().default(15),  // Bottom offset as % (Position Y)
  circleLeftPercent: z.number().default(0),     // Left offset as % (Position X, 0 = center)
  // 🔵 Circle mode (legacy - kept for compatibility)
  isCircleAvatar: z.boolean().default(false),
  avatarBorderRadius: z.number().default(50),   // 0-50% (50 = full circle)
  // 🎭 Split mode settings (when video background is shown)
  splitCircleSize: z.number().default(25),
  splitPositionX: z.number().default(0),
  splitPositionY: z.number().default(0),
  splitFaceScale: z.number().default(1),
  splitIsCircle: z.boolean().default(true),
  splitBorderRadius: z.number().default(50),
  // 🎭 Fullscreen mode settings (avatar fills screen)
  fullscreenCircleSize: z.number().default(50),
  fullscreenPositionX: z.number().default(0),
  fullscreenPositionY: z.number().default(0),
  fullscreenFaceScale: z.number().default(1),
  fullscreenIsCircle: z.boolean().default(false),
  fullscreenBorderRadius: z.number().default(50),
  // Visual effects
  vignetteStrength: z.number().default(0),
  colorCorrection: z.number().default(1),
  // Avatar settings mode (from UI Fullscreen/Split toggle)
  avatarSettingsTab: z.enum(['split', 'fullscreen']).default('split'),
  // Avatar animation (for PiP appearance)
  avatarAnimation: z.enum(['fade', 'scale', 'pop', 'slide', 'bounce', 'none']).default('pop'),
  // Avatar border effect
  avatarBorderEffect: z.enum([
    'none', 'solid', 'neon', 'rainbow', 'glass', 'gradient', 'pulse',
    'glow', 'double', 'neonPulse', 'fire', 'ocean', 'sunset', 'electric', 'holographic'
  ]).default('none'),
  avatarBorderColor: z.string().default('#FFD700'),
  avatarBorderColor2: z.string().default('#FF6B6B'),
  avatarBorderWidth: z.number().default(4),
  avatarBorderIntensity: z.number().default(1.0),
});

export type SplitTalkingHeadProps = z.infer<typeof SplitTalkingHeadSchema>;
export type Segment = z.infer<typeof SegmentSchema>;

// ============================================================
// B-Roll Layer Component (with Sequence for proper playback)
// ============================================================

interface BRollLayerProps {
  segment: Segment;
  width: number;
  height: number;
  segmentIndex: number;
}

const BRollLayer: React.FC<BRollLayerProps> = ({ segment, width, height, segmentIndex }) => {
  if (!segment.bRollUrl) return null;

  const layout = (segment.layout as VideoLayout) || 'top-half';
  const dims = getLayoutDimensions(layout, width, height);

  return (
    <Sequence
      from={segment.startFrame}
      durationInFrames={segment.durationFrames}
      premountFor={90}
      name={`B-Roll ${segmentIndex}`}
    >
      <div
        style={{
          position: 'absolute',
          top: dims.bRoll.top,
          left: dims.bRoll.left,
          width: dims.bRoll.width,
          height: dims.bRoll.height,
          overflow: 'hidden',
          zIndex: 5,
        }}
      >
        {segment.bRollType === 'image' ? (
          <Img
            src={resolveMediaPath(segment.bRollUrl)}
            style={{
              width: '100%',
              height: '100%',
              objectFit: 'cover',
            }}
          />
        ) : (
          <Video
            src={resolveMediaPath(segment.bRollUrl)}
            startFrom={0}
            pauseWhenBuffering
            style={{
              width: '100%',
              height: '100%',
              objectFit: 'cover',
            }}
            muted
            loop
          />
        )}
      </div>
    </Sequence>
  );
};

// ============================================================
// Avatar Animation Helper
// ============================================================

const getAvatarAnimationStyle = (
  frame: number,
  fps: number,
  animation: AvatarAnimation,
  segmentStartFrame: number,
  segmentEndFrame: number
): React.CSSProperties => {
  const framesSinceStart = Math.max(0, frame - segmentStartFrame);
  const framesUntilEnd = Math.max(0, segmentEndFrame - frame);
  const enterDuration = 8;  // frames for enter animation
  const exitDuration = 8;   // frames for exit animation

  // ===== EXIT ANIMATION (when segment is ending) =====
  if (framesUntilEnd < exitDuration && framesUntilEnd >= 0) {
    const exitProgress = (exitDuration - framesUntilEnd) / exitDuration; // 0 → 1

    switch (animation) {
      case 'fade':
        return { opacity: 1 - exitProgress };
      case 'scale':
        return {
          transform: `scale(${1 - exitProgress})`,
          opacity: 1 - exitProgress,
        };
      case 'pop':
      case 'bounce':
        // Reverse spring effect: scale down
        return {
          transform: `scale(${1 - exitProgress})`,
          opacity: 1 - exitProgress,
        };
      case 'slide':
        return {
          transform: `translateX(${exitProgress * 100}px)`,
          opacity: 1 - exitProgress,
        };
      case 'none':
      default:
        return {};
    }
  }

  // ===== ENTER ANIMATION (when segment starts) =====
  switch (animation) {
    case 'fade':
      return {
        opacity: interpolate(framesSinceStart, [0, enterDuration], [0, 1], { extrapolateRight: 'clamp' }),
      };
    case 'scale':
      return {
        transform: `scale(${interpolate(framesSinceStart, [0, enterDuration], [0, 1], { extrapolateRight: 'clamp' })})`,
        opacity: interpolate(framesSinceStart, [0, 4], [0, 1], { extrapolateRight: 'clamp' }),
      };
    case 'pop':
      const popScale = spring({
        frame: framesSinceStart,
        fps,
        config: { damping: 12, stiffness: 180, mass: 0.4 },
      });
      return {
        transform: `scale(${popScale})`,
        opacity: interpolate(framesSinceStart, [0, 2], [0, 1], { extrapolateRight: 'clamp' }),
      };
    case 'slide':
      const slideX = interpolate(framesSinceStart, [0, 10], [-100, 0], { extrapolateRight: 'clamp' });
      return {
        transform: `translateX(${slideX}px)`,
        opacity: interpolate(framesSinceStart, [0, 4], [0, 1], { extrapolateRight: 'clamp' }),
      };
    case 'bounce':
      const bounceScale = spring({
        frame: framesSinceStart,
        fps,
        config: { damping: 8, stiffness: 200, mass: 0.3 },
      });
      return {
        transform: `scale(${bounceScale})`,
        opacity: interpolate(framesSinceStart, [0, 2], [0, 1], { extrapolateRight: 'clamp' }),
      };
    case 'none':
    default:
      return {};
  }
};

// ============================================================
// Avatar Border Effect Helper
// ============================================================

const getAvatarBorderStyle = (
  effect: AvatarBorderEffect,
  color: string,
  color2: string,
  width: number,
  intensity: number
): React.CSSProperties => {
  switch (effect) {
    case 'solid':
      return {
        border: `${width}px solid ${color}`,
      };

    case 'neon':
      const glow = width * 3 * intensity;
      return {
        border: `${width}px solid ${color}`,
        boxShadow: `
          0 0 ${glow}px ${color},
          0 0 ${glow * 2}px ${color},
          0 0 ${glow * 3}px ${color},
          inset 0 0 ${glow}px ${color}40
        `,
      };

    case 'glass':
      return {
        border: `${width}px solid rgba(255, 255, 255, 0.3)`,
        backdropFilter: 'blur(10px)',
        boxShadow: `
          0 8px 32px rgba(0, 0, 0, 0.1),
          inset 0 0 0 1px rgba(255, 255, 255, 0.1)
        `,
      };

    case 'pulse':
      return {
        border: `${width}px solid ${color}`,
        boxShadow: `0 0 ${width * 2 * intensity}px ${color}`,
        '--pulse-color': color,
      } as React.CSSProperties;

    // NEW EFFECTS

    case 'glow':
      // Soft glow (simpler than neon)
      const softGlow = width * 2 * intensity;
      return {
        border: `${width}px solid ${color}80`,
        boxShadow: `
          0 0 ${softGlow}px ${color}60,
          0 0 ${softGlow * 2}px ${color}40
        `,
      };

    case 'double':
      // Double border
      return {
        border: `${width}px solid ${color}`,
        outline: `${width}px solid ${color2}`,
        outlineOffset: `${width}px`,
      };

    case 'neonPulse':
      // Neon + Pulse combined
      const neonPulseGlow = width * 3 * intensity;
      return {
        border: `${width}px solid ${color}`,
        boxShadow: `
          0 0 ${neonPulseGlow}px ${color},
          0 0 ${neonPulseGlow * 2}px ${color},
          0 0 ${neonPulseGlow * 3}px ${color}
        `,
        '--pulse-color': color,
      } as React.CSSProperties;

    case 'electric':
      // Electric/spark effect
      const electricGlow = width * 4 * intensity;
      return {
        border: `${width}px solid #00ffff`,
        boxShadow: `
          0 0 ${electricGlow}px #00ffff,
          0 0 ${electricGlow * 1.5}px #0080ff,
          0 0 ${electricGlow * 2}px #ffffff,
          inset 0 0 ${width * 2}px #00ffff40
        `,
      };

    case 'gradient':
    case 'rainbow':
    case 'fire':
    case 'ocean':
    case 'sunset':
    case 'holographic':
      // These effects need wrapper div - return empty here
      return {};

    case 'none':
    default:
      return {};
  }
};

// Check if effect needs wrapper (animated gradients)
const needsBorderWrapper = (effect: AvatarBorderEffect): boolean => {
  return ['gradient', 'rainbow', 'fire', 'ocean', 'sunset', 'holographic'].includes(effect);
};

// Get wrapper style for gradient/rainbow effects
const getBorderWrapperStyle = (
  effect: AvatarBorderEffect,
  color: string,
  color2: string,
  width: number,
  borderRadius: number
): React.CSSProperties => {
  const baseStyle = {
    padding: width,
    borderRadius: `${borderRadius}%`,
  };

  switch (effect) {
    case 'rainbow':
      return {
        ...baseStyle,
        background: `conic-gradient(from 0deg,
          #ff0000, #ff8000, #ffff00, #80ff00,
          #00ff00, #00ff80, #00ffff, #0080ff,
          #0000ff, #8000ff, #ff00ff, #ff0080, #ff0000)`,
      };

    case 'gradient':
      return {
        ...baseStyle,
        background: `linear-gradient(135deg, ${color}, ${color2})`,
      };

    case 'fire':
      return {
        ...baseStyle,
        background: `linear-gradient(45deg,
          #ff0000, #ff4500, #ff8c00, #ffd700,
          #ff8c00, #ff4500, #ff0000)`,
        backgroundSize: '400% 400%',
      };

    case 'ocean':
      return {
        ...baseStyle,
        background: `linear-gradient(45deg,
          #0077be, #00a8e8, #00d4ff, #40e0d0,
          #00d4ff, #00a8e8, #0077be)`,
        backgroundSize: '400% 400%',
      };

    case 'sunset':
      return {
        ...baseStyle,
        background: `linear-gradient(45deg,
          #ff6b6b, #ee5a24, #f79f1f, #ff6b6b,
          #c44569, #ff6b6b)`,
        backgroundSize: '400% 400%',
      };

    case 'holographic':
      return {
        ...baseStyle,
        background: `linear-gradient(135deg,
          #ff00ff, #00ffff, #ff00ff, #ffff00,
          #00ffff, #ff00ff)`,
        backgroundSize: '300% 300%',
      };

    default:
      return {};
  }
};

// Get CSS class for animated effects
const getBorderClassName = (effect: AvatarBorderEffect): string => {
  switch (effect) {
    case 'pulse':
    case 'neonPulse':
      return 'avatar-border-pulse';
    case 'rainbow':
    case 'holographic':
      return 'avatar-border-rainbow';
    case 'fire':
      return 'avatar-border-fire';
    case 'ocean':
      return 'avatar-border-ocean';
    case 'sunset':
      return 'avatar-border-sunset';
    case 'electric':
      return 'avatar-border-electric';
    default:
      return '';
  }
};

// ============================================================
// Main Component
// ============================================================

export const SplitTalkingHead: React.FC<SplitTalkingHeadProps> = ({
  lipSyncVideo,
  segments,
  captionColor = '#FFFF00',
  splitRatio = 0.5,
  backgroundMusic,
  musicVolume = 0.10,
  videoVolume = 1,
  // 📝 TikTok-style Captions
  captions = [],
  showCaptions = true,
  captionStyle = {},
  // 👤 Face centering (legacy)
  faceOffsetX = 0,
  faceOffsetY = 0,
  faceScale = 1,
  // 🔵 Circle/Avatar sizing & position (legacy)
  circleSizePercent = 25.2,
  circleBottomPercent = 15,  // Position Y
  circleLeftPercent = 0,     // Position X (0 = center)
  // 🔵 Circle mode (legacy)
  isCircleAvatar = false,
  avatarBorderRadius = 50,
  // 🎭 Split mode settings
  splitCircleSize = 25,
  splitPositionX = 0,
  splitPositionY = 0,
  splitFaceScale = 1,
  splitIsCircle = true,
  splitBorderRadius = 50,
  // 🎭 Fullscreen mode settings
  fullscreenCircleSize = 50,
  fullscreenPositionX = 0,
  fullscreenPositionY = 0,
  fullscreenFaceScale = 1,
  fullscreenIsCircle = false,
  fullscreenBorderRadius = 50,
  // 🎨 Visual effects
  vignetteStrength = 0,
  colorCorrection = 1,
  // 🎭 Avatar settings mode (from UI toggle)
  avatarSettingsTab = 'split',
  // 🎬 Avatar animation
  avatarAnimation = 'pop',
  // 🎨 Avatar border effect
  avatarBorderEffect = 'none',
  avatarBorderColor = '#FFD700',
  avatarBorderColor2 = '#FF6B6B',
  avatarBorderWidth = 4,
  avatarBorderIntensity = 1.0,
}) => {
  console.log('[SplitTalkingHead] backgroundMusic:', backgroundMusic, 'musicVolume:', musicVolume);
  const frame = useCurrentFrame();
  const { width, height, fps } = useVideoConfig();

  // 🎬 Prefetch all b-roll videos for smooth playback
  useEffect(() => {
    const videoUrls = segments
      .filter((seg) => seg.type === 'split' && seg.bRollUrl && seg.bRollType === 'video')
      .map((seg) => resolveMediaPath(seg.bRollUrl!));

    const prefetchers = videoUrls.map((url) => {
      try {
        return prefetch(url);
      } catch (e) {
        console.warn('Failed to prefetch:', url, e);
        return null;
      }
    });

    return () => {
      prefetchers.forEach((p) => p?.free());
    };
  }, [segments]);

  // Find current segment
  const currentSegment = segments.find(
    (seg) => frame >= seg.startFrame && frame < seg.startFrame + seg.durationFrames
  );

  // Determine layout mode and dimensions
  const isSplit = currentSegment?.type === 'split';
  const currentLayout = currentSegment?.layout as VideoLayout || 'top-half';
  // Always calculate layoutDims to support PiP layouts even without explicit split type
  const layoutDims = isSplit || currentLayout.startsWith('pip-')
    ? getLayoutDimensions(currentLayout, width, height)
    : null;

  // 🎭 Select settings based on UI mode toggle (NOT timeline segment)
  const useSplitSettings = avatarSettingsTab === 'split';
  const currentSize = useSplitSettings ? splitCircleSize : fullscreenCircleSize;
  const currentPosX = useSplitSettings ? splitPositionX : fullscreenPositionX;
  const currentPosY = useSplitSettings ? splitPositionY : fullscreenPositionY;
  const currentScale = useSplitSettings ? splitFaceScale : fullscreenFaceScale;
  const currentIsCircle = useSplitSettings ? splitIsCircle : fullscreenIsCircle;
  const currentRadius = useSplitSettings ? splitBorderRadius : fullscreenBorderRadius;

  // For PiP mode
  const isPipMode = layoutDims?.isPip;
  const hideAvatar = layoutDims?.hideAvatar;
  // PiP: Scale size (100% -> 25%, 50% -> 12.5%) with limits 8-30%
  const pipSize = Math.max(8, Math.min(splitCircleSize / 4, 30));

  return (
    <AbsoluteFill style={{ backgroundColor: '#000' }}>

      {/* ========== B-ROLL LAYERS (all segments with Sequence timing) ========== */}
      {segments
        .filter((seg) => seg.type === 'split' && seg.bRollUrl)
        .map((segment, index) => (
          <BRollLayer
            key={`broll-${index}`}
            segment={segment}
            width={width}
            height={height}
            segmentIndex={index}
          />
        ))}

      {/* ========== LIPSYNC LAYER (ONE continuous video) ========== */}
      {/* Avatar is hidden in fullscreen B-roll mode */}
      {!hideAvatar && (
        isPipMode ? (
          // PiP mode: small circle avatar in corner (based on pipPosition)
          // Animation wrapper for PiP appearance/disappearance
          (() => {
            const segmentStart = currentSegment?.startFrame ?? 0;
            const segmentEnd = segmentStart + (currentSegment?.durationFrames ?? 0);
            const animStyle = getAvatarAnimationStyle(
              frame,
              fps,
              avatarAnimation as AvatarAnimation,
              segmentStart,
              segmentEnd
            );
            // Merge animation transform with position transform (for center positions)
            const isCenter = layoutDims?.pipPosition === 'center-left' || layoutDims?.pipPosition === 'center-right';
            const positionTransform = isCenter ? 'translateY(-50%)' : '';
            const animTransform = animStyle.transform || '';
            const combinedTransform = [positionTransform, animTransform].filter(Boolean).join(' ');

            // Border effect styles
            const borderEffect = avatarBorderEffect as AvatarBorderEffect;
            const borderStyle = getAvatarBorderStyle(
              borderEffect,
              avatarBorderColor,
              avatarBorderColor2,
              avatarBorderWidth,
              avatarBorderIntensity
            );
            const borderClassName = getBorderClassName(borderEffect);
            const useWrapper = needsBorderWrapper(borderEffect);
            const wrapperStyle = useWrapper
              ? getBorderWrapperStyle(borderEffect, avatarBorderColor, avatarBorderColor2, avatarBorderWidth, splitBorderRadius)
              : {};

            // Base positioning styles
            const basePositionStyle: React.CSSProperties = {
              position: 'absolute',
              ...(layoutDims?.pipPosition === 'top-left' && { top: 140, left: 40 }),
              ...(layoutDims?.pipPosition === 'top-right' && { top: 140, right: 40 }),
              ...(layoutDims?.pipPosition === 'center-left' && { top: '50%', left: 40 }),
              ...(layoutDims?.pipPosition === 'center-right' && { top: '50%', right: 40 }),
              ...(layoutDims?.pipPosition === 'bottom-left' && { bottom: 200, left: 40 }),
              ...(layoutDims?.pipPosition === 'bottom-right' && { bottom: 200, right: 40 }),
              zIndex: 15,
              ...(combinedTransform && { transform: combinedTransform }),
              opacity: animStyle.opacity ?? 1,
            };

            // Video element
            const videoElement = (
              <Video
                src={resolveMediaPath(lipSyncVideo)}
                volume={videoVolume}
                style={{
                  width: '100%',
                  height: '100%',
                  objectFit: 'cover',
                  transform: `scale(${currentScale}) translate(${faceOffsetX}%, ${faceOffsetY}%)`,
                  transformOrigin: 'center center',
                }}
              />
            );

            // Render with or without wrapper based on effect type
            if (useWrapper) {
              // Gradient/Rainbow: needs outer wrapper for background gradient
              return (
                <div
                  style={{
                    ...basePositionStyle,
                    ...wrapperStyle,
                    width: height * (pipSize / 100) + avatarBorderWidth * 2,
                    height: height * (pipSize / 100) + avatarBorderWidth * 2,
                  }}
                  className={borderClassName}
                >
                  <div
                    style={{
                      width: '100%',
                      height: '100%',
                      borderRadius: `${splitBorderRadius}%`,
                      overflow: 'hidden',
                    }}
                  >
                    {videoElement}
                  </div>
                </div>
              );
            }

            // Standard effects: apply styles directly to container
            return (
              <div
                style={{
                  ...basePositionStyle,
                  width: height * (pipSize / 100),
                  height: height * (pipSize / 100),
                  borderRadius: `${splitBorderRadius}%`,
                  overflow: 'hidden',
                  boxShadow: borderEffect === 'none' ? '0 4px 20px rgba(0, 0, 0, 0.4)' : undefined,
                  ...borderStyle,
                }}
                className={borderClassName}
              >
                {videoElement}
              </div>
            );
          })()
        ) : (
          // Normal mode: avatar positioned according to layout
          <div
            style={{
              position: 'absolute',
              left: layoutDims?.avatar.left ?? 0,
              top: layoutDims?.avatar.top ?? (isSplit ? height * 0.5 : 0),
              width: layoutDims?.avatar.width ?? width,
              height: layoutDims?.avatar.height ?? (isSplit ? height * 0.5 : height),
              overflow: 'hidden',
              zIndex: 10,
              display: 'flex',
              justifyContent: 'center',
              alignItems: 'center',
            }}
          >
            {/* Inner container: Size controls width/height, Position X/Y control offset */}
            <div
              style={{
                width: currentIsCircle ? `${currentSize}%` : '100%',
                height: currentIsCircle ? undefined : '100%',
                aspectRatio: currentIsCircle ? '1 / 1' : undefined,
                overflow: 'hidden',
                position: 'relative',
                transform: currentIsCircle ? `translate(${currentPosX}%, ${currentPosY}%)` : undefined,
                borderRadius: currentIsCircle ? `${currentRadius}%` : 0,
              }}
            >
              <Video
                src={resolveMediaPath(lipSyncVideo)}
                volume={videoVolume}
                style={{
                  width: '100%',
                  height: '100%',
                  objectFit: 'cover',
                  transform: `scale(${currentScale}) translate(${faceOffsetX}%, ${faceOffsetY}%)`,
                  transformOrigin: 'center center',
                }}
              />
            </div>
          </div>
        )
      )}

      {/* ========== 📝 reel_01.mp4 style CAPTIONS ========== */}
      {/* Dynamic position: based on layout (at B-roll border in split, 75% in fullscreen) */}
      {showCaptions && captions && captions.length > 0 && (
        <Captions
          captions={captions}
          fontSize={captionStyle?.fontSize ?? CAPTION_DEFAULTS.fontSize}
          textColor={captionStyle?.textColor ?? CAPTION_DEFAULTS.textColor}
          topPercent={layoutDims?.captionTopPercent ?? 75}
          maxWords={CAPTION_DEFAULTS.maxWords}
          fontFamily={captionStyle?.fontFamily}
          fontWeight={captionStyle?.fontWeight}
          showShadow={captionStyle?.showShadow}
          animation={captionStyle?.animation ?? 'pop'}
        />
      )}

      {/* ========== BACKGROUND MUSIC ========== */}
      {backgroundMusic && (
        <Audio
          key={backgroundMusic}
          src={resolveMediaPath(backgroundMusic)}
          volume={musicVolume}
          pauseWhenBuffering
        />
      )}

      {/* ========== COLOR CORRECTION OVERLAY ========== */}
      {colorCorrection !== 1 && (
        <div
          style={{
            position: 'absolute',
            inset: 0,
            backgroundColor: colorCorrection > 1
              ? `rgba(255, 200, 150, ${(colorCorrection - 1) * 0.3})` // Warm
              : `rgba(150, 200, 255, ${(1 - colorCorrection) * 0.3})`, // Cool
            mixBlendMode: 'overlay',
            pointerEvents: 'none',
            zIndex: 50,
          }}
        />
      )}

      {/* ========== VIGNETTE OVERLAY ========== */}
      {vignetteStrength > 0 && (
        <div
          style={{
            position: 'absolute',
            inset: 0,
            background: `radial-gradient(ellipse at center, transparent 30%, rgba(0,0,0,${vignetteStrength}) 100%)`,
            pointerEvents: 'none',
            zIndex: 51,
          }}
        />
      )}
    </AbsoluteFill>
  );
};

export default SplitTalkingHead;
