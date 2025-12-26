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

import React from 'react';
import {
  AbsoluteFill,
  Video,
  OffthreadVideo,
  Img,
  Audio,
  Sequence,
  useCurrentFrame,
  useVideoConfig,
} from 'remotion';
import { z } from 'zod';
import { Captions, type Caption } from '../components/Captions';
import { resolveMediaPath } from '../shared/mediaPath';
import { CAPTION_DEFAULTS } from '../constants/captions';

// ============================================================
// Video Layout Types
// ============================================================

export type VideoLayout =
  | 'top-half'      // B-roll top 50% (DEFAULT)
  | 'top-2-3'       // B-roll top 66%
  | 'top-3-4'       // B-roll top 75%
  | 'bottom-half'   // B-roll bottom 50%
  | 'bottom-2-3'    // B-roll bottom 66%
  | 'side-left'     // B-roll left 50%
  | 'side-right'    // B-roll right 50%
  | 'pip'           // B-roll fullscreen, avatar bottom-left
  | 'fullscreen';   // B-roll fullscreen only

// Layout dimensions calculator
interface LayoutDimensions {
  bRoll: { top: number; left: number; width: number; height: number };
  avatar: { top: number; left: number; width: number; height: number };
  isPip?: boolean;
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
    // Special modes
    case 'pip':
      return {
        bRoll: { top: 0, left: 0, width, height },
        avatar: { top: 0, left: 0, width, height },
        isPip: true,
        captionTopPercent: 75,
      };
    case 'fullscreen':
      return {
        bRoll: { top: 0, left: 0, width, height },
        avatar: { top: 0, left: 0, width: 0, height: 0 },
        hideAvatar: true,
        captionTopPercent: 75,
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
  'pip', 'fullscreen'
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
  circleBottomPercent: z.number().default(15),  // Bottom offset as %
  circleLeftPercent: z.number().default(0),     // Left offset as % (0 = center)
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
});

export type SplitTalkingHeadProps = z.infer<typeof SplitTalkingHeadSchema>;
export type Segment = z.infer<typeof SegmentSchema>;

// ============================================================
// B-Roll Layer Component (with Sequence for proper headless rendering)
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
          <OffthreadVideo
            src={resolveMediaPath(segment.bRollUrl)}
            startFrom={0}
            style={{
              width: '100%',
              height: '100%',
              objectFit: 'cover',
            }}
            muted
          />
        )}
      </div>
    </Sequence>
  );
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
  // 🔵 Circle/Avatar styling (legacy)
  circleSizePercent = 25.2,
  circleBottomPercent = 15,
  circleLeftPercent = 0,
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
  // Visual effects
  vignetteStrength = 0,
  colorCorrection = 1,
}) => {
  const frame = useCurrentFrame();
  const { width, height } = useVideoConfig();

  // Find current segment
  const currentSegment = segments.find(
    (seg) => frame >= seg.startFrame && frame < seg.startFrame + seg.durationFrames
  );

  // Determine layout mode and dimensions
  const isSplit = currentSegment?.type === 'split';
  const currentLayout = (isSplit && currentSegment?.layout) ? currentSegment.layout as VideoLayout : 'top-half';
  const layoutDims = isSplit ? getLayoutDimensions(currentLayout, width, height) : null;

  // 🎭 Select settings based on current mode (split vs fullscreen)
  const currentSize = isSplit ? splitCircleSize : fullscreenCircleSize;
  const currentPosX = isSplit ? splitPositionX : fullscreenPositionX;
  const currentPosY = isSplit ? splitPositionY : fullscreenPositionY;
  const currentScale = isSplit ? splitFaceScale : fullscreenFaceScale;
  const currentIsCircle = isSplit ? splitIsCircle : fullscreenIsCircle;
  const currentRadius = isSplit ? splitBorderRadius : fullscreenBorderRadius;

  // For PiP mode, override with fixed small circle bottom-left
  const isPipMode = layoutDims?.isPip;
  const hideAvatar = layoutDims?.hideAvatar;

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
          // PiP mode: small circle avatar in bottom-left corner
          <div
            style={{
              position: 'absolute',
              bottom: 40,
              left: 40,
              width: height * 0.2, // 20% of height
              height: height * 0.2,
              borderRadius: '50%',
              overflow: 'hidden',
              zIndex: 15,
              boxShadow: '0 4px 20px rgba(0, 0, 0, 0.4)',
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
    </AbsoluteFill>
  );
};

export default SplitTalkingHead;
