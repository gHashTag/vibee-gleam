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
  Img,
  Audio,
  Sequence,
  useCurrentFrame,
  useVideoConfig,
} from 'remotion';
import { z } from 'zod';
import { Captions, type Caption } from '../components/Captions';
import { resolveMediaPath } from '../shared/mediaPath';

// ============================================================
// Schema
// ============================================================

export const SegmentSchema = z.object({
  type: z.enum(['split', 'fullscreen']),
  startFrame: z.number(),
  durationFrames: z.number(),
  bRollUrl: z.string().optional(),
  bRollType: z.enum(['video', 'image']).optional(),
  caption: z.string(),
});

export const CaptionStyleSchema = z.object({
  fontSize: z.number().optional(),
  textColor: z.string().optional(),
  highlightColor: z.string().optional(),
  backgroundColor: z.string().optional(),
  bottomPercent: z.number().optional(),
});

export const SplitTalkingHeadSchema = z.object({
  lipSyncVideo: z.string(),
  segments: z.array(SegmentSchema),
  captionColor: z.string().default('#FFFF00'),
  splitRatio: z.number().default(0.5),
  backgroundMusic: z.string().optional(),
  musicVolume: z.number().default(0.15),
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
});

export type SplitTalkingHeadProps = z.infer<typeof SplitTalkingHeadSchema>;
export type Segment = z.infer<typeof SegmentSchema>;

// ============================================================
// B-Roll Layer Component (with Sequence for proper headless rendering)
// ============================================================

interface BRollLayerProps {
  segment: Segment;
  height: number;
  splitRatio: number;
  segmentIndex: number;
}

const BRollLayer: React.FC<BRollLayerProps> = ({ segment, height, splitRatio, segmentIndex }) => {
  if (!segment.bRollUrl) return null;

  const topHeight = height * splitRatio;

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
          top: 0,
          left: 0,
          right: 0,
          height: topHeight,
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
// Main Component
// ============================================================

export const SplitTalkingHead: React.FC<SplitTalkingHeadProps> = ({
  lipSyncVideo,
  segments,
  captionColor = '#FFFF00',
  splitRatio = 0.5,
  backgroundMusic,
  musicVolume = 0.15,
  // 📝 TikTok-style Captions
  captions = [],
  showCaptions = true,
  captionStyle = {},
  // 👤 Face centering
  faceOffsetX = 0,
  faceOffsetY = 0,
  faceScale = 1,
}) => {
  const frame = useCurrentFrame();
  const { height } = useVideoConfig();

  // Find current segment
  const currentSegment = segments.find(
    (seg) => frame >= seg.startFrame && frame < seg.startFrame + seg.durationFrames
  );

  // Determine layout mode
  const isSplit = currentSegment?.type === 'split';

  // Calculate lipsync container position based on current mode
  const lipSyncTop = isSplit ? height * splitRatio : 0;
  const lipSyncHeight = isSplit ? height * (1 - splitRatio) : height;

  return (
    <AbsoluteFill style={{ backgroundColor: '#000' }}>

      {/* ========== B-ROLL LAYERS (all segments with Sequence timing) ========== */}
      {segments
        .filter((seg) => seg.type === 'split' && seg.bRollUrl)
        .map((segment, index) => (
          <BRollLayer
            key={`broll-${index}`}
            segment={segment}
            height={height}
            splitRatio={splitRatio}
            segmentIndex={index}
          />
        ))}

      {/* ========== LIPSYNC LAYER (ONE continuous video) ========== */}
      <div
        style={{
          position: 'absolute',
          left: 0,
          right: 0,
          top: lipSyncTop,
          height: lipSyncHeight,
          overflow: 'hidden',
          zIndex: 10,
        }}
      >
        <Video
          src={resolveMediaPath(lipSyncVideo)}
          volume={1}
          style={{
            width: `${100 * faceScale}%`,
            height: `${100 * faceScale}%`,
            objectFit: 'cover',
            transform: `translate(${faceOffsetX}%, ${faceOffsetY}%)`,
            transformOrigin: 'center center',
          }}
        />
      </div>

      {/* ========== 📝 reel_01.mp4 style CAPTIONS ========== */}
      {/* Dynamic position: at split border (splitRatio * 100%) in split mode, center (50%) in fullscreen */}
      {showCaptions && captions && captions.length > 0 && (
        <Captions
          captions={captions}
          fontSize={captionStyle?.fontSize ?? 56}
          textColor={captionStyle?.textColor ?? '#FFFF00'} // Yellow like reel_01.mp4
          topPercent={isSplit ? splitRatio * 100 : 75} // At border in split, BOTTOM in fullscreen
          maxWords={2}
        />
      )}

      {/* ========== BACKGROUND MUSIC ========== */}
      {backgroundMusic && (
        <Audio src={resolveMediaPath(backgroundMusic)} volume={musicVolume} />
      )}
    </AbsoluteFill>
  );
};

export default SplitTalkingHead;
