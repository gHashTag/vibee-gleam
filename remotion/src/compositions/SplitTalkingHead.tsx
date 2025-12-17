/**
 * Split Talking Head Composition
 *
 * Recreates the layout style from reel_01.mp4:
 * - Split horizontal (50/50): Top B-roll, Bottom talking head
 * - Yellow centered captions at the split point
 * - Alternating between split and fullscreen modes
 * - CTA with highlight box at the end
 *
 * IMPORTANT: LipSync video plays CONTINUOUSLY throughout the entire composition.
 * Segments only control the visual layout, not the video playback.
 */

import React from 'react';
import {
  AbsoluteFill,
  Sequence,
  Video,
  Img,
  Audio,
  useCurrentFrame,
  useVideoConfig,
  interpolate,
  spring,
  staticFile,
} from 'remotion';
import { z } from 'zod';

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

export const SplitTalkingHeadSchema = z.object({
  lipSyncVideo: z.string(),
  segments: z.array(SegmentSchema),
  captionColor: z.string().default('#FFFF00'),
  splitRatio: z.number().default(0.5),
  backgroundMusic: z.string().optional(),
  musicVolume: z.number().default(0.15),
  ctaText: z.string().optional(),
  ctaHighlight: z.string().optional(),
});

export type SplitTalkingHeadProps = z.infer<typeof SplitTalkingHeadSchema>;
export type Segment = z.infer<typeof SegmentSchema>;

// ============================================================
// Helper: Resolve media path
// ============================================================

function resolveMediaPath(path: string): string {
  if (!path) return '';
  if (path.startsWith('http://') || path.startsWith('https://')) {
    return path;
  }
  if (path.startsWith('/public/')) {
    return path;
  }
  return staticFile(path.startsWith('/') ? path : `/${path}`);
}

// ============================================================
// Yellow Caption Component
// ============================================================

interface YellowCaptionProps {
  text: string;
  color: string;
  position: 'center' | 'bottom';
  isHighlighted?: boolean;
  highlightWord?: string;
  animationFrame: number;
}

const YellowCaption: React.FC<YellowCaptionProps> = ({
  text,
  color,
  position,
  isHighlighted,
  highlightWord,
  animationFrame,
}) => {
  const { fps } = useVideoConfig();

  // Entrance animation based on segment-local frame
  const scale = spring({
    frame: animationFrame,
    fps,
    config: { damping: 12, stiffness: 200 },
  });

  const opacity = interpolate(animationFrame, [0, 5], [0, 1], {
    extrapolateRight: 'clamp',
  });

  const positionStyle: React.CSSProperties =
    position === 'center'
      ? { top: '50%', transform: `translateY(-50%) scale(${scale})` }
      : { bottom: 80, transform: `scale(${scale})` };

  // Render highlighted text if needed
  const renderText = () => {
    if (isHighlighted && highlightWord) {
      const parts = text.split(new RegExp(`(${highlightWord})`, 'gi'));
      return parts.map((part, i) =>
        part.toLowerCase() === highlightWord.toLowerCase() ? (
          <span
            key={i}
            style={{
              border: `3px solid ${color}`,
              borderRadius: 8,
              padding: '4px 16px',
              marginLeft: 8,
              marginRight: 8,
            }}
          >
            {part}
          </span>
        ) : (
          <span key={i}>{part}</span>
        )
      );
    }
    return text;
  };

  return (
    <div
      style={{
        position: 'absolute',
        left: 0,
        right: 0,
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        opacity,
        zIndex: 100,
        ...positionStyle,
      }}
    >
      <span
        style={{
          fontSize: 64,
          fontWeight: 800,
          color,
          textAlign: 'center',
          textShadow: '2px 2px 8px rgba(0,0,0,0.9), 0 0 20px rgba(0,0,0,0.5)',
          textTransform: 'uppercase',
          letterSpacing: 2,
        }}
      >
        {renderText()}
      </span>
    </div>
  );
};

// ============================================================
// B-Roll Layer Component
// ============================================================

interface BRollLayerProps {
  segment: Segment;
  height: number;
  splitRatio: number;
}

const BRollLayer: React.FC<BRollLayerProps> = ({ segment, height, splitRatio }) => {
  if (!segment.bRollUrl) return null;

  const topHeight = height * splitRatio;

  return (
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
          style={{
            width: '100%',
            height: '100%',
            objectFit: 'cover',
          }}
          muted
        />
      )}
    </div>
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
  ctaText,
  ctaHighlight,
}) => {
  const frame = useCurrentFrame();
  const { height, width, durationInFrames } = useVideoConfig();

  // Find current segment
  const currentSegment = segments.find(
    (seg) => frame >= seg.startFrame && frame < seg.startFrame + seg.durationFrames
  );

  // Determine layout mode
  const isSplit = currentSegment?.type === 'split';
  const isCtaSection = ctaText && frame > durationInFrames - 150;

  // Calculate lipsync container position based on current mode
  const lipSyncTop = isSplit ? height * splitRatio : 0;
  const lipSyncHeight = isSplit ? height * (1 - splitRatio) : height;

  // Frame within current segment (for caption animation)
  const segmentFrame = currentSegment ? frame - currentSegment.startFrame : 0;

  return (
    <AbsoluteFill style={{ backgroundColor: '#000' }}>

      {/* ========== B-ROLL LAYER (only when split mode) ========== */}
      {isSplit && currentSegment && (
        <BRollLayer
          segment={currentSegment}
          height={height}
          splitRatio={splitRatio}
        />
      )}

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
            width: '100%',
            height: '100%',
            objectFit: 'cover',
          }}
        />
      </div>

      {/* ========== CAPTION LAYER ========== */}
      {currentSegment && (
        <YellowCaption
          text={currentSegment.caption}
          color={captionColor}
          position={isSplit ? 'center' : 'bottom'}
          isHighlighted={isCtaSection}
          highlightWord={ctaHighlight}
          animationFrame={segmentFrame}
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
