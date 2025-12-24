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

import React, { useEffect, useRef } from 'react';
import {
  AbsoluteFill,
  Video,
  Img,
  Audio,
  Sequence,
  prefetch,
  useCurrentFrame,
  useVideoConfig,
} from 'remotion';
import { z } from 'zod';
import { Captions, type Caption } from './Captions';
import { resolveMediaPath } from '@/shared/mediaPath';
import { CAPTION_DEFAULTS } from '@/constants/captions';

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
});

export type SplitTalkingHeadProps = z.infer<typeof SplitTalkingHeadSchema>;
export type Segment = z.infer<typeof SegmentSchema>;

// ============================================================
// B-Roll Layer Component (with Sequence for proper playback)
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
  musicVolume = 0.10,
  videoVolume = 1,
  // 📝 TikTok-style Captions
  captions = [],
  showCaptions = true,
  captionStyle = {},
  // 👤 Face centering
  faceOffsetX = 0,
  faceOffsetY = 0,
  faceScale = 1,
}) => {
  console.log('[SplitTalkingHead] backgroundMusic:', backgroundMusic, 'musicVolume:', musicVolume);
  const frame = useCurrentFrame();
  const { height, fps, durationInFrames } = useVideoConfig();
  const audioRef = useRef<HTMLAudioElement | null>(null);
  const lastFrameRef = useRef(0);

  // 🎵 HTML5 Audio for background music (bypasses Remotion Audio issues)
  useEffect(() => {
    if (!backgroundMusic) return;

    const audioSrc = resolveMediaPath(backgroundMusic);
    console.log('[SplitTalkingHead] Creating HTML5 Audio:', audioSrc);

    const audio = new window.Audio(audioSrc);
    audio.loop = true;
    audio.volume = musicVolume;
    audio.crossOrigin = 'anonymous';
    audioRef.current = audio;

    return () => {
      audio.pause();
      audio.src = '';
      audioRef.current = null;
    };
  }, [backgroundMusic]);

  // Sync audio with player - use interval to detect play/pause
  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return;

    audio.volume = musicVolume;

    // Stop audio at video end
    if (frame >= durationInFrames - 1) {
      if (!audio.paused) {
        console.log('[Audio] Stopping - video ended');
        audio.pause();
      }
      return;
    }

    // Update frame ref and check if playing
    const wasPlaying = lastFrameRef.current !== frame;
    lastFrameRef.current = frame;

    if (wasPlaying && audio.paused) {
      const currentTime = frame / fps;
      audio.currentTime = currentTime % (audio.duration || 1000);
      audio.play().catch(e => console.warn('[Audio] Play failed:', e));
    }
  }, [frame, fps, musicVolume, durationInFrames]);

  // Detect pause - simplified, less CPU intensive
  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return;

    let lastCheck = lastFrameRef.current;
    const checkPause = setInterval(() => {
      if (lastFrameRef.current === lastCheck && !audio.paused) {
        audio.pause();
      }
      lastCheck = lastFrameRef.current;
    }, 300); // Check every 300ms instead of 150ms

    return () => clearInterval(checkPause);
  }, []);

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
          volume={videoVolume}
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
          fontSize={captionStyle?.fontSize ?? CAPTION_DEFAULTS.fontSize}
          textColor={captionStyle?.textColor ?? CAPTION_DEFAULTS.textColor}
          topPercent={isSplit ? splitRatio * 100 : 75} // At border in split, BOTTOM in fullscreen
          maxWords={CAPTION_DEFAULTS.maxWords}
          fontFamily={captionStyle?.fontFamily}
          fontWeight={captionStyle?.fontWeight}
          showShadow={captionStyle?.showShadow}
        />
      )}

      {/* ========== BACKGROUND MUSIC ========== */}
      {/* Audio is handled via HTML5 Audio in useEffect above */}
    </AbsoluteFill>
  );
};

export default SplitTalkingHead;
