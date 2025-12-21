/**
 * TikTok-style Captions Component for Remotion
 *
 * Style based on reel_01.mp4:
 * - Bright yellow #FFFF00
 * - UPPERCASE, bold (NOT italic)
 * - NO background, thin black outline
 * - 1-2 words at a time
 * - Dynamic position based on layout mode
 */

import React, { useState, useEffect } from 'react';
import {
  AbsoluteFill,
  useCurrentFrame,
  useVideoConfig,
  interpolate,
  spring,
  delayRender,
  continueRender,
} from 'remotion';
import { loadFont, fontFamily } from '@remotion/google-fonts/Inter';
import { CAPTION_DEFAULTS } from '../constants/captions';

// Load Inter Black (900) with Cyrillic support
const { waitUntilDone } = loadFont('normal', {
  weights: ['900'],
  subsets: ['cyrillic', 'latin'],
});

// Our own Caption interface (not from @remotion/captions)
export interface Caption {
  text: string;
  startMs: number;
  endMs: number;
  timestampMs?: number;
  confidence?: number;
}

export interface CaptionsProps {
  /** Array of caption objects with timing info */
  captions: Caption[];
  /** Font size in pixels (default: 56) */
  fontSize?: number;
  /** Text color - main color for captions (default: bright yellow) */
  textColor?: string;
  /** Highlight color for current word - kept for compatibility */
  highlightColor?: string;
  /** Position from top in % (default: 50 = center). Use ~50 for split border, ~50 for fullscreen center */
  topPercent?: number;
  /** Max words to show at once (default: 2) */
  maxWords?: number;
  /** Not used anymore but kept for compatibility */
  combineWithinMs?: number;
  /** Not used anymore but kept for compatibility */
  backgroundColor?: string;
  /** Not used anymore but kept for compatibility */
  position?: 'bottom' | 'center';
  /** Not used anymore but kept for compatibility */
  bottomPercent?: number;
}

/**
 * reel_01.mp4 style captions - 1-2 words, yellow, bold, dynamic position
 */
export const Captions: React.FC<CaptionsProps> = ({
  captions,
  fontSize = CAPTION_DEFAULTS.fontSize,
  textColor = CAPTION_DEFAULTS.textColor,
  topPercent = 50, // Center by default, overridden by SplitTalkingHead
  maxWords = CAPTION_DEFAULTS.maxWords,
}) => {
  const frame = useCurrentFrame();
  const { fps } = useVideoConfig();
  const currentTimeMs = (frame / fps) * 1000;

  // Wait for Montserrat font to load before rendering
  const [fontHandle] = useState(() => delayRender('Loading Montserrat font'));

  useEffect(() => {
    waitUntilDone()
      .then(() => continueRender(fontHandle))
      .catch((err) => {
        console.error('Font loading failed:', err);
        continueRender(fontHandle);
      });
  }, [fontHandle]);

  // Skip if no captions
  if (!captions || captions.length === 0) {
    return null;
  }

  // Find current word index
  let currentWordIndex = -1;
  for (let i = 0; i < captions.length; i++) {
    const cap = captions[i];
    if (currentTimeMs >= cap.startMs && currentTimeMs < cap.endMs) {
      currentWordIndex = i;
      break;
    }
    // If we're between words, show the previous word briefly
    if (i < captions.length - 1) {
      const nextCap = captions[i + 1];
      if (currentTimeMs >= cap.endMs && currentTimeMs < nextCap.startMs) {
        // Only show if gap is small (< 300ms)
        if (nextCap.startMs - cap.endMs < 300) {
          currentWordIndex = i;
        }
        break;
      }
    }
  }

  // If no current word found
  if (currentWordIndex === -1) {
    // Check if we're before the first caption
    if (captions.length > 0 && currentTimeMs < captions[0].startMs) {
      return null;
    }
    // Check if we're after the last caption
    const lastCaption = captions[captions.length - 1];
    if (currentTimeMs > lastCaption.endMs + 200) {
      return null;
    }
    // Show last word briefly after it ends
    if (currentTimeMs >= lastCaption.startMs) {
      currentWordIndex = captions.length - 1;
    } else {
      return null;
    }
  }

  // Get current word
  const currentCaption = captions[currentWordIndex];

  // Get words to display (1-2 words)
  const visibleWords: Caption[] = [currentCaption];

  // Add next word if it starts soon (within 150ms)
  if (currentWordIndex < captions.length - 1 && maxWords > 1) {
    const nextWord = captions[currentWordIndex + 1];
    if (nextWord.startMs - currentCaption.endMs < 150) {
      visibleWords.push(nextWord);
    }
  }

  // Animation - pop in effect
  const wordStartFrame = (currentCaption.startMs / 1000) * fps;
  const framesSinceStart = frame - wordStartFrame;

  const scale = spring({
    frame: framesSinceStart,
    fps,
    config: {
      damping: 12,
      stiffness: 180,
      mass: 0.4,
    },
  });

  const opacity = interpolate(framesSinceStart, [0, 2], [0, 1], {
    extrapolateLeft: 'clamp',
    extrapolateRight: 'clamp',
  });

  // Build caption text
  const captionText = visibleWords.map(w => w.text).join(' ').toUpperCase();

  return (
    <AbsoluteFill
      style={{
        zIndex: 100,
        pointerEvents: 'none',
      }}
    >
      <div
        style={{
          position: 'absolute',
          top: `${topPercent}%`,
          left: 0,
          right: 0,
          transform: `translateY(-50%) scale(${scale})`,
          opacity,
          display: 'flex',
          justifyContent: 'center',
          padding: '0 20px',
        }}
      >
        <p
          style={{
            fontSize,
            fontFamily: fontFamily,
            fontWeight: 900,
            fontStyle: 'normal', // NOT italic - straight like reel_01.mp4
            color: textColor,
            textAlign: 'center',
            margin: 0,
            lineHeight: 1.1,
            // Thin black outline for contrast (like reel_01.mp4)
            textShadow: `
              -1px -1px 0 #000,
              1px -1px 0 #000,
              -1px 1px 0 #000,
              1px 1px 0 #000,
              -2px -2px 0 #000,
              2px -2px 0 #000,
              -2px 2px 0 #000,
              2px 2px 0 #000
            `,
            letterSpacing: '0.02em',
          }}
        >
          {captionText}
        </p>
      </div>
    </AbsoluteFill>
  );
};

export default Captions;
