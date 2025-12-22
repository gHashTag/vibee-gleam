import React from 'react';
import { AbsoluteFill, useCurrentFrame, useVideoConfig, interpolate } from 'remotion';
import type { Caption } from '@remotion/captions';
import { createTikTokStyleCaptions } from '@remotion/captions';

export interface CaptionsProps {
  /** Array of caption objects with timing info */
  captions: Caption[];
  /** Font size in pixels (default: 48) */
  fontSize?: number;
  /** Text color (default: white) */
  textColor?: string;
  /** Highlight color for current word (default: amber) */
  highlightColor?: string;
  /** Background color (default: semi-transparent black) */
  backgroundColor?: string;
  /** Position from bottom in % (default: 20) */
  bottomPercent?: number;
  /** Max width in % (default: 85) */
  maxWidthPercent?: number;
  /** Combine words within this many ms (default: 1500) */
  combineWithinMs?: number;
  /** Show text shadow for better visibility (default: true) */
  showShadow?: boolean;
  /** Font family (default: system) */
  fontFamily?: string;
  /** Font weight (default: 700) */
  fontWeight?: number;
}

/**
 * TikTok-style animated captions component
 * Displays captions with word-by-word highlighting
 */
export const Captions: React.FC<CaptionsProps> = ({
  captions,
  fontSize = 48,
  textColor = '#ffffff',
  highlightColor = '#f59e0b', // VIBEE amber
  backgroundColor = 'rgba(0, 0, 0, 0.6)',
  bottomPercent = 20,
  maxWidthPercent = 85,
  combineWithinMs = 1500,
  showShadow = true,
  fontFamily = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif',
  fontWeight = 700,
}) => {
  const frame = useCurrentFrame();
  const { fps } = useVideoConfig();
  const currentTimeMs = (frame / fps) * 1000;

  // Skip if no captions
  if (!captions || captions.length === 0) {
    return null;
  }

  // Create TikTok-style paginated captions
  const { pages } = createTikTokStyleCaptions({
    captions,
    combineTokensWithinMilliseconds: combineWithinMs,
  });

  // Find current page
  const currentPage = pages.find(
    (page) =>
      currentTimeMs >= page.startMs &&
      currentTimeMs < page.startMs + page.durationMs
  );

  if (!currentPage) {
    return null;
  }

  // Animation: scale in/out
  const pageProgress = (currentTimeMs - currentPage.startMs) / currentPage.durationMs;
  const scaleIn = interpolate(pageProgress, [0, 0.1], [0.9, 1], {
    extrapolateLeft: 'clamp',
    extrapolateRight: 'clamp',
  });
  const scaleOut = interpolate(pageProgress, [0.9, 1], [1, 0.9], {
    extrapolateLeft: 'clamp',
    extrapolateRight: 'clamp',
  });
  const scale = Math.min(scaleIn, scaleOut);

  const opacityIn = interpolate(pageProgress, [0, 0.05], [0, 1], {
    extrapolateLeft: 'clamp',
    extrapolateRight: 'clamp',
  });
  const opacityOut = interpolate(pageProgress, [0.95, 1], [1, 0], {
    extrapolateLeft: 'clamp',
    extrapolateRight: 'clamp',
  });
  const opacity = Math.min(opacityIn, opacityOut);

  const textShadow = showShadow
    ? '0 2px 4px rgba(0, 0, 0, 0.5), 0 4px 8px rgba(0, 0, 0, 0.3)'
    : 'none';

  return (
    <AbsoluteFill
      style={{
        justifyContent: 'flex-end',
        alignItems: 'center',
        paddingBottom: `${bottomPercent}%`,
      }}
    >
      <div
        style={{
          maxWidth: `${maxWidthPercent}%`,
          padding: '12px 24px',
          borderRadius: 12,
          backgroundColor,
          transform: `scale(${scale})`,
          opacity,
          backdropFilter: 'blur(8px)',
          WebkitBackdropFilter: 'blur(8px)',
        }}
      >
        <p
          style={{
            fontSize,
            fontFamily,
            fontWeight,
            color: textColor,
            textAlign: 'center',
            margin: 0,
            lineHeight: 1.3,
            textShadow,
          }}
        >
          {currentPage.tokens.map((token, index) => {
            // Check if this token is currently being spoken
            const isActive =
              currentTimeMs >= token.fromMs && currentTimeMs < token.toMs;

            // Check if this token has already been spoken
            const isPast = currentTimeMs >= token.toMs;

            return (
              <span
                key={index}
                style={{
                  color: isActive ? highlightColor : isPast ? textColor : 'rgba(255, 255, 255, 0.7)',
                  transition: 'color 0.1s ease-out',
                  // Scale up the active word slightly
                  transform: isActive ? 'scale(1.05)' : 'scale(1)',
                  display: 'inline-block',
                }}
              >
                {token.text}
              </span>
            );
          })}
        </p>
      </div>
    </AbsoluteFill>
  );
};

// Export default for convenience
export default Captions;
