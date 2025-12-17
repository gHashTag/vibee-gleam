import React from "react";
import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  spring,
} from "remotion";
import { z } from "zod";

export const TextOverlaySchema = z.object({
  title: z.string(),
  subtitle: z.string(),
  backgroundColor: z.string(),
  textColor: z.string(),
  accentColor: z.string(),
});

type TextOverlayProps = z.infer<typeof TextOverlaySchema>;

export const TextOverlay: React.FC<TextOverlayProps> = ({
  title,
  subtitle,
  backgroundColor,
  textColor,
  accentColor,
}) => {
  const frame = useCurrentFrame();
  const { fps, durationInFrames } = useVideoConfig();

  // Title animation
  const titleProgress = spring({
    frame,
    fps,
    config: {
      damping: 200,
    },
  });

  const titleOpacity = interpolate(titleProgress, [0, 1], [0, 1]);
  const titleY = interpolate(titleProgress, [0, 1], [50, 0]);

  // Subtitle animation - starts slightly after title
  const subtitleProgress = spring({
    frame: frame - 15,
    fps,
    config: {
      damping: 200,
    },
  });

  const subtitleOpacity = interpolate(subtitleProgress, [0, 1], [0, 1]);
  const subtitleY = interpolate(subtitleProgress, [0, 1], [30, 0]);

  // Accent line animation
  const lineWidth = interpolate(
    frame,
    [20, 50],
    [0, 200],
    { extrapolateRight: "clamp" }
  );

  // Fade out near the end
  const fadeOut = interpolate(
    frame,
    [durationInFrames - 30, durationInFrames],
    [1, 0],
    { extrapolateLeft: "clamp" }
  );

  return (
    <AbsoluteFill
      style={{
        backgroundColor,
        justifyContent: "center",
        alignItems: "center",
        opacity: fadeOut,
      }}
    >
      {/* Decorative gradient background */}
      <div
        style={{
          position: "absolute",
          top: 0,
          left: 0,
          right: 0,
          bottom: 0,
          background: `radial-gradient(circle at 50% 50%, ${accentColor}22 0%, transparent 50%)`,
        }}
      />

      {/* Content container */}
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
          padding: 60,
        }}
      >
        {/* Title */}
        <h1
          style={{
            color: textColor,
            fontSize: 72,
            fontWeight: "bold",
            fontFamily: "Inter, system-ui, sans-serif",
            textAlign: "center",
            margin: 0,
            opacity: titleOpacity,
            transform: `translateY(${titleY}px)`,
            textShadow: `0 4px 20px ${accentColor}66`,
          }}
        >
          {title}
        </h1>

        {/* Accent line */}
        <div
          style={{
            width: lineWidth,
            height: 4,
            backgroundColor: accentColor,
            borderRadius: 2,
            margin: "30px 0",
          }}
        />

        {/* Subtitle */}
        <p
          style={{
            color: textColor,
            fontSize: 32,
            fontFamily: "Inter, system-ui, sans-serif",
            textAlign: "center",
            margin: 0,
            opacity: subtitleOpacity * 0.8,
            transform: `translateY(${subtitleY}px)`,
          }}
        >
          {subtitle}
        </p>
      </div>
    </AbsoluteFill>
  );
};
