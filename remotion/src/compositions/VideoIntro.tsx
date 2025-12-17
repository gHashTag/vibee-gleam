import React from "react";
import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  spring,
  Img,
} from "remotion";
import { z } from "zod";

export const VideoIntroSchema = z.object({
  brandName: z.string(),
  tagline: z.string(),
  logoUrl: z.string(),
  primaryColor: z.string(),
  secondaryColor: z.string(),
});

type VideoIntroProps = z.infer<typeof VideoIntroSchema>;

export const VideoIntro: React.FC<VideoIntroProps> = ({
  brandName,
  tagline,
  logoUrl,
  primaryColor,
  secondaryColor,
}) => {
  const frame = useCurrentFrame();
  const { fps, durationInFrames, width, height } = useVideoConfig();

  // Logo/brand entrance
  const logoScale = spring({
    frame,
    fps,
    config: {
      mass: 0.5,
      damping: 15,
    },
  });

  // Brand name animation
  const brandProgress = spring({
    frame: frame - 20,
    fps,
    config: {
      damping: 200,
    },
  });

  // Tagline animation
  const taglineOpacity = interpolate(
    frame,
    [40, 60],
    [0, 1],
    { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
  );

  // Rotating gradient
  const gradientRotation = interpolate(frame, [0, durationInFrames], [0, 360]);

  // Exit animation
  const exitProgress = interpolate(
    frame,
    [durationInFrames - 30, durationInFrames],
    [0, 1],
    { extrapolateLeft: "clamp" }
  );

  const exitScale = interpolate(exitProgress, [0, 1], [1, 1.5]);
  const exitOpacity = interpolate(exitProgress, [0, 1], [1, 0]);

  return (
    <AbsoluteFill
      style={{
        backgroundColor: "#0a0a0f",
        justifyContent: "center",
        alignItems: "center",
        opacity: exitOpacity,
        transform: `scale(${exitScale})`,
      }}
    >
      {/* Animated gradient background */}
      <div
        style={{
          position: "absolute",
          width: width * 2,
          height: height * 2,
          left: -width / 2,
          top: -height / 2,
          background: `conic-gradient(from ${gradientRotation}deg, ${primaryColor}33, ${secondaryColor}33, ${primaryColor}33)`,
          filter: "blur(100px)",
        }}
      />

      {/* Floating particles */}
      {[...Array(8)].map((_, i) => (
        <div
          key={i}
          style={{
            position: "absolute",
            width: 10,
            height: 10,
            borderRadius: "50%",
            backgroundColor: i % 2 === 0 ? primaryColor : secondaryColor,
            left: `${15 + i * 10}%`,
            top: `${20 + (i % 3) * 25}%`,
            opacity: interpolate(
              frame,
              [i * 5, i * 5 + 30],
              [0, 0.6],
              { extrapolateRight: "clamp" }
            ),
            transform: `translateY(${Math.sin((frame + i * 10) * 0.1) * 20}px)`,
          }}
        />
      ))}

      {/* Content container */}
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
          zIndex: 10,
        }}
      >
        {/* Logo */}
        {logoUrl ? (
          <Img
            src={logoUrl}
            style={{
              width: 120,
              height: 120,
              borderRadius: 24,
              transform: `scale(${logoScale})`,
              boxShadow: `0 20px 60px ${primaryColor}66`,
            }}
          />
        ) : (
          <div
            style={{
              width: 120,
              height: 120,
              borderRadius: 24,
              background: `linear-gradient(135deg, ${primaryColor}, ${secondaryColor})`,
              transform: `scale(${logoScale})`,
              boxShadow: `0 20px 60px ${primaryColor}66`,
              display: "flex",
              justifyContent: "center",
              alignItems: "center",
              fontSize: 48,
              fontWeight: "bold",
              color: "#fff",
              fontFamily: "Inter, system-ui, sans-serif",
            }}
          >
            {brandName.charAt(0)}
          </div>
        )}

        {/* Brand name */}
        <h1
          style={{
            fontSize: 80,
            fontWeight: "bold",
            fontFamily: "Inter, system-ui, sans-serif",
            background: `linear-gradient(90deg, ${primaryColor}, ${secondaryColor})`,
            WebkitBackgroundClip: "text",
            WebkitTextFillColor: "transparent",
            backgroundClip: "text",
            margin: "40px 0 20px",
            opacity: brandProgress,
            transform: `translateY(${interpolate(brandProgress, [0, 1], [30, 0])}px)`,
          }}
        >
          {brandName}
        </h1>

        {/* Tagline */}
        <p
          style={{
            fontSize: 28,
            color: "#888",
            fontFamily: "Inter, system-ui, sans-serif",
            opacity: taglineOpacity,
            letterSpacing: 4,
            textTransform: "uppercase",
          }}
        >
          {tagline}
        </p>
      </div>
    </AbsoluteFill>
  );
};
