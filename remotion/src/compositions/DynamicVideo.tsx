import React from "react";
import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  spring,
  Sequence,
  Img,
} from "remotion";
import { z } from "zod";

export const DynamicVideoSchema = z.object({
  userName: z.string(),
  message: z.string(),
  avatarUrl: z.string(),
  items: z.array(z.object({
    title: z.string(),
    value: z.string(),
  })),
  theme: z.enum(["dark", "light"]),
});

type DynamicVideoProps = z.infer<typeof DynamicVideoSchema>;

const themes = {
  dark: {
    bg: "#0f0f1a",
    card: "#1a1a2e",
    text: "#ffffff",
    textSecondary: "#888899",
    accent: "#e94560",
    gradient: "linear-gradient(135deg, #667eea, #764ba2)",
  },
  light: {
    bg: "#f5f5f7",
    card: "#ffffff",
    text: "#1a1a2e",
    textSecondary: "#666677",
    accent: "#e94560",
    gradient: "linear-gradient(135deg, #667eea, #764ba2)",
  },
};

export const DynamicVideo: React.FC<DynamicVideoProps> = ({
  userName,
  message,
  avatarUrl,
  items,
  theme,
}) => {
  const frame = useCurrentFrame();
  const { fps, durationInFrames } = useVideoConfig();
  const colors = themes[theme];

  // Header animation
  const headerProgress = spring({
    frame,
    fps,
    config: { damping: 200 },
  });

  // Message animation
  const messageProgress = spring({
    frame: frame - 20,
    fps,
    config: { damping: 200 },
  });

  // Exit animation
  const exitOpacity = interpolate(
    frame,
    [durationInFrames - 30, durationInFrames],
    [1, 0],
    { extrapolateLeft: "clamp" }
  );

  return (
    <AbsoluteFill
      style={{
        backgroundColor: colors.bg,
        opacity: exitOpacity,
      }}
    >
      {/* Gradient accent */}
      <div
        style={{
          position: "absolute",
          top: 0,
          left: 0,
          right: 0,
          height: 200,
          background: colors.gradient,
          opacity: 0.8,
        }}
      />

      {/* Header */}
      <div
        style={{
          position: "absolute",
          top: 60,
          left: 0,
          right: 0,
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          gap: 20,
          opacity: headerProgress,
          transform: `translateY(${interpolate(headerProgress, [0, 1], [-30, 0])}px)`,
        }}
      >
        {/* Avatar */}
        {avatarUrl ? (
          <Img
            src={avatarUrl}
            style={{
              width: 80,
              height: 80,
              borderRadius: 40,
              border: `4px solid ${colors.accent}`,
            }}
          />
        ) : (
          <div
            style={{
              width: 80,
              height: 80,
              borderRadius: 40,
              backgroundColor: colors.accent,
              display: "flex",
              justifyContent: "center",
              alignItems: "center",
              fontSize: 32,
              fontWeight: "bold",
              color: "#fff",
              fontFamily: "Inter, system-ui, sans-serif",
            }}
          >
            {userName.charAt(0).toUpperCase()}
          </div>
        )}
        <h2
          style={{
            color: "#fff",
            fontSize: 36,
            fontWeight: "bold",
            fontFamily: "Inter, system-ui, sans-serif",
            margin: 0,
          }}
        >
          {userName}
        </h2>
      </div>

      {/* Main message card */}
      <div
        style={{
          position: "absolute",
          top: 240,
          left: 40,
          right: 40,
          backgroundColor: colors.card,
          borderRadius: 24,
          padding: 40,
          boxShadow: "0 20px 60px rgba(0,0,0,0.3)",
          opacity: messageProgress,
          transform: `translateY(${interpolate(messageProgress, [0, 1], [30, 0])}px)`,
        }}
      >
        <p
          style={{
            color: colors.text,
            fontSize: 32,
            fontFamily: "Inter, system-ui, sans-serif",
            lineHeight: 1.6,
            margin: 0,
          }}
        >
          {message}
        </p>
      </div>

      {/* Dynamic items */}
      {items.map((item, index) => (
        <Sequence from={60 + index * 15} key={index}>
          <ItemCard
            item={item}
            index={index}
            colors={colors}
            fps={fps}
          />
        </Sequence>
      ))}
    </AbsoluteFill>
  );
};

// Item card component
const ItemCard: React.FC<{
  item: { title: string; value: string };
  index: number;
  colors: typeof themes.dark;
  fps: number;
}> = ({ item, index, colors, fps }) => {
  const frame = useCurrentFrame();

  const progress = spring({
    frame,
    fps,
    config: { damping: 200 },
  });

  return (
    <div
      style={{
        position: "absolute",
        top: 520 + index * 160,
        left: 40,
        right: 40,
        backgroundColor: colors.card,
        borderRadius: 16,
        padding: 24,
        display: "flex",
        justifyContent: "space-between",
        alignItems: "center",
        opacity: progress,
        transform: `translateX(${interpolate(progress, [0, 1], [100, 0])}px)`,
      }}
    >
      <span
        style={{
          color: colors.textSecondary,
          fontSize: 24,
          fontFamily: "Inter, system-ui, sans-serif",
        }}
      >
        {item.title}
      </span>
      <span
        style={{
          color: colors.accent,
          fontSize: 28,
          fontWeight: "bold",
          fontFamily: "Inter, system-ui, sans-serif",
        }}
      >
        {item.value}
      </span>
    </div>
  );
};
