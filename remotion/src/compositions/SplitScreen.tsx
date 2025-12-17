import React from "react";
import {
  AbsoluteFill,
  Video,
  Img,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  Sequence,
} from "remotion";
import { z } from "zod";

export const SplitScreenSchema = z.object({
  media: z.array(z.object({
    url: z.string(),
    type: z.enum(["video", "image"]),
  })).min(2).max(4),
  layout: z.enum(["2x1", "1x2", "2x2"]).default("2x2"),
  gap: z.number().default(10),
  borderRadius: z.number().default(20),
  animationType: z.enum(["scale", "slide", "fade"]).default("scale"),
  backgroundColor: z.string().default("#000000"),
});

type SplitScreenProps = z.infer<typeof SplitScreenSchema>;

type MediaItem = {
  url: string;
  type: "video" | "image";
};

const MediaCell: React.FC<{
  item: MediaItem;
  index: number;
  frame: number;
  fps: number;
  durationInFrames: number;
  animationType: string;
  borderRadius: number;
  style: React.CSSProperties;
}> = ({ item, index, frame, fps, durationInFrames, animationType, borderRadius, style }) => {
  // Staggered animation entry
  const entryDelay = index * 8;
  const entryProgress = interpolate(
    frame - entryDelay,
    [0, 20],
    [0, 1],
    { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
  );

  // Exit animation
  const exitProgress = interpolate(
    frame,
    [durationInFrames - 30, durationInFrames],
    [1, 0],
    { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
  );

  let transform = "";
  let opacity = entryProgress * exitProgress;

  switch (animationType) {
    case "scale":
      const scale = interpolate(entryProgress, [0, 1], [0.8, 1]);
      transform = `scale(${scale})`;
      break;
    case "slide":
      const directions = [
        { x: -100, y: 0 },   // left
        { x: 100, y: 0 },    // right
        { x: 0, y: -100 },   // top
        { x: 0, y: 100 },    // bottom
      ];
      const dir = directions[index % 4];
      const slideX = interpolate(entryProgress, [0, 1], [dir.x, 0]);
      const slideY = interpolate(entryProgress, [0, 1], [dir.y, 0]);
      transform = `translate(${slideX}px, ${slideY}px)`;
      break;
    case "fade":
      // Just opacity
      break;
  }

  const cellStyle: React.CSSProperties = {
    ...style,
    borderRadius,
    overflow: "hidden",
    transform,
    opacity,
    boxShadow: "0 4px 20px rgba(0,0,0,0.3)",
  };

  return (
    <div style={cellStyle}>
      {item.type === "video" ? (
        <Video
          src={item.url}
          style={{
            width: "100%",
            height: "100%",
            objectFit: "cover",
          }}
        />
      ) : (
        <Img
          src={item.url}
          style={{
            width: "100%",
            height: "100%",
            objectFit: "cover",
          }}
        />
      )}
    </div>
  );
};

export const SplitScreen: React.FC<SplitScreenProps> = ({
  media,
  layout,
  gap,
  borderRadius,
  animationType,
  backgroundColor,
}) => {
  const frame = useCurrentFrame();
  const { fps, width, height, durationInFrames } = useVideoConfig();

  // Calculate cell dimensions based on layout
  const getGridConfig = () => {
    switch (layout) {
      case "2x1":
        return { cols: 2, rows: 1 };
      case "1x2":
        return { cols: 1, rows: 2 };
      case "2x2":
      default:
        return { cols: 2, rows: 2 };
    }
  };

  const { cols, rows } = getGridConfig();
  const cellWidth = (width - gap * (cols + 1)) / cols;
  const cellHeight = (height - gap * (rows + 1)) / rows;

  // Position cells
  const getCellPosition = (index: number): React.CSSProperties => {
    const col = index % cols;
    const row = Math.floor(index / cols);
    return {
      position: "absolute",
      left: gap + col * (cellWidth + gap),
      top: gap + row * (cellHeight + gap),
      width: cellWidth,
      height: cellHeight,
    };
  };

  return (
    <AbsoluteFill style={{ backgroundColor }}>
      {media.slice(0, cols * rows).map((item, index) => (
        <MediaCell
          key={index}
          item={item}
          index={index}
          frame={frame}
          fps={fps}
          durationInFrames={durationInFrames}
          animationType={animationType}
          borderRadius={borderRadius}
          style={getCellPosition(index)}
        />
      ))}
    </AbsoluteFill>
  );
};
