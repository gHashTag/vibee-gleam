import React from "react";
import {
  AbsoluteFill,
  Video,
  Img,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  spring,
  Sequence,
} from "remotion";
import { z } from "zod";

export const SubtitlesSchema = z.object({
  // Background media
  backgroundUrl: z.string().optional(),
  backgroundType: z.enum(["video", "image", "color"]).default("color"),
  backgroundColor: z.string().default("#000000"),

  // Subtitles data
  subtitles: z.array(z.object({
    text: z.string(),
    startFrame: z.number(),
    endFrame: z.number(),
    highlight: z.string().optional(), // word to highlight
  })),

  // Styling
  style: z.enum(["classic", "modern", "karaoke", "typewriter", "bounce"]).default("modern"),
  fontSize: z.number().default(48),
  fontColor: z.string().default("#ffffff"),
  highlightColor: z.string().default("#FFD700"),
  backgroundColor2: z.string().default("rgba(0,0,0,0.7)"), // subtitle background
  position: z.enum(["top", "center", "bottom"]).default("bottom"),
  maxWidth: z.number().default(900), // pixels
});

type SubtitlesProps = z.infer<typeof SubtitlesSchema>;

type SubtitleEntry = {
  text: string;
  startFrame: number;
  endFrame: number;
  highlight?: string;
};

const ClassicSubtitle: React.FC<{
  text: string;
  highlight?: string;
  progress: number;
  fontColor: string;
  highlightColor: string;
  backgroundColor: string;
  fontSize: number;
}> = ({ text, highlight, progress, fontColor, highlightColor, backgroundColor, fontSize }) => {
  const opacity = interpolate(progress, [0, 0.1, 0.9, 1], [0, 1, 1, 0]);

  // Highlight word if specified
  const renderText = () => {
    if (!highlight) {
      return text;
    }
    const parts = text.split(new RegExp(`(${highlight})`, "gi"));
    return parts.map((part, i) =>
      part.toLowerCase() === highlight.toLowerCase() ? (
        <span key={i} style={{ color: highlightColor, fontWeight: "bold" }}>
          {part}
        </span>
      ) : (
        part
      )
    );
  };

  return (
    <div
      style={{
        opacity,
        padding: "12px 24px",
        backgroundColor,
        borderRadius: 8,
      }}
    >
      <span
        style={{
          color: fontColor,
          fontSize,
          fontFamily: "Inter, system-ui, sans-serif",
          fontWeight: 500,
          textAlign: "center",
        }}
      >
        {renderText()}
      </span>
    </div>
  );
};

const ModernSubtitle: React.FC<{
  text: string;
  highlight?: string;
  progress: number;
  frame: number;
  fps: number;
  startFrame: number;
  fontColor: string;
  highlightColor: string;
  fontSize: number;
}> = ({ text, highlight, progress, frame, fps, startFrame, fontColor, highlightColor, fontSize }) => {
  const entryProgress = spring({
    frame: frame - startFrame,
    fps,
    config: { damping: 100 },
  });

  const translateY = interpolate(entryProgress, [0, 1], [30, 0]);
  const opacity = interpolate(progress, [0, 0.1, 0.9, 1], [0, 1, 1, 0]);
  const scale = interpolate(entryProgress, [0, 1], [0.9, 1]);

  // Highlight word
  const renderText = () => {
    if (!highlight) {
      return text;
    }
    const parts = text.split(new RegExp(`(${highlight})`, "gi"));
    return parts.map((part, i) =>
      part.toLowerCase() === highlight.toLowerCase() ? (
        <span
          key={i}
          style={{
            color: highlightColor,
            fontWeight: "bold",
            textShadow: `0 0 20px ${highlightColor}`,
          }}
        >
          {part}
        </span>
      ) : (
        part
      )
    );
  };

  return (
    <div
      style={{
        opacity,
        transform: `translateY(${translateY}px) scale(${scale})`,
      }}
    >
      <span
        style={{
          color: fontColor,
          fontSize,
          fontFamily: "Inter, system-ui, sans-serif",
          fontWeight: 600,
          textAlign: "center",
          textShadow: "0 2px 10px rgba(0,0,0,0.8)",
        }}
      >
        {renderText()}
      </span>
    </div>
  );
};

const KaraokeSubtitle: React.FC<{
  text: string;
  progress: number;
  fontColor: string;
  highlightColor: string;
  fontSize: number;
}> = ({ text, progress, fontColor, highlightColor, fontSize }) => {
  const opacity = interpolate(progress, [0, 0.05, 0.95, 1], [0, 1, 1, 0]);

  // Calculate how much of the text should be highlighted
  const highlightProgress = interpolate(progress, [0.1, 0.9], [0, 100]);

  return (
    <div style={{ opacity, position: "relative" }}>
      {/* Background text */}
      <span
        style={{
          color: fontColor,
          fontSize,
          fontFamily: "Inter, system-ui, sans-serif",
          fontWeight: 600,
          opacity: 0.5,
        }}
      >
        {text}
      </span>

      {/* Highlighted overlay */}
      <span
        style={{
          position: "absolute",
          left: 0,
          top: 0,
          color: highlightColor,
          fontSize,
          fontFamily: "Inter, system-ui, sans-serif",
          fontWeight: 600,
          WebkitBackgroundClip: "text",
          overflow: "hidden",
          whiteSpace: "nowrap",
          width: `${highlightProgress}%`,
          textShadow: `0 0 20px ${highlightColor}`,
        }}
      >
        {text}
      </span>
    </div>
  );
};

const TypewriterSubtitle: React.FC<{
  text: string;
  progress: number;
  fontColor: string;
  fontSize: number;
}> = ({ text, progress, fontColor, fontSize }) => {
  const opacity = interpolate(progress, [0, 0.05, 0.95, 1], [0, 1, 1, 0]);

  // Calculate how many characters to show
  const charProgress = interpolate(progress, [0.05, 0.7], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });
  const charsToShow = Math.floor(charProgress * text.length);
  const displayText = text.slice(0, charsToShow);

  // Cursor blink
  const showCursor = Math.floor(progress * 20) % 2 === 0 && charProgress < 1;

  return (
    <div style={{ opacity }}>
      <span
        style={{
          color: fontColor,
          fontSize,
          fontFamily: "'Courier New', monospace",
          fontWeight: 500,
        }}
      >
        {displayText}
        {showCursor && (
          <span style={{ opacity: 1 }}>|</span>
        )}
      </span>
    </div>
  );
};

const BounceSubtitle: React.FC<{
  text: string;
  progress: number;
  frame: number;
  fps: number;
  startFrame: number;
  fontColor: string;
  fontSize: number;
}> = ({ text, progress, frame, fps, startFrame, fontColor, fontSize }) => {
  const opacity = interpolate(progress, [0, 0.1, 0.9, 1], [0, 1, 1, 0]);

  // Split into words and animate each
  const words = text.split(" ");

  return (
    <div
      style={{
        opacity,
        display: "flex",
        flexWrap: "wrap",
        justifyContent: "center",
        gap: "12px",
      }}
    >
      {words.map((word, i) => {
        const wordDelay = i * 3;
        const wordProgress = spring({
          frame: frame - startFrame - wordDelay,
          fps,
          config: {
            damping: 80,
            stiffness: 200,
          },
        });

        const translateY = interpolate(wordProgress, [0, 1], [40, 0]);
        const scale = interpolate(wordProgress, [0, 1], [0.5, 1]);

        return (
          <span
            key={i}
            style={{
              color: fontColor,
              fontSize,
              fontFamily: "Inter, system-ui, sans-serif",
              fontWeight: 600,
              transform: `translateY(${translateY}px) scale(${scale})`,
              textShadow: "0 2px 10px rgba(0,0,0,0.8)",
            }}
          >
            {word}
          </span>
        );
      })}
    </div>
  );
};

export const Subtitles: React.FC<SubtitlesProps> = ({
  backgroundUrl,
  backgroundType,
  backgroundColor,
  subtitles,
  style,
  fontSize,
  fontColor,
  highlightColor,
  backgroundColor2,
  position,
  maxWidth,
}) => {
  const frame = useCurrentFrame();
  const { fps, durationInFrames } = useVideoConfig();

  // Find current subtitle
  const currentSubtitle = subtitles.find(
    (s) => frame >= s.startFrame && frame <= s.endFrame
  );

  // Calculate progress within subtitle
  const getProgress = (sub: SubtitleEntry) => {
    const duration = sub.endFrame - sub.startFrame;
    return (frame - sub.startFrame) / duration;
  };

  // Position styles
  const positionStyle: React.CSSProperties = {
    position: "absolute",
    left: "50%",
    transform: "translateX(-50%)",
    maxWidth,
    padding: "0 40px",
    ...(position === "top" && { top: 100 }),
    ...(position === "center" && { top: "50%", transform: "translate(-50%, -50%)" }),
    ...(position === "bottom" && { bottom: 150 }),
  };

  // Global fade
  const globalFade = interpolate(
    frame,
    [durationInFrames - 20, durationInFrames],
    [1, 0],
    { extrapolateLeft: "clamp" }
  );

  return (
    <AbsoluteFill style={{ backgroundColor, opacity: globalFade }}>
      {/* Background */}
      {backgroundType === "video" && backgroundUrl && (
        <Video
          src={backgroundUrl}
          style={{ width: "100%", height: "100%", objectFit: "cover" }}
        />
      )}
      {backgroundType === "image" && backgroundUrl && (
        <Img
          src={backgroundUrl}
          style={{ width: "100%", height: "100%", objectFit: "cover" }}
        />
      )}

      {/* Subtitle container */}
      <div style={positionStyle}>
        {currentSubtitle && (
          <>
            {style === "classic" && (
              <ClassicSubtitle
                text={currentSubtitle.text}
                highlight={currentSubtitle.highlight}
                progress={getProgress(currentSubtitle)}
                fontColor={fontColor}
                highlightColor={highlightColor}
                backgroundColor={backgroundColor2}
                fontSize={fontSize}
              />
            )}
            {style === "modern" && (
              <ModernSubtitle
                text={currentSubtitle.text}
                highlight={currentSubtitle.highlight}
                progress={getProgress(currentSubtitle)}
                frame={frame}
                fps={fps}
                startFrame={currentSubtitle.startFrame}
                fontColor={fontColor}
                highlightColor={highlightColor}
                fontSize={fontSize}
              />
            )}
            {style === "karaoke" && (
              <KaraokeSubtitle
                text={currentSubtitle.text}
                progress={getProgress(currentSubtitle)}
                fontColor={fontColor}
                highlightColor={highlightColor}
                fontSize={fontSize}
              />
            )}
            {style === "typewriter" && (
              <TypewriterSubtitle
                text={currentSubtitle.text}
                progress={getProgress(currentSubtitle)}
                fontColor={fontColor}
                fontSize={fontSize}
              />
            )}
            {style === "bounce" && (
              <BounceSubtitle
                text={currentSubtitle.text}
                progress={getProgress(currentSubtitle)}
                frame={frame}
                fps={fps}
                startFrame={currentSubtitle.startFrame}
                fontColor={fontColor}
                fontSize={fontSize}
              />
            )}
          </>
        )}
      </div>
    </AbsoluteFill>
  );
};
