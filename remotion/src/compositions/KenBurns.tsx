import React from "react";
import {
  AbsoluteFill,
  Img,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  Easing,
} from "remotion";
import { z } from "zod";

export const KenBurnsSchema = z.object({
  images: z.array(z.object({
    url: z.string(),
    effect: z.enum(["zoomIn", "zoomOut", "panLeft", "panRight", "panUp", "panDown", "random"]).default("random"),
    startScale: z.number().optional(),
    endScale: z.number().optional(),
  })).min(1),
  imageDuration: z.number().default(120), // frames per image
  transitionDuration: z.number().default(30), // crossfade frames
  backgroundColor: z.string().default("#000000"),
  overlayColor: z.string().optional(), // optional color overlay
  overlayOpacity: z.number().default(0),
  vignette: z.boolean().default(true),
});

type KenBurnsProps = z.infer<typeof KenBurnsSchema>;

type ImageItem = {
  url: string;
  effect: "zoomIn" | "zoomOut" | "panLeft" | "panRight" | "panUp" | "panDown" | "random";
  startScale?: number;
  endScale?: number;
};

const getRandomEffect = (seed: number): "zoomIn" | "zoomOut" | "panLeft" | "panRight" | "panUp" | "panDown" => {
  const effects: Array<"zoomIn" | "zoomOut" | "panLeft" | "panRight" | "panUp" | "panDown"> = [
    "zoomIn", "zoomOut", "panLeft", "panRight", "panUp", "panDown"
  ];
  return effects[seed % effects.length];
};

const KenBurnsImage: React.FC<{
  item: ImageItem;
  progress: number; // 0 to 1
  opacity: number;
  index: number;
}> = ({ item, progress, opacity, index }) => {
  // Determine effect
  const effect = item.effect === "random" ? getRandomEffect(index) : item.effect;

  // Default scale values
  const defaultStartScale = 1.1;
  const defaultEndScale = 1.3;

  let transform = "";

  switch (effect) {
    case "zoomIn": {
      const startScale = item.startScale ?? 1.0;
      const endScale = item.endScale ?? 1.3;
      const scale = interpolate(progress, [0, 1], [startScale, endScale]);
      transform = `scale(${scale})`;
      break;
    }
    case "zoomOut": {
      const startScale = item.startScale ?? 1.3;
      const endScale = item.endScale ?? 1.0;
      const scale = interpolate(progress, [0, 1], [startScale, endScale]);
      transform = `scale(${scale})`;
      break;
    }
    case "panLeft": {
      const scale = defaultStartScale;
      const translateX = interpolate(progress, [0, 1], [10, -10]); // percentage
      transform = `scale(${scale}) translateX(${translateX}%)`;
      break;
    }
    case "panRight": {
      const scale = defaultStartScale;
      const translateX = interpolate(progress, [0, 1], [-10, 10]); // percentage
      transform = `scale(${scale}) translateX(${translateX}%)`;
      break;
    }
    case "panUp": {
      const scale = defaultStartScale;
      const translateY = interpolate(progress, [0, 1], [5, -5]); // percentage
      transform = `scale(${scale}) translateY(${translateY}%)`;
      break;
    }
    case "panDown": {
      const scale = defaultStartScale;
      const translateY = interpolate(progress, [0, 1], [-5, 5]); // percentage
      transform = `scale(${scale}) translateY(${translateY}%)`;
      break;
    }
  }

  return (
    <AbsoluteFill style={{ opacity }}>
      <Img
        src={item.url}
        style={{
          width: "100%",
          height: "100%",
          objectFit: "cover",
          transform,
        }}
      />
    </AbsoluteFill>
  );
};

const Vignette: React.FC<{ strength?: number }> = ({ strength = 0.7 }) => {
  return (
    <div
      style={{
        position: "absolute",
        top: 0,
        left: 0,
        right: 0,
        bottom: 0,
        background: `radial-gradient(ellipse at center, transparent 0%, transparent 40%, rgba(0,0,0,${strength}) 100%)`,
        pointerEvents: "none",
      }}
    />
  );
};

export const KenBurns: React.FC<KenBurnsProps> = ({
  images,
  imageDuration,
  transitionDuration,
  backgroundColor,
  overlayColor,
  overlayOpacity,
  vignette,
}) => {
  const frame = useCurrentFrame();
  const { durationInFrames } = useVideoConfig();

  // Calculate current image and progress
  const totalImageTime = imageDuration;
  const currentImageIndex = Math.floor(frame / totalImageTime) % images.length;
  const nextImageIndex = (currentImageIndex + 1) % images.length;

  // Time within current image
  const frameInImage = frame % totalImageTime;

  // Image progress (0 to 1)
  const imageProgress = frameInImage / totalImageTime;

  // Check if we're in crossfade transition
  const isTransitioning = frameInImage >= totalImageTime - transitionDuration;

  // Crossfade progress
  const crossfadeProgress = isTransitioning
    ? interpolate(
        frameInImage,
        [totalImageTime - transitionDuration, totalImageTime],
        [0, 1],
        { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
      )
    : 0;

  // Current image opacity
  const currentOpacity = isTransitioning ? 1 - crossfadeProgress : 1;

  // Global fade out
  const globalFade = interpolate(
    frame,
    [durationInFrames - 30, durationInFrames],
    [1, 0],
    { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
  );

  // Global fade in
  const globalFadeIn = interpolate(
    frame,
    [0, 30],
    [0, 1],
    { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
  );

  const currentImage = images[currentImageIndex];
  const nextImage = images[nextImageIndex];

  return (
    <AbsoluteFill style={{ backgroundColor, opacity: globalFade * globalFadeIn }}>
      {/* Current image */}
      <KenBurnsImage
        item={currentImage}
        progress={imageProgress}
        opacity={currentOpacity}
        index={currentImageIndex}
      />

      {/* Next image (during crossfade) */}
      {isTransitioning && (
        <KenBurnsImage
          item={nextImage}
          progress={crossfadeProgress * 0.3} // Start progress slightly into the effect
          opacity={crossfadeProgress}
          index={nextImageIndex}
        />
      )}

      {/* Color overlay */}
      {overlayColor && overlayOpacity > 0 && (
        <AbsoluteFill
          style={{
            backgroundColor: overlayColor,
            opacity: overlayOpacity,
            pointerEvents: "none",
          }}
        />
      )}

      {/* Vignette effect */}
      {vignette && <Vignette />}
    </AbsoluteFill>
  );
};
