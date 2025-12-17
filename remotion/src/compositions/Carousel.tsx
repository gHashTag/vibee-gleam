import React from "react";
import {
  AbsoluteFill,
  Img,
  Video,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  Easing,
} from "remotion";
import { z } from "zod";

export const CarouselSchema = z.object({
  slides: z.array(z.object({
    url: z.string(),
    type: z.enum(["video", "image"]),
    caption: z.string().optional(),
  })).min(1),
  transition: z.enum(["fade", "slide", "zoom", "swipe"]).default("fade"),
  transitionDuration: z.number().default(15), // frames
  slideInterval: z.number().default(90), // frames per slide
  backgroundColor: z.string().default("#000000"),
  captionColor: z.string().default("#ffffff"),
  showCaptions: z.boolean().default(true),
  showIndicators: z.boolean().default(true),
});

type CarouselProps = z.infer<typeof CarouselSchema>;

type SlideItem = {
  url: string;
  type: "video" | "image";
  caption?: string;
};

const Slide: React.FC<{
  item: SlideItem;
  progress: number;
  transition: string;
  entering: boolean;
  width: number;
  height: number;
}> = ({ item, progress, transition, entering, width, height }) => {
  let transform = "";
  let opacity = 1;

  switch (transition) {
    case "fade":
      opacity = entering ? progress : 1 - progress;
      break;
    case "slide":
      const slideX = entering
        ? interpolate(progress, [0, 1], [width, 0])
        : interpolate(progress, [0, 1], [0, -width]);
      transform = `translateX(${slideX}px)`;
      break;
    case "zoom":
      const scale = entering
        ? interpolate(progress, [0, 1], [1.2, 1])
        : interpolate(progress, [0, 1], [1, 0.8]);
      opacity = entering ? progress : 1 - progress;
      transform = `scale(${scale})`;
      break;
    case "swipe":
      const swipeX = entering
        ? interpolate(progress, [0, 1], [width, 0])
        : interpolate(progress, [0, 1], [0, -width]);
      const swipeScale = entering
        ? interpolate(progress, [0, 1], [0.9, 1])
        : interpolate(progress, [0, 1], [1, 0.9]);
      transform = `translateX(${swipeX}px) scale(${swipeScale})`;
      break;
  }

  return (
    <AbsoluteFill
      style={{
        transform,
        opacity,
      }}
    >
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
    </AbsoluteFill>
  );
};

const Indicators: React.FC<{
  total: number;
  current: number;
  color: string;
}> = ({ total, current, color }) => {
  return (
    <div
      style={{
        position: "absolute",
        bottom: 80,
        left: "50%",
        transform: "translateX(-50%)",
        display: "flex",
        gap: 12,
        zIndex: 100,
      }}
    >
      {Array.from({ length: total }, (_, i) => (
        <div
          key={i}
          style={{
            width: i === current ? 30 : 10,
            height: 10,
            borderRadius: 5,
            backgroundColor: color,
            opacity: i === current ? 1 : 0.4,
            transition: "all 0.3s ease",
          }}
        />
      ))}
    </div>
  );
};

const Caption: React.FC<{
  text: string;
  color: string;
  progress: number;
}> = ({ text, color, progress }) => {
  const opacity = interpolate(progress, [0, 0.3, 0.7, 1], [0, 1, 1, 0]);
  const translateY = interpolate(progress, [0, 0.3, 0.7, 1], [20, 0, 0, -20]);

  return (
    <div
      style={{
        position: "absolute",
        bottom: 150,
        left: 0,
        right: 0,
        padding: "0 60px",
        opacity,
        transform: `translateY(${translateY}px)`,
        zIndex: 100,
      }}
    >
      <p
        style={{
          color,
          fontSize: 42,
          fontFamily: "Inter, system-ui, sans-serif",
          fontWeight: 600,
          textAlign: "center",
          textShadow: "0 2px 10px rgba(0,0,0,0.5)",
          margin: 0,
        }}
      >
        {text}
      </p>
    </div>
  );
};

export const Carousel: React.FC<CarouselProps> = ({
  slides,
  transition,
  transitionDuration,
  slideInterval,
  backgroundColor,
  captionColor,
  showCaptions,
  showIndicators,
}) => {
  const frame = useCurrentFrame();
  const { width, height, durationInFrames } = useVideoConfig();

  // Calculate current slide and transition progress
  const totalSlideTime = slideInterval;
  const currentSlideIndex = Math.floor(frame / totalSlideTime) % slides.length;
  const nextSlideIndex = (currentSlideIndex + 1) % slides.length;

  // Time within current slide
  const frameInSlide = frame % totalSlideTime;

  // Check if we're in transition
  const isTransitioning = frameInSlide >= totalSlideTime - transitionDuration;

  // Transition progress (0 to 1)
  const transitionProgress = isTransitioning
    ? interpolate(
        frameInSlide,
        [totalSlideTime - transitionDuration, totalSlideTime],
        [0, 1],
        { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
      )
    : 0;

  // Slide progress for caption animation (0 to 1 over entire slide duration)
  const slideProgress = frameInSlide / totalSlideTime;

  // Fade out at the end of video
  const globalFade = interpolate(
    frame,
    [durationInFrames - 30, durationInFrames],
    [1, 0],
    { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
  );

  const currentSlide = slides[currentSlideIndex];
  const nextSlide = slides[nextSlideIndex];

  return (
    <AbsoluteFill style={{ backgroundColor, opacity: globalFade }}>
      {/* Current slide */}
      <Slide
        item={currentSlide}
        progress={isTransitioning ? transitionProgress : 0}
        transition={transition}
        entering={false}
        width={width}
        height={height}
      />

      {/* Next slide (during transition) */}
      {isTransitioning && (
        <Slide
          item={nextSlide}
          progress={transitionProgress}
          transition={transition}
          entering={true}
          width={width}
          height={height}
        />
      )}

      {/* Caption */}
      {showCaptions && currentSlide.caption && (
        <Caption
          text={currentSlide.caption}
          color={captionColor}
          progress={slideProgress}
        />
      )}

      {/* Slide indicators */}
      {showIndicators && slides.length > 1 && (
        <Indicators
          total={slides.length}
          current={currentSlideIndex}
          color={captionColor}
        />
      )}
    </AbsoluteFill>
  );
};
