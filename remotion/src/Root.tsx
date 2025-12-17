import React from "react";
import { Composition } from "remotion";
import { TextOverlay, TextOverlaySchema } from "./compositions/TextOverlay";
import { VideoIntro, VideoIntroSchema } from "./compositions/VideoIntro";
import { DynamicVideo, DynamicVideoSchema } from "./compositions/DynamicVideo";
import { LipSyncMain } from "./compositions/LipSyncMain";
import { SplitScreen, SplitScreenSchema } from "./compositions/SplitScreen";
import { Carousel, CarouselSchema } from "./compositions/Carousel";
import { KenBurns, KenBurnsSchema } from "./compositions/KenBurns";
import { Subtitles, SubtitlesSchema } from "./compositions/Subtitles";
import { FactoryTalkingHead, FactoryTalkingHeadSchema } from "./factory/TemplateFactory";
import { SplitTalkingHead, SplitTalkingHeadSchema } from "./compositions/SplitTalkingHead";

export const RemotionRoot: React.FC = () => {
  return (
    <>
      {/* Text overlay composition - simple text animation */}
      <Composition
        id="TextOverlay"
        component={TextOverlay}
        durationInFrames={150}
        fps={30}
        width={1080}
        height={1920}
        schema={TextOverlaySchema}
        defaultProps={{
          title: "Welcome to VIBEE",
          subtitle: "AI-powered video generation",
          backgroundColor: "#1a1a2e",
          textColor: "#ffffff",
          accentColor: "#e94560",
        }}
      />

      {/* Video intro composition - branded intro animation */}
      <Composition
        id="VideoIntro"
        component={VideoIntro}
        durationInFrames={180}
        fps={30}
        width={1920}
        height={1080}
        schema={VideoIntroSchema}
        defaultProps={{
          brandName: "VIBEE",
          tagline: "Your AI Assistant",
          logoUrl: "",
          primaryColor: "#6c5ce7",
          secondaryColor: "#00cec9",
        }}
      />

      {/* Dynamic video - data-driven content */}
      <Composition
        id="DynamicVideo"
        component={DynamicVideo}
        durationInFrames={300}
        fps={30}
        width={1080}
        height={1920}
        schema={DynamicVideoSchema}
        defaultProps={{
          userName: "User",
          message: "Hello from VIBEE!",
          avatarUrl: "",
          items: [],
          theme: "dark",
        }}
      />

      {/* LipSync Main - Instagram Reels template with avatar morphing */}
      <Composition
        id="LipSyncMain"
        component={LipSyncMain as unknown as React.FC<Record<string, unknown>>}
        durationInFrames={900}
        fps={30}
        width={1080}
        height={1920}
        defaultProps={{
          lipSyncVideo: "/lip-sync.mp4",
          coverImage: "/covers/spiritual-cover.jpg",
          backgroundMusic: "/music/educational-ambient.mp3",
          musicVolume: 0.3,
          backgroundVideos: [
            "/backgrounds/spiritual/01.mp4",
            "/backgrounds/spiritual/02.mp4",
            "/backgrounds/spiritual/03.mp4",
            "/backgrounds/spiritual/04.mp4",
          ],
          coverDuration: 0.5,
          vignetteStrength: 0.7,
          colorCorrection: 1.2,
          circleSizePercent: 25.2,
          circleBottomPercent: 15,
          circleLeftPx: 40,
        }}
      />

      {/* LipSync Business - Business theme with lipsync avatar */}
      <Composition
        id="LipSyncBusiness"
        component={LipSyncMain as unknown as React.FC<Record<string, unknown>>}
        durationInFrames={900}
        fps={30}
        width={1080}
        height={1920}
        defaultProps={{
          lipSyncVideo: "/lipsync/lipsync.mp4",
          coverImage: "/lipsync/lipsync.mp4",
          backgroundMusic: "",
          musicVolume: 0,
          backgroundVideos: [
            "/backgrounds/business/00.mp4",
            "/backgrounds/business/01.mp4",
            "/backgrounds/business/02.mp4",
            "/backgrounds/business/03.mp4",
            "/backgrounds/business/04.mp4",
          ],
          coverDuration: 0,
          vignetteStrength: 0.6,
          colorCorrection: 1.15,
          circleSizePercent: 25.2,
          circleBottomPercent: 15,
          circleLeftPx: 40,
        }}
      />

      {/* SplitScreen - 2-4 media in grid layout */}
      <Composition
        id="SplitScreen"
        component={SplitScreen}
        durationInFrames={300}
        fps={30}
        width={1080}
        height={1920}
        schema={SplitScreenSchema}
        defaultProps={{
          media: [
            { url: "/video1.mp4", type: "video" as const },
            { url: "/video2.mp4", type: "video" as const },
            { url: "/image1.jpg", type: "image" as const },
            { url: "/image2.jpg", type: "image" as const },
          ],
          layout: "2x2" as const,
          gap: 10,
          borderRadius: 20,
          animationType: "scale" as const,
          backgroundColor: "#000000",
        }}
      />

      {/* Carousel - Slideshow with transitions */}
      <Composition
        id="Carousel"
        component={Carousel}
        durationInFrames={450}
        fps={30}
        width={1080}
        height={1920}
        schema={CarouselSchema}
        defaultProps={{
          slides: [
            { url: "/slide1.jpg", type: "image" as const, caption: "First Slide" },
            { url: "/slide2.jpg", type: "image" as const, caption: "Second Slide" },
            { url: "/slide3.mp4", type: "video" as const, caption: "Third Slide" },
          ],
          transition: "fade" as const,
          transitionDuration: 15,
          slideInterval: 90,
          backgroundColor: "#000000",
          captionColor: "#ffffff",
          showCaptions: true,
          showIndicators: true,
        }}
      />

      {/* KenBurns - Cinematic pan/zoom effects for photos */}
      <Composition
        id="KenBurns"
        component={KenBurns}
        durationInFrames={480}
        fps={30}
        width={1080}
        height={1920}
        schema={KenBurnsSchema}
        defaultProps={{
          images: [
            { url: "/photo1.jpg", effect: "zoomIn" as const },
            { url: "/photo2.jpg", effect: "panLeft" as const },
            { url: "/photo3.jpg", effect: "zoomOut" as const },
            { url: "/photo4.jpg", effect: "panRight" as const },
          ],
          imageDuration: 120,
          transitionDuration: 30,
          backgroundColor: "#000000",
          vignette: true,
          overlayOpacity: 0,
        }}
      />

      {/* Subtitles - Animated subtitles with various styles */}
      <Composition
        id="Subtitles"
        component={Subtitles}
        durationInFrames={300}
        fps={30}
        width={1080}
        height={1920}
        schema={SubtitlesSchema}
        defaultProps={{
          backgroundType: "color" as const,
          backgroundColor: "#1a1a2e",
          subtitles: [
            { text: "Welcome to VIBEE", startFrame: 0, endFrame: 60 },
            { text: "Create amazing videos with AI", startFrame: 70, endFrame: 140, highlight: "AI" },
            { text: "Fast, beautiful, professional", startFrame: 150, endFrame: 220 },
            { text: "Start creating today!", startFrame: 230, endFrame: 290 },
          ],
          style: "modern" as const,
          fontSize: 48,
          fontColor: "#ffffff",
          highlightColor: "#FFD700",
          backgroundColor2: "rgba(0,0,0,0.7)",
          position: "bottom" as const,
          maxWidth: 900,
        }}
      />

      {/* Factory Talking Head - Template factory for variation generation */}
      <Composition
        id="FactoryTalkingHead"
        component={FactoryTalkingHead}
        durationInFrames={900}
        fps={30}
        width={1080}
        height={1920}
        schema={FactoryTalkingHeadSchema}
        defaultProps={{
          // Core media
          lipSyncVideo: "{{assets.placeholders.lipsync}}",
          backgroundVideos: [
            "{{assets.placeholders.broll_01}}",
            "{{assets.placeholders.broll_02}}",
            "{{assets.placeholders.broll_03}}",
            "{{assets.placeholders.broll_04}}",
          ],
          backgroundMusic: "",
          musicVolume: 0.3,
          coverDuration: 0,

          // Variation axes (defaults)
          avatarPosition: "circle-bottom-left" as const,
          hookStyle: "zoom-impact" as const,
          hookText: "Did you know?",
          hookDuration: 3,
          captionStyle: "karaoke" as const,
          bRollPattern: "hook-content-cta" as const,

          // Avatar config
          circleSizePercent: 25,
          circleBottomPercent: 15,
          circleLeftPx: 40,
          glassMorphism: true,

          // B-roll config
          bRollDuration: 4,
          bRollGapDuration: 1.5,

          // Effects
          vignetteStrength: 0.7,
          filmGrainOpacity: 0.05,
          colorCorrection: 1.2,

          // Captions
          captions: [
            { text: "This is an example caption", startFrame: 90, endFrame: 150 },
            { text: "With multiple segments", startFrame: 160, endFrame: 220, highlight: "multiple" },
            { text: "And highlighted words", startFrame: 230, endFrame: 290, highlight: "highlighted" },
          ],
          captionPosition: "bottom" as const,
          captionFontSize: 48,
          captionFontColor: "#ffffff",
          captionHighlightColor: "#FFD700",
        }}
      />

      {/* Split Talking Head - Based on reel_01.mp4 style */}
      <Composition
        id="SplitTalkingHead"
        component={SplitTalkingHead}
        durationInFrames={1020}
        fps={30}
        width={1080}
        height={1920}
        schema={SplitTalkingHeadSchema}
        defaultProps={{
          lipSyncVideo: "/lipsync/lipsync.mp4",
          captionColor: "#FFFF00",
          splitRatio: 0.5,
          backgroundMusic: "",
          musicVolume: 0.15,
          segments: [
            // Split segments with B-roll
            { type: "split" as const, startFrame: 0, durationFrames: 90, bRollUrl: "/backgrounds/business/00.mp4", bRollType: "video" as const, caption: "AT THE FIRST" },
            { type: "split" as const, startFrame: 90, durationFrames: 90, bRollUrl: "/backgrounds/business/01.mp4", bRollType: "video" as const, caption: "THAT GENERATES" },
            // Fullscreen segment
            { type: "fullscreen" as const, startFrame: 180, durationFrames: 60, caption: "ACROSS IMAGES" },
            // More split
            { type: "split" as const, startFrame: 240, durationFrames: 90, bRollUrl: "/backgrounds/business/02.mp4", bRollType: "video" as const, caption: "IDEOGRAM" },
            { type: "fullscreen" as const, startFrame: 330, durationFrames: 60, caption: "REMEMBERS" },
            { type: "split" as const, startFrame: 390, durationFrames: 90, bRollUrl: "/backgrounds/business/03.mp4", bRollType: "video" as const, caption: "STYLE LOOKS" },
            { type: "split" as const, startFrame: 480, durationFrames: 90, bRollUrl: "/backgrounds/business/04.mp4", bRollType: "video" as const, caption: "TO DROP THEM" },
            // CTA fullscreen
            { type: "fullscreen" as const, startFrame: 570, durationFrames: 450, caption: "COMMENT PERSONA" },
          ],
          ctaText: "COMMENT PERSONA",
          ctaHighlight: "PERSONA",
        }}
      />
    </>
  );
};
