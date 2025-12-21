import React from "react";
import { fetchCaptions } from "./lib/fetchCaptions";
import { getVideoMetadata } from "@remotion/media-utils";
import { Composition } from "remotion";
import { SplitTalkingHead, SplitTalkingHeadSchema } from "./compositions/SplitTalkingHead";
import { resolveMediaPath } from "./shared/mediaPath";

export const RemotionRoot: React.FC = () => {
  return (
    <>
      {/* SplitTalkingHead - Main template */}
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
          captionLanguage: "ru",
          captionColor: "#FFFF00",
          splitRatio: 0.5,
          backgroundMusic: "",
          musicVolume: 0.15,
          segments: [
            { type: "split" as const, startFrame: 0, durationFrames: 210, bRollUrl: "/backgrounds/business/00.mp4", bRollType: "video" as const, caption: "" },
            { type: "split" as const, startFrame: 210, durationFrames: 180, bRollUrl: "/backgrounds/business/01.mp4", bRollType: "video" as const, caption: "" },
            { type: "fullscreen" as const, startFrame: 390, durationFrames: 90, caption: "" },
            { type: "split" as const, startFrame: 480, durationFrames: 180, bRollUrl: "/backgrounds/business/02.mp4", bRollType: "video" as const, caption: "" },
            { type: "fullscreen" as const, startFrame: 660, durationFrames: 90, caption: "" },
            { type: "split" as const, startFrame: 750, durationFrames: 180, bRollUrl: "/backgrounds/business/03.mp4", bRollType: "video" as const, caption: "" },
            { type: "fullscreen" as const, startFrame: 930, durationFrames: 90, caption: "" },
          ],
          ctaText: "COMMENT PERSONA",
          ctaHighlight: "PERSONA",
          captions: [],
          showCaptions: true,
          captionStyle: {
            fontSize: 56,
            highlightColor: "#FFFF00",
          },
        }}
        calculateMetadata={async ({ props }) => {
          const fps = 30;
          let durationInFrames = 1020; // Default fallback

          // Get actual video duration
          try {
            const videoUrl = resolveMediaPath(props.lipSyncVideo);
            const metadata = await getVideoMetadata(videoUrl);
            durationInFrames = Math.ceil(metadata.durationInSeconds * fps);
            console.log(`📏 LipSync video duration: ${metadata.durationInSeconds.toFixed(2)}s = ${durationInFrames} frames`);
          } catch (e) {
            console.warn('Could not get video metadata, using default duration:', e);
          }

          // Load captions if not provided
          let captions = props.captions;
          if (!captions || captions.length === 0) {
            const language = (props as any).captionLanguage || "ru";
            captions = await fetchCaptions(props.lipSyncVideo, language);
          }

          return {
            durationInFrames,
            props: {
              ...props,
              captions,
            },
          };
        }}
      />
    </>
  );
};
