import { useRef, useEffect, useCallback, useState, useMemo } from 'react';
import { Player } from '@remotion/player';
import type { PlayerRef } from '@remotion/player';
import { useAtomValue, useSetAtom } from 'jotai';
import { useLanguage } from '@/hooks/useLanguage';
import {
  projectAtom,
  currentFrameAtom,
  isPlayingAtom,
  isMutedAtom,
  playbackRateAtom,
  canvasZoomAtom,
  tracksAtom,
  assetsAtom,
  playerRefAtom,
  clearSelectionAtom,
  templatePropsAtom,
  transcribingAtom,
  captionsLoadingAtom,
  avatarSettingsTabAtom,
  currentRemixSourceAtom,
} from '@/atoms';
import { SplitTalkingHead, type SplitTalkingHeadProps, type Segment } from '@compositions/SplitTalkingHead';
import { Loader2, Mic } from 'lucide-react';
import { convertPropsToAbsoluteUrls, toAbsoluteUrl } from '@/lib/mediaUrl';
import type { LipSyncMainProps, TrackItem, Asset } from '@/store/types';
import './InteractiveCanvas.css';

/**
 * Convert LipSyncMainProps to SplitTalkingHeadProps
 * Uses actual timeline positions from video track items
 */
function convertToSplitTalkingHeadProps(
  props: LipSyncMainProps,
  durationInFrames: number,
  fps: number,
  videoTrackItems: TrackItem[],
  assets: Asset[],
  avatarSettingsTab: 'split' | 'fullscreen'
): SplitTalkingHeadProps {
  const segments: Segment[] = [];

  // Sort items by startFrame to ensure correct order
  const sortedItems = [...videoTrackItems].sort((a, b) => a.startFrame - b.startFrame);

  if (sortedItems.length === 0) {
    // No B-roll - just fullscreen lipsync
    segments.push({
      type: 'fullscreen',
      startFrame: 0,
      durationFrames: durationInFrames,
      caption: '',
    });
  } else {
    // Use actual timeline positions from video track items
    let lastEndFrame = 0;

    sortedItems.forEach((item) => {
      // Add fullscreen segment for gap before this b-roll (if any)
      if (item.startFrame > lastEndFrame) {
        segments.push({
          type: 'fullscreen',
          startFrame: lastEndFrame,
          durationFrames: item.startFrame - lastEndFrame,
          caption: '',
        });
      }

      // Get URL from assets using assetId
      const asset = item.assetId ? assets.find((a) => a.id === item.assetId) : null;
      const bRollUrl = asset?.url ? toAbsoluteUrl(asset.url) : undefined;

      // Add split segment with B-roll at timeline position
      segments.push({
        type: 'split',
        startFrame: item.startFrame,
        durationFrames: item.durationInFrames,
        bRollUrl: bRollUrl,
        bRollType: 'video',
        caption: '',
        layout: (item as any).layout || 'top-half',
      });

      lastEndFrame = item.startFrame + item.durationInFrames;
    });

    // Add final fullscreen segment if there's remaining time
    if (lastEndFrame < durationInFrames) {
      segments.push({
        type: 'fullscreen',
        startFrame: lastEndFrame,
        durationFrames: durationInFrames - lastEndFrame,
        caption: '',
      });
    }
  }

  console.log('[convertToSplitTalkingHeadProps] backgroundMusic:', props.backgroundMusic, 'musicVolume:', props.musicVolume);

  return {
    lipSyncVideo: props.lipSyncVideo,
    segments,
    captionColor: props.captionStyle?.highlightColor || '#FFFF00',
    splitRatio: 0.5,
    backgroundMusic: props.backgroundMusic,
    musicVolume: props.musicVolume,
    captions: props.captions || [],
    showCaptions: props.showCaptions ?? true,
    captionStyle: props.captionStyle || {},
    // Face centering
    faceOffsetX: props.faceOffsetX ?? 0,
    faceOffsetY: props.faceOffsetY ?? 0,
    faceScale: props.faceScale ?? 1,
    // Video volume (default 1, applied in splitTalkingHeadProps override)
    videoVolume: 1,
    // Circle/Avatar positioning - MUST pass these for agent actions to work!
    circleSizePercent: props.circleSizePercent,
    circleBottomPercent: props.circleBottomPercent,
    circleLeftPercent: props.circleLeftPercent,
    // Circle mode
    isCircleAvatar: props.isCircleAvatar ?? false,
    avatarBorderRadius: props.avatarBorderRadius ?? 50,
    // Split mode settings
    splitCircleSize: props.splitCircleSize ?? 25,
    splitPositionX: props.splitPositionX ?? 0,
    splitPositionY: props.splitPositionY ?? 0,
    splitFaceScale: props.splitFaceScale ?? 1,
    splitIsCircle: props.splitIsCircle ?? true,
    splitBorderRadius: props.splitBorderRadius ?? 50,
    // Fullscreen mode settings
    fullscreenCircleSize: props.fullscreenCircleSize ?? 50,
    fullscreenPositionX: props.fullscreenPositionX ?? 0,
    fullscreenPositionY: props.fullscreenPositionY ?? 0,
    fullscreenFaceScale: props.fullscreenFaceScale ?? 1,
    fullscreenIsCircle: props.fullscreenIsCircle ?? false,
    fullscreenBorderRadius: props.fullscreenBorderRadius ?? 50,
    // Visual effects
    vignetteStrength: props.vignetteStrength,
    colorCorrection: props.colorCorrection,
    // Avatar settings mode (from UI toggle)
    avatarSettingsTab,
    // Avatar animation
    avatarAnimation: props.avatarAnimation ?? 'pop',
    // Avatar border effects
    avatarBorderEffect: props.avatarBorderEffect ?? 'none',
    avatarBorderColor: props.avatarBorderColor ?? '#f59e0b',
    avatarBorderColor2: props.avatarBorderColor2 ?? '#fbbf24',
    avatarBorderWidth: props.avatarBorderWidth ?? 4,
    avatarBorderIntensity: props.avatarBorderIntensity ?? 0.8,
  };
}

export function InteractiveCanvas() {
  const playerRef = useRef<PlayerRef>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const [autoZoom, setAutoZoom] = useState(0.3);
  const [isFullscreen, setIsFullscreen] = useState(false);
  const [fullscreenZoom, setFullscreenZoom] = useState(1);

  // Jotai atoms - прямое использование
  const project = useAtomValue(projectAtom);
  const currentFrame = useAtomValue(currentFrameAtom);
  const isPlaying = useAtomValue(isPlayingAtom);
  const isMuted = useAtomValue(isMutedAtom);
  const playbackRate = useAtomValue(playbackRateAtom);
  const canvasZoom = useAtomValue(canvasZoomAtom);
  const tracks = useAtomValue(tracksAtom);
  const assets = useAtomValue(assetsAtom);
  const isTranscribing = useAtomValue(transcribingAtom);
  const captionsLoading = useAtomValue(captionsLoadingAtom);
  const avatarSettingsTab = useAtomValue(avatarSettingsTabAtom);
  const remixSource = useAtomValue(currentRemixSourceAtom);

  const { t } = useLanguage();

  const setCurrentFrame = useSetAtom(currentFrameAtom);
  const setIsPlaying = useSetAtom(isPlayingAtom);
  const setCanvasZoom = useSetAtom(canvasZoomAtom);
  const clearSelection = useSetAtom(clearSelectionAtom);
  const setPlayerRefAtom = useSetAtom(playerRefAtom);

  // Store player ref for direct control (needed for autoplay policy)
  useEffect(() => {
    setPlayerRefAtom(playerRef);
  }, [setPlayerRefAtom]);

  // Get video track items for timeline position sync
  const videoTrackItems = useMemo(() => {
    const videoTrack = tracks.find((t) => t.type === 'video');
    return videoTrack?.items || [];
  }, [tracks]);

  // Get audio track volume and URL from track items
  const { audioTrackVolume, audioTrackUrl } = useMemo(() => {
    const audioTrack = tracks.find((t) => t.type === 'audio');
    if (!audioTrack || audioTrack.items.length === 0) {
      return { audioTrackVolume: 0.06, audioTrackUrl: null };
    }

    // Use the last added audio item (most recent)
    const lastAudioItem = audioTrack.items[audioTrack.items.length - 1];
    const volume = (lastAudioItem as any)?.volume ?? 0.06;

    // Get URL from asset
    const asset = assets.find(a => a.id === lastAudioItem.assetId);
    const url = asset?.url || null;

    return { audioTrackVolume: volume, audioTrackUrl: url };
  }, [tracks, assets]);

  // Get avatar track volume from track item
  const avatarTrackVolume = useMemo(() => {
    const avatarTrack = tracks.find((t) => t.type === 'avatar');
    const avatarItem = avatarTrack?.items[0];
    return (avatarItem as any)?.volume ?? 1;
  }, [tracks]);

  // Get computed props for LipSyncMain (base props from store)
  const lipSyncPropsRaw = useAtomValue(templatePropsAtom);

  // Convert relative paths to absolute URLs
  const lipSyncPropsWithUrls = useMemo(
    () => convertPropsToAbsoluteUrls(lipSyncPropsRaw),
    [lipSyncPropsRaw]
  );

  // Convert to SplitTalkingHead props - using actual timeline positions
  const splitTalkingHeadPropsBase = useMemo(
    () => convertToSplitTalkingHeadProps(lipSyncPropsWithUrls, project.durationInFrames, project.fps, videoTrackItems, assets, avatarSettingsTab),
    [lipSyncPropsWithUrls, project.durationInFrames, project.fps, videoTrackItems, assets, avatarSettingsTab]
  );

  // Apply mute/volume state to music and video
  // Use track item volumes (controlled by VolumePopup)
  // Override backgroundMusic if audio was added via timeline
  const splitTalkingHeadProps = useMemo(
    () => {
      const props = {
        ...splitTalkingHeadPropsBase,
        // Background music - use audio from track if available, otherwise use default
        backgroundMusic: audioTrackUrl ? toAbsoluteUrl(audioTrackUrl) : splitTalkingHeadPropsBase.backgroundMusic,
        // Background music volume - use audio track item volume
        musicVolume: isMuted ? 0 : audioTrackVolume,
        // LipSync video (avatar) volume - use avatar track item volume
        videoVolume: isMuted ? 0 : avatarTrackVolume,
      };
      return props;
    },
    [splitTalkingHeadPropsBase, isMuted, audioTrackVolume, avatarTrackVolume, audioTrackUrl]
  );

  // Calculate zoom to fit height (allow scaling up for vertical videos)
  useEffect(() => {
    const container = containerRef.current;
    if (!container) return;

    const updateZoom = () => {
      const containerHeight = container.clientHeight - 40; // small padding
      const containerWidth = container.clientWidth - 40;
      const videoHeight = project.height;
      const videoWidth = project.width;

      // Calculate zoom to fit both dimensions
      const fitHeightZoom = containerHeight / videoHeight;
      const fitWidthZoom = containerWidth / videoWidth;

      // Use the smaller zoom to ensure video fits in both dimensions
      const fitZoom = Math.min(fitHeightZoom, fitWidthZoom);
      setAutoZoom(fitZoom); // Allow scaling up for small/vertical videos
    };

    updateZoom();
    const observer = new ResizeObserver(updateZoom);
    observer.observe(container);

    return () => observer.disconnect();
  }, [project.height, project.width]);

  // Sync player with store - frame updates
  useEffect(() => {
    const player = playerRef.current;
    if (!player) return;

    const handleFrameUpdate = (e: { detail: { frame: number } }) => {
      setCurrentFrame(e.detail.frame);
    };

    player.addEventListener('frameupdate', handleFrameUpdate);
    return () => {
      player.removeEventListener('frameupdate', handleFrameUpdate);
    };
  }, [setCurrentFrame]);

  // Control playback
  useEffect(() => {
    const player = playerRef.current;
    if (!player) return;

    if (isPlaying) {
      player.play();
    } else {
      player.pause();
    }
  }, [isPlaying]);

  // Sync player playback state
  useEffect(() => {
    const player = playerRef.current;
    if (!player) return;

    const handlePlay = () => setIsPlaying(true);
    const handlePause = () => setIsPlaying(false);

    player.addEventListener('play', handlePlay);
    player.addEventListener('pause', handlePause);

    return () => {
      player.removeEventListener('play', handlePlay);
      player.removeEventListener('pause', handlePause);
    };
  }, [setIsPlaying]);

  // Seek to frame when currentFrame changes externally
  useEffect(() => {
    const player = playerRef.current;
    if (!player || isPlaying) return;

    player.seekTo(currentFrame);
  }, [currentFrame, isPlaying]);

  const handleCanvasClick = useCallback((e: React.MouseEvent) => {
    // Click on empty canvas = clear selection
    if (e.target === e.currentTarget) {
      clearSelection();
    }
  }, [clearSelection]);

  // Use autoZoom if canvasZoom hasn't been manually set
  const effectiveZoom = canvasZoom || autoZoom;

  // Listen for fullscreen changes and calculate zoom
  useEffect(() => {
    const handleFullscreenChange = () => {
      const isFs = !!document.fullscreenElement;
      setIsFullscreen(isFs);

      if (isFs) {
        // Calculate zoom to fit video height to screen height
        const screenHeight = window.innerHeight;
        const videoHeight = project.height;
        const zoom = (screenHeight - 40) / videoHeight; // 40px padding
        setFullscreenZoom(Math.min(zoom, 1)); // Don't zoom more than 100%
      }
    };

    document.addEventListener('fullscreenchange', handleFullscreenChange);
    return () => document.removeEventListener('fullscreenchange', handleFullscreenChange);
  }, [project.height]);

  return (
    <div className="canvas-container" onClick={handleCanvasClick} ref={containerRef}>
      {/* Transcribing/Loading Overlay */}
      {(isTranscribing || captionsLoading) && (
        <div className="canvas-transcribing-overlay">
          <div className="transcribing-indicator">
            <Mic size={24} className="transcribing-icon" />
            <Loader2 size={20} className="transcribing-spinner" />
            <span>{isTranscribing ? t('canvas.transcribingAudio') : t('canvas.loadingCaptions')}</span>
          </div>
        </div>
      )}

      {/* Player Wrapper */}
      <div
        className="canvas-player-wrapper"
        style={{
          transform: isFullscreen
            ? `translate(-50%, -50%) scale(${fullscreenZoom})`
            : `scale(${effectiveZoom})`,
        }}
      >
        <Player
          ref={playerRef}
          component={SplitTalkingHead as unknown as React.ComponentType<Record<string, unknown>>}
          inputProps={splitTalkingHeadProps as unknown as Record<string, unknown>}
          durationInFrames={project.durationInFrames}
          fps={project.fps}
          compositionWidth={project.width}
          compositionHeight={project.height}
          style={{
            width: project.width,
            height: project.height,
          }}
          controls={true}
          showVolumeControls={true}
          loop
          clickToPlay={true}
          playbackRate={playbackRate}
          numberOfSharedAudioTags={4}
        />
      </div>

      {/* Template/Reel Name Overlay */}
      {remixSource && (
        <div className="canvas-template-info">
          <span className="template-name">{remixSource.templateName}</span>
          <span className="template-creator">by {remixSource.creatorName}</span>
        </div>
      )}

    </div>
  );
}
