import { useRef, useEffect, useCallback, useState, useMemo } from 'react';
import { Player } from '@remotion/player';
import type { PlayerRef } from '@remotion/player';
import { useAtomValue, useSetAtom } from 'jotai';
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
} from '@/atoms';
import { SplitTalkingHead, type SplitTalkingHeadProps, type Segment } from '@compositions/SplitTalkingHead';
import { ZoomIn, ZoomOut, Maximize, Minimize, Loader2, Mic } from 'lucide-react';
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
  assets: Asset[]
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

  // Get audio track volume from track item (not templateProps)
  const audioTrackVolume = useMemo(() => {
    const audioTrack = tracks.find((t) => t.type === 'audio');
    const audioItem = audioTrack?.items[0];
    return (audioItem as any)?.volume ?? 0.06;
  }, [tracks]);

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
    () => convertToSplitTalkingHeadProps(lipSyncPropsWithUrls, project.durationInFrames, project.fps, videoTrackItems, assets),
    [lipSyncPropsWithUrls, project.durationInFrames, project.fps, videoTrackItems, assets]
  );

  // Apply mute/volume state to music and video
  // Use track item volumes (controlled by VolumePopup)
  const splitTalkingHeadProps = useMemo(
    () => {
      const props = {
        ...splitTalkingHeadPropsBase,
        // Background music - use audio track item volume
        musicVolume: isMuted ? 0 : audioTrackVolume,
        // LipSync video (avatar) volume - use avatar track item volume
        videoVolume: isMuted ? 0 : avatarTrackVolume,
      };
      return props;
    },
    [splitTalkingHeadPropsBase, isMuted, audioTrackVolume, avatarTrackVolume]
  );

  // Calculate zoom to fit height
  useEffect(() => {
    const container = containerRef.current;
    if (!container) return;

    const updateZoom = () => {
      const containerHeight = container.clientHeight - 60; // padding for controls
      const videoHeight = project.height;
      const fitZoom = containerHeight / videoHeight;
      setAutoZoom(Math.min(fitZoom, 1)); // Don't zoom more than 100%
    };

    updateZoom();
    const observer = new ResizeObserver(updateZoom);
    observer.observe(container);

    return () => observer.disconnect();
  }, [project.height]);

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

  const handleZoomIn = useCallback((e: React.MouseEvent) => {
    e.stopPropagation();
    setCanvasZoom(Math.min(effectiveZoom + 0.05, 1));
  }, [effectiveZoom, setCanvasZoom]);

  const handleZoomOut = useCallback((e: React.MouseEvent) => {
    e.stopPropagation();
    setCanvasZoom(Math.max(effectiveZoom - 0.05, 0.1));
  }, [effectiveZoom, setCanvasZoom]);

  const handleFitToHeight = useCallback(() => {
    setCanvasZoom(autoZoom);
  }, [autoZoom, setCanvasZoom]);

  // Fullscreen toggle
  const handleFullscreen = useCallback((e: React.MouseEvent) => {
    e.stopPropagation();
    const container = containerRef.current;
    if (!container) return;

    if (!document.fullscreenElement) {
      container.requestFullscreen().catch((err) => {
        console.error('Fullscreen error:', err);
        alert('Fullscreen not supported or blocked');
      });
    } else {
      document.exitFullscreen();
    }
  }, []);

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
      {/* Zoom Controls */}
      <div className="canvas-controls" onClick={(e) => e.stopPropagation()}>
        <button type="button" className="canvas-control-btn" onClick={handleZoomOut} title="Zoom out">
          <ZoomOut size={16} />
        </button>
        <span className="canvas-zoom-label">{Math.round(effectiveZoom * 100)}%</span>
        <button type="button" className="canvas-control-btn" onClick={handleZoomIn} title="Zoom in">
          <ZoomIn size={16} />
        </button>
        <button type="button" className="canvas-control-btn" onClick={handleFullscreen} title={isFullscreen ? "Exit fullscreen" : "Fullscreen"}>
          {isFullscreen ? <Minimize size={16} /> : <Maximize size={16} />}
        </button>
      </div>

      {/* Transcribing/Loading Overlay */}
      {(isTranscribing || captionsLoading) && (
        <div className="canvas-transcribing-overlay">
          <div className="transcribing-indicator">
            <Mic size={24} className="transcribing-icon" />
            <Loader2 size={20} className="transcribing-spinner" />
            <span>{isTranscribing ? 'Transcribing audio...' : 'Loading captions...'}</span>
          </div>
        </div>
      )}

      {/* Player Wrapper */}
      <div
        className="canvas-player-wrapper"
        style={{
          transform: `translate(-50%, -50%) scale(${isFullscreen ? fullscreenZoom : effectiveZoom})`,
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
    </div>
  );
}
