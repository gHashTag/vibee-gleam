import { useRef, useEffect, useCallback, useState, useMemo } from 'react';
import { Player } from '@remotion/player';
import type { PlayerRef } from '@remotion/player';
import { useEditorStore, useLipSyncProps } from '@/store/editorStore';
import { LipSyncMain } from '@compositions/LipSyncMain';
import { ZoomIn, ZoomOut, Maximize } from 'lucide-react';
import { convertPropsToAbsoluteUrls } from '@/lib/mediaUrl';
import './InteractiveCanvas.css';

export function InteractiveCanvas() {
  const playerRef = useRef<PlayerRef>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const [autoZoom, setAutoZoom] = useState(0.3);

  const project = useEditorStore((s) => s.project);
  const currentFrame = useEditorStore((s) => s.currentFrame);
  const isPlaying = useEditorStore((s) => s.isPlaying);
  const playbackRate = useEditorStore((s) => s.playbackRate);
  const canvasZoom = useEditorStore((s) => s.canvasZoom);
  const setCurrentFrame = useEditorStore((s) => s.setCurrentFrame);
  const setIsPlaying = useEditorStore((s) => s.setIsPlaying);
  const setCanvasZoom = useEditorStore((s) => s.setCanvasZoom);
  const clearSelection = useEditorStore((s) => s.clearSelection);

  // Get computed props for LipSyncMain
  const lipSyncPropsRaw = useLipSyncProps();

  // Convert relative paths to absolute URLs for render server
  const lipSyncProps = useMemo(
    () => convertPropsToAbsoluteUrls(lipSyncPropsRaw),
    [lipSyncPropsRaw]
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

  const handleZoomIn = useCallback(() => {
    setCanvasZoom(Math.min(effectiveZoom + 0.05, 1));
  }, [effectiveZoom, setCanvasZoom]);

  const handleZoomOut = useCallback(() => {
    setCanvasZoom(Math.max(effectiveZoom - 0.05, 0.1));
  }, [effectiveZoom, setCanvasZoom]);

  const handleFitToHeight = useCallback(() => {
    setCanvasZoom(autoZoom);
  }, [autoZoom, setCanvasZoom]);

  return (
    <div className="canvas-container" onClick={handleCanvasClick} ref={containerRef}>
      {/* Zoom Controls */}
      <div className="canvas-controls">
        <button className="canvas-control-btn" onClick={handleZoomOut} title="Zoom out">
          <ZoomOut size={16} />
        </button>
        <span className="canvas-zoom-label">{Math.round(effectiveZoom * 100)}%</span>
        <button className="canvas-control-btn" onClick={handleZoomIn} title="Zoom in">
          <ZoomIn size={16} />
        </button>
        <button className="canvas-control-btn" onClick={handleFitToHeight} title="Fit to height">
          <Maximize size={16} />
        </button>
      </div>

      {/* Player Wrapper */}
      <div
        className="canvas-player-wrapper"
        style={{
          transform: `translate(-50%, -50%) scale(${effectiveZoom})`,
        }}
      >
        <Player
          ref={playerRef}
          component={LipSyncMain as unknown as React.ComponentType<Record<string, unknown>>}
          inputProps={lipSyncProps as unknown as Record<string, unknown>}
          durationInFrames={project.durationInFrames}
          fps={project.fps}
          compositionWidth={project.width}
          compositionHeight={project.height}
          style={{
            width: project.width,
            height: project.height,
          }}
          controls={false}
          loop
          clickToPlay={false}
          playbackRate={playbackRate}
        />
      </div>
    </div>
  );
}
