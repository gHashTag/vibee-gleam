import { useRef, useCallback, useEffect, useState } from 'react';
import { useAtomValue, useSetAtom } from 'jotai';
import { useLanguage } from '@/hooks/useLanguage';
import {
  projectAtom,
  tracksAtom,
  currentFrameAtom,
  isPlayingAtom,
  playbackRateAtom,
  timelineZoomAtom,
  snapSettingsAtom,
  inPointAtom,
  outPointAtom,
  markersAtom,
  isMutedAtom,
  volumeAtom,
  playerRefAtom,
  playAtom,
  pauseAtom,
  setSnapEnabledAtom,
  updateTrackAtom,
  canvasZoomAtom,
  // Export atoms
  assetsAtom,
  isExportingAtom,
  exportProgressAtom,
  setExportingAtom,
  templatePropsAtom,
  segmentsAtom,
  userAtom,
  showPaywallAtom,
  showLoginModalAtom,
  canRenderAtom,
  logRenderAtom,
  publishToFeedAtom,
  // Undo/Redo atoms
  undoAtom,
  redoAtom,
  canUndoAtom,
  canRedoAtom,
  // Reset atoms
  resetTracksAtom,
  updateTemplatePropAtom,
  // Template props atoms for reset
  musicVolumeAtom,
  vignetteStrengthAtom,
  colorCorrectionAtom,
  faceOffsetXAtom,
  faceOffsetYAtom,
  showCaptionsAtom,
  splitCircleSizeAtom,
  splitPositionXAtom,
  splitPositionYAtom,
  splitFaceScaleAtom,
  splitIsCircleAtom,
  splitBorderRadiusAtom,
  fullscreenCircleSizeAtom,
  fullscreenPositionXAtom,
  fullscreenPositionYAtom,
  fullscreenFaceScaleAtom,
  fullscreenIsCircleAtom,
  fullscreenBorderRadiusAtom,
  // Templates
  addTemplateAtom,
  sidebarTabAtom,
} from '@/atoms';
import { editorStore } from '@/atoms/Provider';
import { TimeRuler } from './TimeRuler';
import { Track } from './Track';
import { Playhead } from './Playhead';
import { VolumePopup } from './VolumePopup';
import { Play, Pause, SkipBack, SkipForward, ZoomIn, ZoomOut, ChevronDown, ChevronUp, Magnet, Lock, Unlock, Maximize2, Maximize, Minimize, Volume2, VolumeX, Download, Loader2, AlertTriangle, X, Undo2, Redo2, Save, Upload, RotateCcw } from 'lucide-react';
import { RENDER_SERVER_URL, toAbsoluteUrl } from '@/lib/mediaUrl';
import { DEFAULT_COMPOSITION_ID } from '@/shared/compositions';
import { logExport } from '@/lib/logger';
import './Timeline.css';

// Download file using fetch + blob to bypass CORS download restriction
async function downloadFile(url: string, filename: string): Promise<boolean> {
  try {
    console.log('[Download] Fetching file:', url);
    const response = await fetch(url);

    if (!response.ok) {
      throw new Error(`HTTP ${response.status}`);
    }

    const blob = await response.blob();
    const blobUrl = URL.createObjectURL(blob);

    const link = document.createElement('a');
    link.href = blobUrl;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);

    // Cleanup blob URL after short delay
    setTimeout(() => URL.revokeObjectURL(blobUrl), 100);

    console.log('[Download] Success!');
    return true;
  } catch (error) {
    console.error('[Download] Failed:', error);
    // Fallback: open in new tab
    window.open(url, '_blank');
    return false;
  }
}

export function Timeline() {
  const { t } = useLanguage();
  const timelineRef = useRef<HTMLDivElement>(null);
  const projectImportRef = useRef<HTMLInputElement>(null);

  // Jotai atoms - прямое использование
  const project = useAtomValue(projectAtom);
  const tracks = useAtomValue(tracksAtom);
  const currentFrame = useAtomValue(currentFrameAtom);
  const isPlaying = useAtomValue(isPlayingAtom);
  const playbackRate = useAtomValue(playbackRateAtom);
  const timelineZoom = useAtomValue(timelineZoomAtom);
  const snapSettings = useAtomValue(snapSettingsAtom);
  const inPoint = useAtomValue(inPointAtom);
  const outPoint = useAtomValue(outPointAtom);
  const markers = useAtomValue(markersAtom);
  const isMuted = useAtomValue(isMutedAtom);
  const volume = useAtomValue(volumeAtom);
  const playerRef = useAtomValue(playerRefAtom);

  // Export atoms
  const assets = useAtomValue(assetsAtom);
  const isExporting = useAtomValue(isExportingAtom);
  const exportProgress = useAtomValue(exportProgressAtom);
  const templateProps = useAtomValue(templatePropsAtom);
  const segments = useAtomValue(segmentsAtom);
  const user = useAtomValue(userAtom);
  const canRender = useAtomValue(canRenderAtom);

  // Undo/Redo atoms
  const canUndoValue = useAtomValue(canUndoAtom);
  const canRedoValue = useAtomValue(canRedoAtom);
  const undo = useSetAtom(undoAtom);
  const redo = useSetAtom(redoAtom);

  const setCurrentFrame = useSetAtom(currentFrameAtom);
  const playDirect = useSetAtom(playAtom);
  const pauseDirect = useSetAtom(pauseAtom);
  const setPlaybackRate = useSetAtom(playbackRateAtom);
  const setTimelineZoom = useSetAtom(timelineZoomAtom);
  const setSnapEnabled = useSetAtom(setSnapEnabledAtom);
  const updateTrack = useSetAtom(updateTrackAtom);
  const setIsMuted = useSetAtom(isMutedAtom);
  const setVolume = useSetAtom(volumeAtom);
  const canvasZoom = useAtomValue(canvasZoomAtom);
  const setCanvasZoom = useSetAtom(canvasZoomAtom);
  const setProject = useSetAtom(projectAtom);

  // Export actions
  const setExportingAction = useSetAtom(setExportingAtom);
  const setShowPaywall = useSetAtom(showPaywallAtom);
  const setShowLoginModal = useSetAtom(showLoginModalAtom);
  const logRender = useSetAtom(logRenderAtom);
  const publishToFeed = useSetAtom(publishToFeedAtom);

  // Reset atoms
  const resetTracks = useSetAtom(resetTracksAtom);
  const updateTemplateProp = useSetAtom(updateTemplatePropAtom);
  const setMusicVolume = useSetAtom(musicVolumeAtom);
  const setVignetteStrength = useSetAtom(vignetteStrengthAtom);
  const setColorCorrection = useSetAtom(colorCorrectionAtom);
  const setFaceOffsetX = useSetAtom(faceOffsetXAtom);
  const setFaceOffsetY = useSetAtom(faceOffsetYAtom);
  const setShowCaptions = useSetAtom(showCaptionsAtom);
  const setSplitSize = useSetAtom(splitCircleSizeAtom);
  const setSplitPosX = useSetAtom(splitPositionXAtom);
  const setSplitPosY = useSetAtom(splitPositionYAtom);
  const setSplitScale = useSetAtom(splitFaceScaleAtom);
  const setSplitIsCircle = useSetAtom(splitIsCircleAtom);
  const setSplitRadius = useSetAtom(splitBorderRadiusAtom);
  const setFullSize = useSetAtom(fullscreenCircleSizeAtom);
  const setFullPosX = useSetAtom(fullscreenPositionXAtom);
  const setFullPosY = useSetAtom(fullscreenPositionYAtom);
  const setFullScale = useSetAtom(fullscreenFaceScaleAtom);
  const setFullIsCircle = useSetAtom(fullscreenIsCircleAtom);
  const setFullRadius = useSetAtom(fullscreenBorderRadiusAtom);

  // Templates
  const addTemplate = useSetAtom(addTemplateAtom);
  const setSidebarTab = useSetAtom(sidebarTabAtom);

  // Local state for blob warnings
  const [showBlobWarning, setShowBlobWarning] = useState(false);
  const [showCriticalBlobError, setShowCriticalBlobError] = useState(false);
  const [blobAssets, setBlobAssets] = useState<{ critical: string[]; optional: string[] }>({ critical: [], optional: [] });

  // Local state for dialogs
  const [showResetConfirm, setShowResetConfirm] = useState(false);
  const [showSaveTemplateDialog, setShowSaveTemplateDialog] = useState(false);
  const [templateName, setTemplateName] = useState('');

  // Helper functions
  const canUndo = () => canUndoValue;
  const canRedo = () => canRedoValue;

  const resetToDefaults = () => {
    resetTracks({ fps: 30, durationInFrames: 825 });
    setMusicVolume(0.06);
    setVignetteStrength(0.7);
    setColorCorrection(1.2);
    setFaceOffsetX(0);
    setFaceOffsetY(0);
    setShowCaptions(true);
    setSplitSize(100);
    setSplitPosX(0);
    setSplitPosY(0);
    setSplitScale(1.0);
    setSplitIsCircle(false);
    setSplitRadius(50);
    setFullSize(100);
    setFullPosX(0);
    setFullPosY(0);
    setFullScale(1.0);
    setFullIsCircle(false);
    setFullRadius(50);
  };

  // Save as template
  const handleSaveClick = () => {
    setTemplateName(project.name);
    setShowSaveTemplateDialog(true);
  };

  const handleSaveTemplate = () => {
    if (!templateName.trim()) return;
    addTemplate({
      name: templateName.trim(),
      description: `Created from ${project.name}`,
      compositionId: 'SplitTalkingHead',
      defaultProps: { ...templateProps },
      assets: [...assets],
      tracks: JSON.parse(JSON.stringify(tracks)),
    });
    setShowSaveTemplateDialog(false);
    setTemplateName('');
    setSidebarTab('templates');
  };

  // Project Import
  const handleProjectImport = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = (event) => {
      try {
        const content = event.target?.result as string;
        const data = JSON.parse(content);
        if (!data.project || !data.tracks) {
          alert(t('editor.invalidFormat'));
          return;
        }
        if (data.project) editorStore.set(projectAtom, data.project);
        if (data.templateProps) {
          Object.entries(data.templateProps).forEach(([key, value]) => {
            editorStore.set(updateTemplatePropAtom, { key: key as any, value: value as any });
          });
        }
        alert(t('editor.importSuccess'));
      } catch (err) {
        console.error('[Project] Import failed:', err);
        alert(t('editor.importFailed'));
      }
    };
    reader.readAsText(file);
    e.target.value = '';
  };

  // Helper for setExporting
  const setExporting = (exporting: boolean, progress?: number) => setExportingAction({ exporting, progress });

  // Handle mute toggle - uses both store state and player API
  const handleMuteToggle = useCallback(() => {
    const newMuted = !isMuted;
    setIsMuted(newMuted);

    // Also control the Remotion Player's mute state
    if (playerRef?.current) {
      if (newMuted) {
        playerRef.current.mute?.();
      } else {
        playerRef.current.unmute?.();
        playerRef.current.setVolume?.(volume);
      }
    }
  }, [isMuted, setIsMuted, playerRef, volume]);

  // Handle volume change
  const handleVolumeChange = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    const newVolume = parseFloat(e.target.value);
    setVolume(newVolume);

    // Apply to Remotion Player
    if (playerRef?.current) {
      if (newVolume === 0) {
        playerRef.current.mute?.();
        setIsMuted(true);
      } else {
        playerRef.current.unmute?.();
        playerRef.current.setVolume?.(newVolume);
        if (isMuted) setIsMuted(false);
      }
    }
  }, [setVolume, playerRef, isMuted, setIsMuted]);

  // Audio warmup - required for browser autoplay policy
  const warmupAudio = useCallback(() => {
    try {
      const AudioContext = window.AudioContext || (window as any).webkitAudioContext;
      if (AudioContext) {
        const audioContext = new AudioContext();
        if (audioContext.state === 'suspended') {
          audioContext.resume();
        }
      }
    } catch (e) {
      console.warn('[Timeline] Audio warmup failed:', e);
    }
  }, []);

  // Handle play with audio warmup - uses playDirect for user gesture context
  // CRITICAL: Must pass event to playDirect for browser autoplay policy!
  // See: https://www.remotion.dev/docs/player/autoplay
  const handlePlay = useCallback((e: React.MouseEvent) => {
    warmupAudio();

    // Ensure player is unmuted on first play with correct volume
    if (playerRef?.current && !isMuted) {
      playerRef.current.unmute?.();
      playerRef.current.setVolume?.(volume);
    }

    if (isPlaying) {
      pauseDirect();
    } else {
      playDirect(e); // Pass event for audio to work!
    }
  }, [isPlaying, isMuted, volume, playDirect, pauseDirect, warmupAudio, playerRef]);

  const fps = project.fps;
  const duration = project.durationInFrames;

  // Pixels per frame (base = 2px, scaled by zoom)
  const pxPerFrame = 2 * timelineZoom;

  // Auto-scroll to follow playhead during playback
  useEffect(() => {
    if (!isPlaying || !timelineRef.current) return;

    const wrapper = timelineRef.current;
    const playheadX = currentFrame * pxPerFrame;
    const wrapperWidth = wrapper.clientWidth;
    const scrollLeft = wrapper.scrollLeft;
    const scrollRight = scrollLeft + wrapperWidth;

    // Add some padding so playhead stays visible
    const padding = wrapperWidth * 0.2; // 20% padding

    // If playhead is near the right edge, scroll to keep it visible
    if (playheadX > scrollRight - padding) {
      wrapper.scrollTo({
        left: playheadX - wrapperWidth * 0.3, // Keep playhead at ~30% from left
        behavior: 'smooth',
      });
    }
    // If playhead is near the left edge (e.g., after seeking), scroll back
    else if (playheadX < scrollLeft + padding) {
      wrapper.scrollTo({
        left: Math.max(0, playheadX - padding),
        behavior: 'smooth',
      });
    }
  }, [currentFrame, pxPerFrame, isPlaying]);

  // Format frame as timecode
  const formatTime = useCallback(
    (frame: number) => {
      const totalSeconds = frame / fps;
      const minutes = Math.floor(totalSeconds / 60);
      const seconds = Math.floor(totalSeconds % 60);
      const frames = Math.floor(frame % fps);
      return `${minutes.toString().padStart(2, '0')}:${seconds
        .toString()
        .padStart(2, '0')}:${frames.toString().padStart(2, '0')}`;
    },
    [fps]
  );

  const handleTimelineClick = (e: React.MouseEvent<HTMLDivElement>) => {
    const rect = e.currentTarget.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const frame = Math.round(x / pxPerFrame);
    setCurrentFrame(Math.max(0, Math.min(frame, duration - 1)));
  };

  const handleSkipBack = () => {
    setCurrentFrame(0);
  };

  const handleSkipForward = () => {
    setCurrentFrame(duration - 1);
  };

  const handleZoomIn = () => {
    setTimelineZoom(Math.min(timelineZoom + 0.5, 5));
  };

  const handleZoomOut = () => {
    setTimelineZoom(Math.max(timelineZoom - 0.5, 0.25));
  };

  const handleFitToView = () => {
    if (!timelineRef.current) return;
    const viewportWidth = timelineRef.current.clientWidth - 20; // Account for padding
    const optimalZoom = viewportWidth / (duration * 2); // 2px per frame base
    setTimelineZoom(Math.max(0.25, Math.min(optimalZoom, 5)));
  };

  // Playback rate presets
  const speedPresets = [0.25, 0.5, 0.75, 1, 1.25, 1.5, 2];
  const currentSpeedIndex = speedPresets.indexOf(playbackRate);

  const handleSpeedUp = () => {
    const nextIndex = Math.min(currentSpeedIndex + 1, speedPresets.length - 1);
    setPlaybackRate(speedPresets[nextIndex]);
  };

  const handleSpeedDown = () => {
    const prevIndex = Math.max(currentSpeedIndex - 1, 0);
    setPlaybackRate(speedPresets[prevIndex]);
  };

  // Canvas zoom handlers
  const effectiveCanvasZoom = canvasZoom || 0.3; // Default zoom
  const handleCanvasZoomIn = () => setCanvasZoom(Math.min(effectiveCanvasZoom + 0.05, 1));
  const handleCanvasZoomOut = () => setCanvasZoom(Math.max(effectiveCanvasZoom - 0.05, 0.1));

  // ===============================
  // EXPORT HANDLERS
  // ===============================

  // Check for blob URLs in assets
  const checkForBlobUrls = (): { critical: string[]; optional: string[] } => {
    const critical: string[] = [];
    const optional: string[] = [];

    // Check lipSyncVideo - CRITICAL (required for render with audio)
    if (templateProps.lipSyncVideo.startsWith('blob:')) {
      critical.push('Lipsync video');
    }

    // Check backgroundVideos - optional (will be skipped)
    templateProps.backgroundVideos.forEach((url) => {
      if (url.startsWith('blob:')) {
        const asset = assets.find((a) => a.url === url);
        optional.push(asset?.name || 'Unknown video');
      }
    });

    // Check coverImage - optional
    if (templateProps.coverImage.startsWith('blob:')) {
      optional.push('Cover image');
    }

    // Check backgroundMusic - optional
    if (templateProps.backgroundMusic?.startsWith('blob:')) {
      optional.push('Background music');
    }

    return { critical, optional };
  };

  const handleExportClick = () => {
    // Check if user is logged in
    if (!user) {
      setShowLoginModal(true);
      return;
    }

    // Check if user has quota
    if (!canRender) {
      setShowPaywall(true);
      return;
    }

    const blobs = checkForBlobUrls();
    setBlobAssets(blobs);

    // Critical blob URLs (lipSyncVideo) - block export completely
    if (blobs.critical.length > 0) {
      setShowCriticalBlobError(true);
      return;
    }

    // Optional blob URLs - show warning but allow export
    if (blobs.optional.length > 0) {
      setShowBlobWarning(true);
    } else {
      handleExport();
    }
  };

  const handleExport = async () => {
    setShowBlobWarning(false);
    console.log('[Export] Starting export...');

    if (isExporting) {
      console.log('[Export] Already exporting, skipping');
      return;
    }

    console.log('[Export] Setting exporting state to true');
    setExporting(true, 0);

    try {
      // Check render server health
      console.log('[Export] Checking render server health at:', `${RENDER_SERVER_URL}/health`);
      const healthRes = await fetch(`${RENDER_SERVER_URL}/health`);
      console.log('[Export] Health response status:', healthRes.status);

      if (!healthRes.ok) {
        throw new Error('Render server not available');
      }

      const healthData = await healthRes.json();
      console.log('[Export] Health data:', healthData);

      // Get segments from timeline with exact positions
      logExport('Segments from timeline', { count: segments.length, segments });

      // Convert segments to absolute URLs
      const segmentsWithAbsoluteUrls = segments.map((seg) => ({
        ...seg,
        bRollUrl: seg.bRollUrl && !seg.bRollUrl.startsWith('blob:')
          ? toAbsoluteUrl(seg.bRollUrl)
          : seg.bRollUrl,
      }));

      // Filter out segments with blob URLs (can't be accessed by render server)
      const validSegments = segmentsWithAbsoluteUrls.filter((seg) => {
        if (seg.type === 'split' && seg.bRollUrl?.startsWith('blob:')) {
          console.warn('[Export] Skipping segment with blob URL - upload files to server first');
          return false;
        }
        return true;
      });

      // Also keep backgroundVideos for backward compatibility
      const filteredBackgroundVideos = templateProps.backgroundVideos
        .filter(url => !url.startsWith('blob:'))
        .map(toAbsoluteUrl);

      if (filteredBackgroundVideos.length < templateProps.backgroundVideos.length) {
        console.warn('[Export] Skipped blob URLs from backgroundVideos - upload files to server first');
      }

      const renderProps = {
        ...templateProps,
        lipSyncVideo: toAbsoluteUrl(templateProps.lipSyncVideo),
        coverImage: toAbsoluteUrl(templateProps.coverImage),
        backgroundMusic: templateProps.backgroundMusic ? toAbsoluteUrl(templateProps.backgroundMusic) : undefined,
        backgroundVideos: filteredBackgroundVideos,
        // Add segments with timeline positions for render
        segments: validSegments,
        // Explicitly set video volume for LipSync audio
        videoVolume: 1,
      };

      logExport('Render props prepared', {
        segmentsCount: validSegments.length,
        hasMusic: !!templateProps.backgroundMusic,
        lipSyncVideo: renderProps.lipSyncVideo,
      });
      console.log('[Export] Render props:', renderProps);

      // Notify about render start
      if (user) {
        fetch(`${RENDER_SERVER_URL}/api/notify/render-start`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            telegram_id: user.id,
            username: user.username,
            first_name: user.first_name,
            project_name: project.name,
          }),
        }).catch(() => {}); // Don't block on notification failure
      }

      // Start render with converted template props
      console.log('[Export] Sending render request to:', `${RENDER_SERVER_URL}/render`);
      const renderRes = await fetch(`${RENDER_SERVER_URL}/render`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          type: 'video',
          compositionId: DEFAULT_COMPOSITION_ID,
          codec: 'h264',
          inputProps: renderProps,
          // User info for Telegram notification on completion
          userInfo: user ? {
            telegram_id: user.id,
            username: user.username,
            first_name: user.first_name,
            project_name: project.name,
          } : undefined,
        }),
      });

      console.log('[Export] Render response status:', renderRes.status);
      const result = await renderRes.json();
      console.log('[Export] Render result:', result);

      if (result.success && result.renderId) {
        console.log('[Export] Render started, subscribing to SSE for progress...');

        // Subscribe to SSE for real-time progress
        const sseUrl = `${RENDER_SERVER_URL}/render/${result.renderId}/status`;
        console.log('[Export] SSE URL:', sseUrl);

        const eventSource = new EventSource(sseUrl);

        eventSource.onmessage = (event) => {
          try {
            const data = JSON.parse(event.data);
            console.log('[Export] SSE update:', data);

            // Update progress
            setExporting(true, data.progress || 0);

            // Check if completed
            if (data.status === 'completed' && data.outputUrl) {
              console.log('[Export] Render completed!');
              eventSource.close();

              // Log render to quota system
              logRender();

              // Download the file
              const downloadUrl = data.outputUrl.startsWith('http')
                ? data.outputUrl
                : `${RENDER_SERVER_URL}${data.outputUrl}`;

              console.log('[Export] Downloading from:', downloadUrl);

              // Auto-publish to community feed
              if (user) {
                publishToFeed({
                  name: project.name,
                  description: `Created by ${user.first_name || user.username || 'Anonymous'}`,
                  videoUrl: downloadUrl,
                  templateSettings: templateProps,
                }).then(() => {
                  console.log('[Feed] Auto-published to community feed');
                }).catch((err: Error) => {
                  console.error('[Feed] Auto-publish failed:', err);
                  // Show error to user - publish failed but download continues
                  alert(`Failed to publish to Feed: ${err.message || 'Unknown error'}`);
                });
              } else {
                console.log('[Feed] Skipping auto-publish: user not authenticated');
              }

              // Download using fetch + blob to bypass CORS restriction
              downloadFile(downloadUrl, `${project.name}.mp4`).then(() => {
                setExporting(false, 0);
              });
            } else if (data.status === 'failed') {
              console.error('[Export] Render failed:', data.error);
              eventSource.close();
              alert(`${t('editor.exportFailed')}: ${data.error || t('editor.unknownError')}`);
              setExporting(false, 0);
            }
          } catch (e) {
            console.error('[Export] SSE parse error:', e);
          }
        };

        eventSource.onerror = (error) => {
          console.error('[Export] SSE error:', error);
          eventSource.close();
          // Don't show error if already completed
          if (isExporting) {
            alert(t('editor.connectionLost'));
            setExporting(false, 0);
          }
        };

      } else if (result.success && result.outputUrl) {
        // Legacy: immediate response with outputUrl (fallback)
        console.log('[Export] Legacy render response with immediate outputUrl');
        const downloadUrl = result.outputUrl.startsWith('http')
          ? result.outputUrl
          : `${RENDER_SERVER_URL}${result.outputUrl}`;

        console.log('[Export] Downloading from:', downloadUrl);
        await downloadFile(downloadUrl, `${project.name}.mp4`);
        setExporting(false, 0);
      } else {
        console.error('[Export] Render failed:', result);
        throw new Error(result.error || 'Export failed');
      }
    } catch (error) {
      console.error('[Export] Error:', error);
      alert(`${t('editor.exportFailed')}: ${error instanceof Error ? error.message : t('editor.unknownError')}`);
      setExporting(false, 0);
    }
  };

  return (
    <div className="timeline">
      {/* Transport Controls - 3-column layout: LEFT | CENTER | RIGHT */}
      <div className="timeline-transport">
        {/* LEFT: Editing tools */}
        <div className="transport-left">
          {/* Undo/Redo buttons */}
          <div className="transport-undo-redo">
            <button
              className="transport-btn"
              onClick={undo}
              disabled={!canUndo()}
              title={`${t('editor.undo')} (Cmd+Z)`}
            >
              <Undo2 size={16} />
            </button>
            <button
              className="transport-btn"
              onClick={redo}
              disabled={!canRedo()}
              title={`${t('editor.redo')} (Cmd+Shift+Z)`}
            >
              <Redo2 size={16} />
            </button>
          </div>

          <div className="transport-divider" />

          {/* Zoom controls */}
          <div className="transport-zoom">
            <button className="transport-btn" onClick={handleZoomOut} title={`${t('timeline.zoomOut')} (-)`}>
              <ZoomOut size={14} />
            </button>
            <span className="zoom-label">{Math.round(timelineZoom * 100)}%</span>
            <button className="transport-btn" onClick={handleZoomIn} title={`${t('timeline.zoomIn')} (+)`}>
              <ZoomIn size={14} />
            </button>
          </div>

          <button
            className={`transport-btn snap-btn ${snapSettings.enabled ? 'active' : ''}`}
            onClick={() => setSnapEnabled(!snapSettings.enabled)}
            title={`${t('timeline.snapToGrid')} (${snapSettings.enabled ? t('timeline.on') : t('timeline.off')})`}
          >
            <Magnet size={14} />
          </button>
        </div>

        {/* CENTER: Playback controls */}
        <div className="transport-center">
          <button className="transport-btn" onClick={handleSkipBack} title={t('timeline.skipToStart')}>
            <SkipBack size={16} />
          </button>
          <button
            className="transport-btn play-btn"
            onClickCapture={handlePlay}
            title={isPlaying ? `${t('timeline.pause')} (Space)` : `${t('timeline.play')} (Space)`}
          >
            {isPlaying ? <Pause size={24} /> : <Play size={24} />}
          </button>
          <button className="transport-btn" onClick={handleSkipForward} title={t('timeline.skipToEnd')}>
            <SkipForward size={16} />
          </button>
        </div>

        {/* RIGHT: Time, Volume, Speed, File ops, Export */}
        <div className="transport-right">
          <div className="transport-time">
            <span className="time-current">{formatTime(currentFrame)}</span>
            <span className="time-separator">/</span>
            <span className="time-duration">{formatTime(duration)}</span>
          </div>

          <div className="transport-divider" />

          <div className="volume-control">
            <button
              className={`transport-btn ${isMuted ? '' : 'active'}`}
              onClick={handleMuteToggle}
              title={isMuted ? t('timeline.unmute') : t('timeline.mute')}
            >
              {isMuted ? <VolumeX size={14} /> : <Volume2 size={14} />}
            </button>
            <input
              type="range"
              className="volume-slider"
              min="0"
              max="1"
              step="0.05"
              value={isMuted ? 0 : volume}
              onChange={handleVolumeChange}
              title={`${t('timeline.volume')}: ${Math.round(volume * 100)}%`}
            />
          </div>

          <div className="transport-speed">
            <button
              className="speed-btn"
              onClick={handleSpeedDown}
              disabled={currentSpeedIndex <= 0}
              title={t('timeline.slower')}
            >
              <ChevronDown size={12} />
            </button>
            <span className={`speed-value ${playbackRate !== 1 ? 'modified' : ''}`}>
              {playbackRate}x
            </span>
            <button
              className="speed-btn"
              onClick={handleSpeedUp}
              disabled={currentSpeedIndex >= speedPresets.length - 1}
              title={t('timeline.faster')}
            >
              <ChevronUp size={12} />
            </button>
          </div>

          <div className="transport-divider" />

          {/* Save/Load/Reset buttons */}
          <div className="transport-file-ops">
            <button
              className="transport-btn"
              onClick={handleSaveClick}
              title={t('editor.saveTemplate')}
            >
              <Save size={16} />
            </button>
            <input
              ref={projectImportRef}
              type="file"
              accept=".json,.vibee.json"
              onChange={handleProjectImport}
              style={{ display: 'none' }}
            />
            <button
              className="transport-btn"
              onClick={() => projectImportRef.current?.click()}
              title={t('editor.load')}
            >
              <Upload size={16} />
            </button>
            <button
              className="transport-btn"
              onClick={() => setShowResetConfirm(true)}
              title={t('editor.reset')}
            >
              <RotateCcw size={16} />
            </button>
          </div>

          {/* Export Button */}
          <button
            className={`export-btn-large ${isExporting ? 'exporting' : ''}`}
            onClick={handleExportClick}
            disabled={isExporting}
            title={isExporting ? `${t('editor.exporting')} ${exportProgress}%` : t('editor.export')}
          >
            {isExporting ? (
              <>
                <Loader2 size={18} className="spin" />
                <span>{exportProgress > 0 ? `${exportProgress}%` : t('editor.exporting')}</span>
              </>
            ) : (
              <>
                <Download size={18} />
                <span>{t('editor.export')}</span>
              </>
            )}
          </button>
        </div>
      </div>

      {/* Timeline Content */}
      <div className="timeline-content">
        {/* Track Labels */}
        <div className="timeline-labels">
          {tracks.map((track) => (
            <div key={track.id} className={`track-label ${track.locked ? 'locked' : ''}`}>
              <span className="track-name">{track.name}</span>
              <button
                className={`track-lock-btn ${track.locked ? 'active' : ''}`}
                onClick={() => updateTrack({ trackId: track.id, updates: { locked: !track.locked } })}
                title={track.locked ? t('layers.unlockTrack') : t('layers.lockTrack')}
              >
                {track.locked ? <Lock size={12} /> : <Unlock size={12} />}
              </button>
            </div>
          ))}
        </div>

        {/* Timeline Tracks */}
        <div className="timeline-tracks-wrapper" ref={timelineRef}>
          <div
            className="timeline-tracks"
            style={{ width: duration * pxPerFrame }}
            onClick={handleTimelineClick}
          >
            {/* Time Ruler */}
            <TimeRuler
              duration={duration}
              fps={fps}
              pxPerFrame={pxPerFrame}
            />

            {/* Tracks */}
            {tracks.map((track) => (
              <Track
                key={track.id}
                track={track}
                pxPerFrame={pxPerFrame}
              />
            ))}

            {/* In/Out Zone */}
            {(inPoint !== null || outPoint !== null) && (
              <div className="in-out-zone">
                {inPoint !== null && (
                  <div
                    className="in-out-marker in-marker"
                    style={{ left: inPoint * pxPerFrame }}
                    title={`In: ${inPoint}`}
                  />
                )}
                {outPoint !== null && (
                  <div
                    className="in-out-marker out-marker"
                    style={{ left: outPoint * pxPerFrame }}
                    title={`Out: ${outPoint}`}
                  />
                )}
                {inPoint !== null && outPoint !== null && inPoint < outPoint && (
                  <div
                    className="in-out-range"
                    style={{
                      left: inPoint * pxPerFrame,
                      width: (outPoint - inPoint) * pxPerFrame,
                    }}
                  />
                )}
              </div>
            )}

            {/* Timeline Markers */}
            {markers.map((marker) => (
              <div
                key={marker.id}
                className={`timeline-marker marker-${marker.color}`}
                style={{ left: marker.frame * pxPerFrame }}
                title={`${marker.name} (${marker.frame})`}
              >
                <div className="marker-flag" />
              </div>
            ))}

            {/* Playhead */}
            <Playhead
              frame={currentFrame}
              pxPerFrame={pxPerFrame}
            />
          </div>
        </div>
      </div>

      {/* Volume Popup - Rendered here to bypass TrackItem memo */}
      <VolumePopup />

      {/* Critical Blob URL Error Dialog - blocks export */}
      {showCriticalBlobError && (
        <div className="blob-warning-overlay" onClick={() => setShowCriticalBlobError(false)}>
          <div className="blob-warning-dialog" onClick={(e) => e.stopPropagation()}>
            <div className="blob-warning-header">
              <X size={24} className="warning-icon" style={{ color: '#ef4444' }} />
              <h3>{t('blob.criticalTitle')}</h3>
            </div>
            <p className="blob-warning-text">
              {t('blob.criticalText')}
            </p>
            <ul className="blob-warning-list">
              {blobAssets.critical.map((name, i) => (
                <li key={i} style={{ color: '#ef4444' }}>{name}</li>
              ))}
            </ul>
            <p className="blob-warning-hint">
              {t('blob.criticalHint')}
            </p>
            <div className="blob-warning-actions">
              <button
                className="blob-warning-btn primary"
                onClick={() => setShowCriticalBlobError(false)}
              >
                {t('dialog.ok')}
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Blob URL Warning Dialog - optional assets */}
      {showBlobWarning && (
        <div className="blob-warning-overlay" onClick={() => setShowBlobWarning(false)}>
          <div className="blob-warning-dialog" onClick={(e) => e.stopPropagation()}>
            <div className="blob-warning-header">
              <AlertTriangle size={24} className="warning-icon" />
              <h3>{t('blob.title')}</h3>
            </div>
            <p className="blob-warning-text">
              {t('blob.text')}
            </p>
            <ul className="blob-warning-list">
              {blobAssets.optional.map((name, i) => (
                <li key={i}>{name}</li>
              ))}
            </ul>
            <p className="blob-warning-hint">
              {t('blob.hint')}
            </p>
            <div className="blob-warning-actions">
              <button
                className="blob-warning-btn secondary"
                onClick={() => setShowBlobWarning(false)}
              >
                {t('dialog.cancel')}
              </button>
              <button
                className="blob-warning-btn primary"
                onClick={handleExport}
              >
                {t('dialog.exportAnyway')}
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Reset Confirmation Dialog */}
      {showResetConfirm && (
        <div className="reset-overlay" onClick={() => setShowResetConfirm(false)}>
          <div className="reset-dialog" onClick={(e) => e.stopPropagation()}>
            <div className="reset-header">
              <RotateCcw size={24} className="reset-icon" />
              <h3>{t('dialog.reset.title')}</h3>
            </div>
            <p className="reset-text">
              {t('dialog.reset.text')}
            </p>
            <p className="reset-warning">
              {t('dialog.reset.warning')}
            </p>
            <div className="reset-actions">
              <button
                className="reset-btn-secondary"
                onClick={() => setShowResetConfirm(false)}
              >
                {t('dialog.cancel')}
              </button>
              <button
                className="reset-btn-danger"
                onClick={() => {
                  resetToDefaults();
                  setShowResetConfirm(false);
                }}
              >
                {t('dialog.reset')}
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Save Template Dialog */}
      {showSaveTemplateDialog && (
        <div className="save-template-overlay" onClick={() => setShowSaveTemplateDialog(false)}>
          <div className="save-template-dialog" onClick={(e) => e.stopPropagation()}>
            <div className="save-template-header">
              <Save size={24} />
              <h3>{t('templates.saveTitle')}</h3>
            </div>
            <p className="save-template-text">{t('templates.saveDescription')}</p>
            <input
              type="text"
              className="save-template-input"
              value={templateName}
              onChange={(e) => setTemplateName(e.target.value)}
              placeholder={t('templates.namePlaceholder')}
              autoFocus
              onKeyDown={(e) => {
                if (e.key === 'Enter') handleSaveTemplate();
                if (e.key === 'Escape') setShowSaveTemplateDialog(false);
              }}
            />
            <div className="save-template-actions">
              <button
                className="save-template-btn secondary"
                onClick={() => setShowSaveTemplateDialog(false)}
              >
                {t('dialog.cancel')}
              </button>
              <button
                className="save-template-btn primary"
                onClick={handleSaveTemplate}
                disabled={!templateName.trim()}
              >
                {t('templates.save')}
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
