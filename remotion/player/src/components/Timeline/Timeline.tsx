import { useRef, useCallback, useEffect } from 'react';
import { useEditorStore } from '@/store/editorStore';
import { TimeRuler } from './TimeRuler';
import { Track } from './Track';
import { Playhead } from './Playhead';
import { Play, Pause, SkipBack, SkipForward, ZoomIn, ZoomOut, ChevronDown, ChevronUp, Magnet, Lock, Unlock, Maximize2, Volume2, VolumeX } from 'lucide-react';
import './Timeline.css';

export function Timeline() {
  const timelineRef = useRef<HTMLDivElement>(null);

  const project = useEditorStore((s) => s.project);
  const tracks = useEditorStore((s) => s.tracks);
  const currentFrame = useEditorStore((s) => s.currentFrame);
  const isPlaying = useEditorStore((s) => s.isPlaying);
  const playbackRate = useEditorStore((s) => s.playbackRate);
  const timelineZoom = useEditorStore((s) => s.timelineZoom);
  const setCurrentFrame = useEditorStore((s) => s.setCurrentFrame);
  const playDirect = useEditorStore((s) => s.playDirect);
  const pauseDirect = useEditorStore((s) => s.pauseDirect);
  const setPlaybackRate = useEditorStore((s) => s.setPlaybackRate);
  const setTimelineZoom = useEditorStore((s) => s.setTimelineZoom);
  const snapSettings = useEditorStore((s) => s.snapSettings);
  const setSnapEnabled = useEditorStore((s) => s.setSnapEnabled);
  const updateTrack = useEditorStore((s) => s.updateTrack);
  const inPoint = useEditorStore((s) => s.inPoint);
  const outPoint = useEditorStore((s) => s.outPoint);
  const markers = useEditorStore((s) => s.markers);
  const isMuted = useEditorStore((s) => s.isMuted);
  const setIsMuted = useEditorStore((s) => s.setIsMuted);
  const volume = useEditorStore((s) => s.volume);
  const setVolume = useEditorStore((s) => s.setVolume);
  const playerRef = useEditorStore((s) => s.playerRef);

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

  return (
    <div className="timeline">
      {/* Transport Controls */}
      <div className="timeline-transport">
        <div className="transport-controls">
          <button className="transport-btn" onClick={handleSkipBack} title="Skip to start">
            <SkipBack size={16} />
          </button>
          <button
            className="transport-btn play-btn"
            onClickCapture={handlePlay}
            title={isPlaying ? 'Pause (Space)' : 'Play (Space)'}
          >
            {isPlaying ? <Pause size={18} /> : <Play size={18} />}
          </button>
          <button className="transport-btn" onClick={handleSkipForward} title="Skip to end">
            <SkipForward size={16} />
          </button>
        </div>

        <div className="transport-time">
          <span className="time-current">{formatTime(currentFrame)}</span>
          <span className="time-separator">/</span>
          <span className="time-duration">{formatTime(duration)}</span>
        </div>

        <div className="transport-speed">
          <button
            className="speed-btn"
            onClick={handleSpeedDown}
            disabled={currentSpeedIndex <= 0}
            title="Slower"
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
            title="Faster"
          >
            <ChevronUp size={12} />
          </button>
        </div>

        <div className="volume-control">
          <button
            className={`transport-btn ${isMuted ? '' : 'active'}`}
            onClick={handleMuteToggle}
            title={isMuted ? 'Unmute audio' : 'Mute audio'}
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
            title={`Volume: ${Math.round(volume * 100)}%`}
          />
        </div>

        <button
          className={`transport-btn snap-btn ${snapSettings.enabled ? 'active' : ''}`}
          onClick={() => setSnapEnabled(!snapSettings.enabled)}
          title={`Snap to grid (${snapSettings.enabled ? 'ON' : 'OFF'})`}
        >
          <Magnet size={14} />
        </button>

        <div className="transport-zoom">
          <button className="transport-btn" onClick={handleZoomOut} title="Zoom out (-)">
            <ZoomOut size={14} />
          </button>
          <span className="zoom-label">{Math.round(timelineZoom * 100)}%</span>
          <button className="transport-btn" onClick={handleZoomIn} title="Zoom in (+)">
            <ZoomIn size={14} />
          </button>
          <button className="transport-btn" onClick={handleFitToView} title="Fit to view (Shift+Z)">
            <Maximize2 size={14} />
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
                onClick={() => updateTrack(track.id, { locked: !track.locked })}
                title={track.locked ? 'Unlock track' : 'Lock track'}
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
    </div>
  );
}
