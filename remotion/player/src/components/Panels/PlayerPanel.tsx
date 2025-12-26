import { useState } from 'react';
import { useAtom, useAtomValue } from 'jotai';
import {
  musicVolumeAtom,
  vignetteStrengthAtom,
  colorCorrectionAtom,
  faceOffsetXAtom,
  faceOffsetYAtom,
  showCaptionsAtom,
  playbackRateAtom,
  lipSyncVideoAtom,
  // Split/Fullscreen mode atoms
  avatarSettingsTabAtom,
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
} from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import { analyzeFace } from '@/lib/agentApi';
import { Music, Palette, User, Type, Play, ScanFace, Circle, Layers, Maximize, RotateCcw } from 'lucide-react';
import './PlayerPanel.css';

export function PlayerPanel() {
  const { t } = useLanguage();
  const [isDetecting, setIsDetecting] = useState(false);

  // Common atoms
  const [musicVolume, setMusicVolume] = useAtom(musicVolumeAtom);
  const [vignetteStrength, setVignetteStrength] = useAtom(vignetteStrengthAtom);
  const [colorCorrection, setColorCorrection] = useAtom(colorCorrectionAtom);
  const [, setFaceOffsetX] = useAtom(faceOffsetXAtom);
  const [, setFaceOffsetY] = useAtom(faceOffsetYAtom);
  const [showCaptions, setShowCaptions] = useAtom(showCaptionsAtom);
  const [playbackRate, setPlaybackRate] = useAtom(playbackRateAtom);
  const lipSyncVideo = useAtomValue(lipSyncVideoAtom);

  // Tab selection
  const [activeTab, setActiveTab] = useAtom(avatarSettingsTabAtom);

  // Split mode atoms
  const [splitSize, setSplitSize] = useAtom(splitCircleSizeAtom);
  const [splitPosX, setSplitPosX] = useAtom(splitPositionXAtom);
  const [splitPosY, setSplitPosY] = useAtom(splitPositionYAtom);
  const [splitScale, setSplitScale] = useAtom(splitFaceScaleAtom);
  const [splitIsCircle, setSplitIsCircle] = useAtom(splitIsCircleAtom);
  const [splitRadius, setSplitRadius] = useAtom(splitBorderRadiusAtom);

  // Fullscreen mode atoms
  const [fullSize, setFullSize] = useAtom(fullscreenCircleSizeAtom);
  const [fullPosX, setFullPosX] = useAtom(fullscreenPositionXAtom);
  const [fullPosY, setFullPosY] = useAtom(fullscreenPositionYAtom);
  const [fullScale, setFullScale] = useAtom(fullscreenFaceScaleAtom);
  const [fullIsCircle, setFullIsCircle] = useAtom(fullscreenIsCircleAtom);
  const [fullRadius, setFullRadius] = useAtom(fullscreenBorderRadiusAtom);

  // Current mode values based on active tab
  const isSplit = activeTab === 'split';
  const size = isSplit ? splitSize : fullSize;
  const setSize = isSplit ? setSplitSize : setFullSize;
  const posX = isSplit ? splitPosX : fullPosX;
  const setPosX = isSplit ? setSplitPosX : setFullPosX;
  const posY = isSplit ? splitPosY : fullPosY;
  const setPosY = isSplit ? setSplitPosY : setFullPosY;
  const scale = isSplit ? splitScale : fullScale;
  const setScale = isSplit ? setSplitScale : setFullScale;
  const isCircle = isSplit ? splitIsCircle : fullIsCircle;
  const setIsCircle = isSplit ? setSplitIsCircle : setFullIsCircle;
  const radius = isSplit ? splitRadius : fullRadius;
  const setRadius = isSplit ? setSplitRadius : setFullRadius;

  // Auto detect face center
  const handleAutoDetect = async () => {
    if (!lipSyncVideo || isDetecting) return;

    setIsDetecting(true);
    try {
      const result = await analyzeFace(lipSyncVideo);

      if (result.success && result.faceDetected && result.cropSettings) {
        setFaceOffsetX(result.cropSettings.offsetX);
        setFaceOffsetY(result.cropSettings.offsetY);
        setScale(result.cropSettings.scale);
        console.log('[PlayerPanel] Face detected, applied:', result.cropSettings);
      } else {
        console.warn('[PlayerPanel] No face detected');
      }
    } catch (error) {
      console.error('[PlayerPanel] Face detection failed:', error);
    } finally {
      setIsDetecting(false);
    }
  };

  // Reset current mode to defaults
  const handleReset = () => {
    if (isSplit) {
      setSplitSize(25);
      setSplitPosX(0);
      setSplitPosY(0);
      setSplitScale(1.0);
      setSplitIsCircle(false); // Default: fill bottom half
      setSplitRadius(50);
    } else {
      setFullSize(50);
      setFullPosX(0);
      setFullPosY(0);
      setFullScale(1.0);
      setFullIsCircle(false); // Default: fill entire screen
      setFullRadius(50);
    }
  };

  return (
    <div className="player-panel">
      {/* Music Section */}
      <div className="player-section">
        <div className="player-section-header">
          <Music size={14} />
          <span>{t('player.music')}</span>
        </div>
        <div className="player-control">
          <label>{t('player.musicVolume')}</label>
          <div className="player-slider-row">
            <input
              type="range"
              min="0"
              max="1"
              step="0.01"
              value={musicVolume}
              onChange={(e) => setMusicVolume(Number(e.target.value))}
            />
            <span className="player-value">{Math.round(musicVolume * 100)}%</span>
          </div>
        </div>
      </div>

      {/* Visual Effects Section */}
      <div className="player-section">
        <div className="player-section-header">
          <Palette size={14} />
          <span>{t('player.effects')}</span>
        </div>
        <div className="player-control">
          <label>{t('player.vignette')}</label>
          <div className="player-slider-row">
            <input
              type="range"
              min="0"
              max="1"
              step="0.05"
              value={vignetteStrength}
              onChange={(e) => setVignetteStrength(Number(e.target.value))}
            />
            <span className="player-value">{Math.round(vignetteStrength * 100)}%</span>
          </div>
        </div>
        <div className="player-control">
          <label>{t('player.colorCorrection')}</label>
          <div className="player-slider-row">
            <input
              type="range"
              min="0.5"
              max="2"
              step="0.1"
              value={colorCorrection}
              onChange={(e) => setColorCorrection(Number(e.target.value))}
            />
            <span className="player-value">{colorCorrection.toFixed(1)}x</span>
          </div>
        </div>
      </div>

      {/* Avatar Section with Tabs */}
      <div className="player-section">
        <div className="player-section-header">
          <User size={14} />
          <span>{t('player.avatar')}</span>
        </div>

        {/* Mode Tabs - Fullscreen first */}
        <div className="player-tabs-row">
          <div className="player-tabs">
            <button
              className={`player-tab ${activeTab === 'fullscreen' ? 'active' : ''}`}
              onClick={() => setActiveTab('fullscreen')}
            >
              <Maximize size={12} />
              {t('player.fullscreen')}
            </button>
            <button
              className={`player-tab ${activeTab === 'split' ? 'active' : ''}`}
              onClick={() => setActiveTab('split')}
            >
              <Layers size={12} />
              {t('player.split')}
            </button>
          </div>
          <button
            className="player-reset-btn"
            onClick={handleReset}
            title={t('player.reset')}
          >
            <RotateCcw size={14} />
          </button>
        </div>

        {/* Auto Detect Face Button */}
        <div className="player-control">
          <label>{t('player.autoDetect')}</label>
          <button
            className={`player-action-btn ${isDetecting ? 'loading' : ''}`}
            onClick={handleAutoDetect}
            disabled={isDetecting || !lipSyncVideo}
          >
            <ScanFace size={14} />
            {isDetecting ? t('player.detecting') : t('player.detect')}
          </button>
        </div>

        {/* Circle Toggle */}
        <div className="player-control">
          <label>{t('player.circle')}</label>
          <button
            className={`player-toggle ${isCircle ? 'active' : ''}`}
            onClick={() => setIsCircle(!isCircle)}
          >
            <Circle size={14} />
            {isCircle ? 'ON' : 'OFF'}
          </button>
        </div>

        {/* Border Radius (only when circle is enabled) */}
        {isCircle && (
          <div className="player-control">
            <label>{t('player.borderRadius')}</label>
            <div className="player-slider-row">
              <input
                type="range"
                min="0"
                max="50"
                step="1"
                value={radius}
                onChange={(e) => setRadius(Number(e.target.value))}
              />
              <span className="player-value">{radius}%</span>
            </div>
          </div>
        )}

        {/* Size/Position controls - only work when Circle is ON */}
        <div className="player-control">
          <label>{t('player.avatarSize')}</label>
          <div className="player-slider-row">
            <input
              type="range"
              min="10"
              max="100"
              step="1"
              value={size}
              onChange={(e) => setSize(Number(e.target.value))}
              disabled={!isCircle}
            />
            <span className="player-value">{Math.round(size)}%</span>
          </div>
        </div>
        <div className="player-control">
          <label>{t('player.positionX')}</label>
          <div className="player-slider-row">
            <input
              type="range"
              min="-50"
              max="50"
              step="1"
              value={posX}
              onChange={(e) => setPosX(Number(e.target.value))}
              disabled={!isCircle}
            />
            <span className="player-value">{Math.round(posX)}%</span>
          </div>
        </div>
        <div className="player-control">
          <label>{t('player.positionY')}</label>
          <div className="player-slider-row">
            <input
              type="range"
              min="-50"
              max="50"
              step="1"
              value={posY}
              onChange={(e) => setPosY(Number(e.target.value))}
              disabled={!isCircle}
            />
            <span className="player-value">{Math.round(posY)}%</span>
          </div>
        </div>
        <div className="player-control">
          <label>{t('player.faceScale')}</label>
          <div className="player-slider-row">
            <input
              type="range"
              min="0.5"
              max="2"
              step="0.1"
              value={scale}
              onChange={(e) => setScale(Number(e.target.value))}
            />
            <span className="player-value">{scale.toFixed(1)}x</span>
          </div>
        </div>
      </div>

      {/* Captions Section */}
      <div className="player-section">
        <div className="player-section-header">
          <Type size={14} />
          <span>{t('player.captions')}</span>
        </div>
        <div className="player-control">
          <label>{t('player.showCaptions')}</label>
          <button
            className={`player-toggle ${showCaptions ? 'active' : ''}`}
            onClick={() => setShowCaptions(!showCaptions)}
          >
            {showCaptions ? 'ON' : 'OFF'}
          </button>
        </div>
      </div>

      {/* Playback Section */}
      <div className="player-section">
        <div className="player-section-header">
          <Play size={14} />
          <span>{t('player.playback')}</span>
        </div>
        <div className="player-control">
          <label>{t('player.playbackSpeed')}</label>
          <div className="player-speed-buttons">
            {[0.5, 1, 1.5, 2].map((speed) => (
              <button
                key={speed}
                className={`player-speed-btn ${playbackRate === speed ? 'active' : ''}`}
                onClick={() => setPlaybackRate(speed)}
              >
                {speed}x
              </button>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}
