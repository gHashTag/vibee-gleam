import { useState, useRef, useEffect } from 'react';
import { useAtomValue, useSetAtom } from 'jotai';
import { useLanguage } from '@/hooks/useLanguage';
import {
  projectAtom,
  tracksAtom,
  assetsAtom,
  isPlayingAtom,
  isExportingAtom,
  exportProgressAtom,
  playAtom,
  pauseAtom,
  setExportingAtom,
  templatePropsAtom,
  undoAtom,
  redoAtom,
  canUndoAtom,
  canRedoAtom,
  resetTracksAtom,
  segmentsAtom,
  updateTemplatePropAtom,
  // User & Quota atoms
  userAtom,
  renderQuotaAtom,
  showPaywallAtom,
  showLoginModalAtom,
  fetchQuotaAtom,
  logRenderAtom,
  canRenderAtom,
  logoutAtom,
  // Templates
  addTemplateAtom,
  sidebarTabAtom,
  // Template props atoms for reset
  musicVolumeAtom,
  vignetteStrengthAtom,
  colorCorrectionAtom,
  faceOffsetXAtom,
  faceOffsetYAtom,
  showCaptionsAtom,
  playbackRateAtom,
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
import { editorStore } from '@/atoms/Provider';
import { Download, Play, Pause, Loader2, AlertTriangle, Undo2, Redo2, X, Keyboard, Upload, Save, RotateCcw, Zap } from 'lucide-react';
import { TelegramLoginButton, UserAvatar, PaywallModal } from '@/components/Auth';
import { RemixBadge } from '@/components/RemixBadge';
import { RENDER_SERVER_URL, toAbsoluteUrl } from '@/lib/mediaUrl';
import { DEFAULT_COMPOSITION_ID } from '@/shared/compositions';
import { logExport } from '@/lib/logger';
import './styles.css';

// Export settings stored in localStorage
interface ExportSettings {
  codec: 'h264' | 'h265' | 'vp9' | 'prores';
  quality: 'high' | 'medium' | 'low';
}

const DEFAULT_EXPORT_SETTINGS: ExportSettings = {
  codec: 'h264',
  quality: 'high',
};

function getExportSettings(): ExportSettings {
  try {
    const saved = localStorage.getItem('vibee-export-settings');
    return saved ? { ...DEFAULT_EXPORT_SETTINGS, ...JSON.parse(saved) } : DEFAULT_EXPORT_SETTINGS;
  } catch {
    return DEFAULT_EXPORT_SETTINGS;
  }
}

function saveExportSettings(settings: ExportSettings) {
  localStorage.setItem('vibee-export-settings', JSON.stringify(settings));
}

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

interface HeaderProps {
  wsStatus?: 'connected' | 'disconnected';
  wsClientId?: string | null;
}

export function Header({ wsStatus, wsClientId }: HeaderProps) {
  // Language hook
  const { lang, setLang, t } = useLanguage();

  // Jotai atoms - прямое использование
  const project = useAtomValue(projectAtom);
  const tracks = useAtomValue(tracksAtom);
  const isPlaying = useAtomValue(isPlayingAtom);
  const isExporting = useAtomValue(isExportingAtom);
  const exportProgress = useAtomValue(exportProgressAtom);
  const templateProps = useAtomValue(templatePropsAtom);
  const assets = useAtomValue(assetsAtom);
  const canUndoValue = useAtomValue(canUndoAtom);
  const canRedoValue = useAtomValue(canRedoAtom);
  const segments = useAtomValue(segmentsAtom);

  // User & Quota state
  const user = useAtomValue(userAtom);
  const quota = useAtomValue(renderQuotaAtom);
  const showPaywall = useAtomValue(showPaywallAtom);
  const showLoginModal = useAtomValue(showLoginModalAtom);
  const canRender = useAtomValue(canRenderAtom);

  const play = useSetAtom(playAtom);
  const pause = useSetAtom(pauseAtom);
  const setExportingAction = useSetAtom(setExportingAtom);
  const undo = useSetAtom(undoAtom);
  const redo = useSetAtom(redoAtom);
  const resetTracks = useSetAtom(resetTracksAtom);
  const updateTemplateProp = useSetAtom(updateTemplatePropAtom);
  const setProject = useSetAtom(projectAtom);

  // User actions
  const fetchQuota = useSetAtom(fetchQuotaAtom);
  const logRender = useSetAtom(logRenderAtom);
  const logout = useSetAtom(logoutAtom);
  const setShowPaywall = useSetAtom(showPaywallAtom);
  const setShowLoginModal = useSetAtom(showLoginModalAtom);

  // Templates
  const addTemplate = useSetAtom(addTemplateAtom);
  const setSidebarTab = useSetAtom(sidebarTabAtom);

  // Template props setters for reset
  const setMusicVolume = useSetAtom(musicVolumeAtom);
  const setVignetteStrength = useSetAtom(vignetteStrengthAtom);
  const setColorCorrection = useSetAtom(colorCorrectionAtom);
  const setFaceOffsetX = useSetAtom(faceOffsetXAtom);
  const setFaceOffsetY = useSetAtom(faceOffsetYAtom);
  const setShowCaptions = useSetAtom(showCaptionsAtom);
  const setPlaybackRate = useSetAtom(playbackRateAtom);
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

  // Fetch quota on mount if user is logged in
  useEffect(() => {
    if (user) {
      fetchQuota();
    }
  }, [user, fetchQuota]);

  // Обертки для совместимости
  const setExporting = (exporting: boolean, progress?: number) => setExportingAction({ exporting, progress });
  const canUndo = () => canUndoValue;
  const canRedo = () => canRedoValue;
  const resetToDefaults = () => {
    // Reset tracks/timeline
    resetTracks({ fps: 30, durationInFrames: 825 });

    // Reset ALL template props settings
    setMusicVolume(0.06);
    setVignetteStrength(0.7);
    setColorCorrection(1.2);
    setFaceOffsetX(0);
    setFaceOffsetY(0);
    setShowCaptions(true);
    setPlaybackRate(1);

    // Split mode defaults
    setSplitSize(100);
    setSplitPosX(0);
    setSplitPosY(0);
    setSplitScale(1.0);
    setSplitIsCircle(false);
    setSplitRadius(50);

    // Fullscreen mode defaults
    setFullSize(100);
    setFullPosX(0);
    setFullPosY(0);
    setFullScale(1.0);
    setFullIsCircle(false);
    setFullRadius(50);
  };

  const [showBlobWarning, setShowBlobWarning] = useState(false);
  const [showCriticalBlobError, setShowCriticalBlobError] = useState(false);
  const [showResetConfirm, setShowResetConfirm] = useState(false);
  const [showSettings, setShowSettings] = useState(false);
  const [exportSettings, setExportSettings] = useState<ExportSettings>(getExportSettings);
  const projectImportRef = useRef<HTMLInputElement>(null);

  // Save as template dialog
  const [showSaveTemplateDialog, setShowSaveTemplateDialog] = useState(false);
  const [templateName, setTemplateName] = useState('');

  const handleSettingsChange = (key: keyof ExportSettings, value: string) => {
    const newSettings = { ...exportSettings, [key]: value };
    setExportSettings(newSettings);
    saveExportSettings(newSettings);
  };
  const [blobAssets, setBlobAssets] = useState<{ critical: string[]; optional: string[] }>({ critical: [], optional: [] });

  // Open save as template dialog
  const handleSaveClick = () => {
    setTemplateName(project.name);
    setShowSaveTemplateDialog(true);
  };

  // Save current state as a template
  const handleSaveTemplate = () => {
    if (!templateName.trim()) return;

    addTemplate({
      name: templateName.trim(),
      description: `Created from ${project.name}`,
      compositionId: 'SplitTalkingHead',
      defaultProps: { ...templateProps },
      assets: [...assets],
      tracks: JSON.parse(JSON.stringify(tracks)), // Deep clone
    });

    setShowSaveTemplateDialog(false);
    setTemplateName('');

    // Switch to templates tab to show the new template
    setSidebarTab('templates');

    console.log('[Templates] Saved new template:', templateName);
  };

  // Project Export - save as JSON file (legacy, kept for import/export)
  const handleProjectExport = () => {
    const projectData = {
      version: 1,
      exportedAt: new Date().toISOString(),
      project,
      tracks,
      assets,
      templateProps,
    };

    const blob = new Blob([JSON.stringify(projectData, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `${project.name.replace(/[^a-z0-9]/gi, '_')}.vibee.json`;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
  };

  // Project Import - load from JSON file
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

        // Apply imported data to store
        if (data.project) editorStore.set(projectAtom, data.project);
        if (data.templateProps) {
          Object.entries(data.templateProps).forEach(([key, value]) => {
            editorStore.set(updateTemplatePropAtom, { key: key as any, value: value as any });
          });
        }

        console.log(`[Project] Imported project: ${data.project?.name || 'Unknown'}`);
        alert(t('editor.importSuccess'));
      } catch (err) {
        console.error('[Project] Import failed:', err);
        alert(t('editor.importFailed'));
      }
    };

    reader.readAsText(file);
    e.target.value = '';
  };

  // Check for blob URLs in assets
  // Returns { critical: string[], optional: string[] }
  // Critical = lipSyncVideo (required for render), Optional = can be skipped
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
    <header className="header">
      <div className="header-left">
        <div className="logo">
          <span className="logo-icon">🐝</span>
          <span className="logo-text">VIBEE Editor</span>
        </div>
      </div>

      <div className="header-center">
        <input
          type="text"
          className="project-name-input"
          value={project.name}
          onChange={(e) => setProject({ ...project, name: e.target.value })}
        />
        <RemixBadge />
      </div>

      <div className="header-right">
        {/* Undo/Redo buttons */}
        <div className="undo-redo-group">
          <button
            className="header-button undo-btn"
            onClick={undo}
            disabled={!canUndo()}
            title={`${t('editor.undo')} (Cmd+Z)`}
          >
            <Undo2 size={16} />
          </button>
          <button
            className="header-button redo-btn"
            onClick={redo}
            disabled={!canRedo()}
            title={`${t('editor.redo')} (Cmd+Shift+Z)`}
          >
            <Redo2 size={16} />
          </button>
        </div>

        
        <button
          className="header-button"
          onClick={() => (isPlaying ? pause() : play())}
          title={isPlaying ? `${t('player.pause')} (Space)` : `${t('player.play')} (Space)`}
        >
          {isPlaying ? <Pause size={18} /> : <Play size={18} />}
        </button>

        {/* Save as Template */}
        <button
          className="header-button"
          onClick={handleSaveClick}
          title={t('editor.saveTemplate')}
        >
          <Save size={18} />
        </button>
        <input
          ref={projectImportRef}
          type="file"
          accept=".json,.vibee.json"
          onChange={handleProjectImport}
          style={{ display: 'none' }}
        />
        <button
          className="header-button"
          onClick={() => projectImportRef.current?.click()}
          title={t('editor.load')}
        >
          <Upload size={18} />
        </button>

        <button
          className="header-button reset-btn"
          onClick={() => setShowResetConfirm(true)}
          title={t('editor.reset')}
        >
          <RotateCcw size={18} />
        </button>

        {/* Language Switcher */}
        <button
          className="header-button lang-toggle"
          onClick={() => setLang(lang === 'en' ? 'ru' : 'en')}
          title={lang === 'en' ? 'Переключить на русский' : 'Switch to English'}
        >
          {lang.toUpperCase()}
        </button>

        {/* User login / Quota display */}
        {user ? (
          <div className="user-section">
            {/* Quota display */}
            {quota && (
              <div
                className={`quota-display ${
                  quota.free_remaining === 0 && !quota.subscription
                    ? 'exhausted'
                    : quota.free_remaining <= 1
                    ? 'warning'
                    : ''
                }`}
                title={`${quota.total_renders} renders used`}
              >
                <Zap size={14} />
                <span>
                  {quota.subscription
                    ? quota.subscription.remaining === null
                      ? t('quota.unlimited')
                      : `${quota.subscription.remaining} ${t('quota.left')}`
                    : `${quota.free_remaining}/3 ${t('quota.free')}`}
                </span>
              </div>
            )}
            <UserAvatar user={user} onLogout={logout} />
          </div>
        ) : (
          <TelegramLoginButton
            onSuccess={() => setShowLoginModal(false)}
          />
        )}

        <button
          className={`header-button export-button ${isExporting ? 'exporting' : ''}`}
          onClick={handleExportClick}
          disabled={isExporting}
        >
          {isExporting ? <Loader2 size={18} className="spin" /> : <Download size={18} />}
          <span>
            {isExporting
              ? exportProgress > 0
                ? `${exportProgress}%`
                : t('editor.exporting')
              : t('editor.export')}
          </span>
        </button>
      </div>

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

      {/* Settings Modal */}
      {showSettings && (
        <div className="settings-overlay" onClick={() => setShowSettings(false)}>
          <div className="settings-modal" onClick={(e) => e.stopPropagation()}>
            <div className="settings-header">
              <h2>{t('settings.title')}</h2>
              <button className="settings-close" onClick={() => setShowSettings(false)}>
                <X size={20} />
              </button>
            </div>

            <div className="settings-content">
              {/* Export Settings */}
              <div className="settings-section">
                <h3>{t('settings.export')}</h3>
                <div className="settings-row">
                  <label>{t('settings.codec')}</label>
                  <select
                    value={exportSettings.codec}
                    onChange={(e) => handleSettingsChange('codec', e.target.value)}
                  >
                    <option value="h264">{t('codec.h264')}</option>
                    <option value="h265">{t('codec.h265')}</option>
                    <option value="vp9">{t('codec.vp9')}</option>
                    <option value="prores">{t('codec.prores')}</option>
                  </select>
                </div>
                <div className="settings-row">
                  <label>{t('settings.quality')}</label>
                  <select
                    value={exportSettings.quality}
                    onChange={(e) => handleSettingsChange('quality', e.target.value)}
                  >
                    <option value="high">{t('quality.high')}</option>
                    <option value="medium">{t('quality.medium')}</option>
                    <option value="low">{t('quality.low')}</option>
                  </select>
                </div>
              </div>

              {/* Keyboard Shortcuts */}
              <div className="settings-section">
                <h3><Keyboard size={16} /> {t('settings.shortcuts')}</h3>
                <div className="shortcuts-grid">
                  <div className="shortcut-item">
                    <kbd>Space</kbd>
                    <span>{t('shortcut.playPause')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>J / K / L</kbd>
                    <span>{t('shortcut.jkl')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + Z</kbd>
                    <span>{t('shortcut.undo')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + Shift + Z</kbd>
                    <span>{t('shortcut.redo')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + A</kbd>
                    <span>{t('shortcut.selectAll')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + C</kbd>
                    <span>{t('shortcut.copy')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + V</kbd>
                    <span>{t('shortcut.paste')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + D</kbd>
                    <span>{t('shortcut.duplicate')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Delete / Backspace</kbd>
                    <span>{t('shortcut.delete')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Escape</kbd>
                    <span>{t('shortcut.clearSelection')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Arrow Left/Right</kbd>
                    <span>{t('shortcut.move1Frame')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Shift + Arrow</kbd>
                    <span>{t('shortcut.move10Frames')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Home / End</kbd>
                    <span>{t('shortcut.goToStartEnd')}</span>
                  </div>
                </div>
              </div>

              {/* Project Info */}
              <div className="settings-section">
                <h3>{t('settings.project')}</h3>
                <div className="project-info">
                  <div className="info-row">
                    <span>{t('settings.name')}:</span>
                    <span>{project.name}</span>
                  </div>
                  <div className="info-row">
                    <span>{t('settings.resolution')}:</span>
                    <span>{project.width} x {project.height}</span>
                  </div>
                  <div className="info-row">
                    <span>{t('settings.fps')}:</span>
                    <span>{project.fps}</span>
                  </div>
                  <div className="info-row">
                    <span>{t('settings.duration')}:</span>
                    <span>{(project.durationInFrames / project.fps).toFixed(1)}s ({project.durationInFrames} frames)</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Paywall Modal */}
      <PaywallModal />

      {/* Login Modal */}
      {showLoginModal && (
        <div className="login-modal-overlay" onClick={() => setShowLoginModal(false)}>
          <div className="login-modal" onClick={(e) => e.stopPropagation()}>
            <h2>{t('login.title')}</h2>
            <p>{t('login.subtitle')}</p>
            <div className="login-modal-widget">
              <TelegramLoginButton
                onSuccess={() => setShowLoginModal(false)}
                size="large"
                showFallback={true}
              />
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
    </header>
  );
}
