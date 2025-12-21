import { useState, useRef } from 'react';
import { useAtomValue, useSetAtom } from 'jotai';
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
} from '@/atoms';
import { editorStore } from '@/atoms/Provider';
import { Download, Play, Pause, Settings, Wifi, WifiOff, Loader2, AlertTriangle, Undo2, Redo2, X, Keyboard, Upload, Save, RotateCcw } from 'lucide-react';
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

  const play = useSetAtom(playAtom);
  const pause = useSetAtom(pauseAtom);
  const setExportingAction = useSetAtom(setExportingAtom);
  const undo = useSetAtom(undoAtom);
  const redo = useSetAtom(redoAtom);
  const resetTracks = useSetAtom(resetTracksAtom);
  const updateTemplateProp = useSetAtom(updateTemplatePropAtom);
  const setProject = useSetAtom(projectAtom);

  // Обертки для совместимости
  const setExporting = (exporting: boolean, progress?: number) => setExportingAction({ exporting, progress });
  const canUndo = () => canUndoValue;
  const canRedo = () => canRedoValue;
  const resetToDefaults = () => resetTracks({ fps: 30, durationInFrames: 825 });

  const [showBlobWarning, setShowBlobWarning] = useState(false);
  const [showResetConfirm, setShowResetConfirm] = useState(false);
  const [showSettings, setShowSettings] = useState(false);
  const [exportSettings, setExportSettings] = useState<ExportSettings>(getExportSettings);
  const projectImportRef = useRef<HTMLInputElement>(null);

  const handleSettingsChange = (key: keyof ExportSettings, value: string) => {
    const newSettings = { ...exportSettings, [key]: value };
    setExportSettings(newSettings);
    saveExportSettings(newSettings);
  };
  const [blobAssets, setBlobAssets] = useState<string[]>([]);

  // Project Export - save as JSON file
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
          alert('Invalid project file format');
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
        alert(`Project "${data.project?.name || 'Unknown'}" imported successfully!`);
      } catch (err) {
        console.error('[Project] Import failed:', err);
        alert('Failed to import project. Invalid JSON format.');
      }
    };

    reader.readAsText(file);
    e.target.value = '';
  };

  // Check for blob URLs in assets
  const checkForBlobUrls = (): string[] => {
    const blobUrls: string[] = [];

    // Check backgroundVideos
    templateProps.backgroundVideos.forEach((url) => {
      if (url.startsWith('blob:')) {
        const asset = assets.find((a) => a.url === url);
        blobUrls.push(asset?.name || 'Unknown video');
      }
    });

    // Check lipSyncVideo
    if (templateProps.lipSyncVideo.startsWith('blob:')) {
      blobUrls.push('Lipsync video');
    }

    // Check coverImage
    if (templateProps.coverImage.startsWith('blob:')) {
      blobUrls.push('Cover image');
    }

    // Check backgroundMusic
    if (templateProps.backgroundMusic?.startsWith('blob:')) {
      blobUrls.push('Background music');
    }

    return blobUrls;
  };

  const handleExportClick = () => {
    const blobs = checkForBlobUrls();
    if (blobs.length > 0) {
      setBlobAssets(blobs);
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
        backgroundMusic: templateProps.backgroundMusic ? toAbsoluteUrl(templateProps.backgroundMusic) : '',
        backgroundVideos: filteredBackgroundVideos,
        // Add segments with timeline positions for render
        segments: validSegments,
      };

      logExport('Render props prepared', {
        segmentsCount: validSegments.length,
        hasMusic: !!templateProps.backgroundMusic,
        lipSyncVideo: renderProps.lipSyncVideo,
      });
      console.log('[Export] Render props:', renderProps);

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
              alert(`Export failed: ${data.error || 'Unknown error'}`);
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
            alert('Lost connection to render server');
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
      alert(`Export failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
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
      </div>

      <div className="header-right">
        {/* Undo/Redo buttons */}
        <div className="undo-redo-group">
          <button
            className="header-button undo-btn"
            onClick={undo}
            disabled={!canUndo()}
            title="Undo (Cmd+Z)"
          >
            <Undo2 size={16} />
          </button>
          <button
            className="header-button redo-btn"
            onClick={redo}
            disabled={!canRedo()}
            title="Redo (Cmd+Shift+Z)"
          >
            <Redo2 size={16} />
          </button>
        </div>

        {/* WebSocket status indicator */}
        {wsStatus && (
          <div
            className={`ws-status ${wsStatus}`}
            title={wsStatus === 'connected'
              ? `Real-time sync active${wsClientId ? ` (${wsClientId.slice(0, 8)})` : ''}`
              : 'Connecting to sync server...'}
          >
            {wsStatus === 'connected' ? (
              <Wifi size={16} className="ws-icon connected" />
            ) : (
              <WifiOff size={16} className="ws-icon disconnected" />
            )}
          </div>
        )}

        <button
          className="header-button"
          onClick={() => (isPlaying ? pause() : play())}
          title={isPlaying ? 'Pause (Space)' : 'Play (Space)'}
        >
          {isPlaying ? <Pause size={18} /> : <Play size={18} />}
        </button>

        {/* Project Save/Load */}
        <button
          className="header-button"
          onClick={handleProjectExport}
          title="Save Project (.vibee.json)"
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
          title="Load Project"
        >
          <Upload size={18} />
        </button>

        <button
          className="header-button reset-btn"
          onClick={() => setShowResetConfirm(true)}
          title="Reset to Defaults"
        >
          <RotateCcw size={18} />
        </button>

        <button
          className="header-button"
          title="Settings"
          onClick={() => setShowSettings(true)}
        >
          <Settings size={18} />
        </button>

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
                : 'Rendering...'
              : 'Export'}
          </span>
        </button>
      </div>

      {/* Blob URL Warning Dialog */}
      {showBlobWarning && (
        <div className="blob-warning-overlay" onClick={() => setShowBlobWarning(false)}>
          <div className="blob-warning-dialog" onClick={(e) => e.stopPropagation()}>
            <div className="blob-warning-header">
              <AlertTriangle size={24} className="warning-icon" />
              <h3>Local Files Detected</h3>
            </div>
            <p className="blob-warning-text">
              The following files are stored locally and will be <strong>skipped</strong> during export:
            </p>
            <ul className="blob-warning-list">
              {blobAssets.map((name, i) => (
                <li key={i}>{name}</li>
              ))}
            </ul>
            <p className="blob-warning-hint">
              To include these files, delete and re-upload them. They will be stored in the cloud.
            </p>
            <div className="blob-warning-actions">
              <button
                className="blob-warning-btn secondary"
                onClick={() => setShowBlobWarning(false)}
              >
                Cancel
              </button>
              <button
                className="blob-warning-btn primary"
                onClick={handleExport}
              >
                Export Anyway
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
              <h3>Reset to Defaults?</h3>
            </div>
            <p className="reset-text">
              All changes will be lost. Timeline, assets, and settings will be restored to their original state.
            </p>
            <p className="reset-warning">
              This action cannot be undone.
            </p>
            <div className="reset-actions">
              <button
                className="reset-btn-secondary"
                onClick={() => setShowResetConfirm(false)}
              >
                Cancel
              </button>
              <button
                className="reset-btn-danger"
                onClick={() => {
                  resetToDefaults();
                  setShowResetConfirm(false);
                }}
              >
                Reset
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
              <h2>Settings</h2>
              <button className="settings-close" onClick={() => setShowSettings(false)}>
                <X size={20} />
              </button>
            </div>

            <div className="settings-content">
              {/* Export Settings */}
              <div className="settings-section">
                <h3>Export</h3>
                <div className="settings-row">
                  <label>Codec</label>
                  <select
                    value={exportSettings.codec}
                    onChange={(e) => handleSettingsChange('codec', e.target.value)}
                  >
                    <option value="h264">H.264 (MP4) - Best compatibility</option>
                    <option value="h265">H.265 (HEVC) - Smaller size</option>
                    <option value="vp9">VP9 (WebM) - Web optimized</option>
                    <option value="prores">ProRes - Professional</option>
                  </select>
                </div>
                <div className="settings-row">
                  <label>Quality</label>
                  <select
                    value={exportSettings.quality}
                    onChange={(e) => handleSettingsChange('quality', e.target.value)}
                  >
                    <option value="high">High (1080p)</option>
                    <option value="medium">Medium (720p)</option>
                    <option value="low">Low (480p)</option>
                  </select>
                </div>
              </div>

              {/* Keyboard Shortcuts */}
              <div className="settings-section">
                <h3><Keyboard size={16} /> Keyboard Shortcuts</h3>
                <div className="shortcuts-grid">
                  <div className="shortcut-item">
                    <kbd>Space</kbd>
                    <span>Play / Pause</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>J / K / L</kbd>
                    <span>-1s / Pause / +1s</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + Z</kbd>
                    <span>Undo</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + Shift + Z</kbd>
                    <span>Redo</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + A</kbd>
                    <span>Select All</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + C</kbd>
                    <span>Copy</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + V</kbd>
                    <span>Paste</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + D</kbd>
                    <span>Duplicate</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Delete / Backspace</kbd>
                    <span>Delete Selected</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Escape</kbd>
                    <span>Clear Selection</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Arrow Left/Right</kbd>
                    <span>Move 1 Frame</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Shift + Arrow</kbd>
                    <span>Move 10 Frames</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Home / End</kbd>
                    <span>Go to Start / End</span>
                  </div>
                </div>
              </div>

              {/* Project Info */}
              <div className="settings-section">
                <h3>Project</h3>
                <div className="project-info">
                  <div className="info-row">
                    <span>Name:</span>
                    <span>{project.name}</span>
                  </div>
                  <div className="info-row">
                    <span>Resolution:</span>
                    <span>{project.width} x {project.height}</span>
                  </div>
                  <div className="info-row">
                    <span>FPS:</span>
                    <span>{project.fps}</span>
                  </div>
                  <div className="info-row">
                    <span>Duration:</span>
                    <span>{(project.durationInFrames / project.fps).toFixed(1)}s ({project.durationInFrames} frames)</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      )}
    </header>
  );
}
