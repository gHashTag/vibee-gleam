import { useEffect, useState, Suspense, useRef } from 'react';
import { useSetAtom, useAtomValue } from 'jotai';
import { loadCaptionsAtom, updateDurationFromLipSyncAtom, lipSyncVideoAtom, transcribeVideoAtom, ensureAudioTrackAtom, ensureImageTrackAtom, selectedItemIdsAtom } from '@/atoms';
import { layoutPresetAtom, LAYOUT_PRESETS } from '@/atoms/ui';
import { useAutoRecordHistory } from '@/atoms/hooks';
import { AssetsPanel } from '@/components/Panels/AssetsPanel';
import { TemplatesPanel } from '@/components/Panels/TemplatesPanel';
import { Header } from '@/components/Header';
import { PropertiesPanel } from '@/components/Panels/PropertiesPanel';
import { InteractiveCanvas } from '@/components/Canvas/InteractiveCanvas';
import { Timeline } from '@/components/Timeline/Timeline';
import { ShortcutsModal } from '@/components/Modals/ShortcutsModal';
import { BottomSheet } from '@/components/BottomSheet/BottomSheet';
import { ErrorBoundary } from '@/components/ErrorBoundary';
import { PanelError } from '@/components/Panels/PanelError';
import { useKeyboardShortcuts } from '@/hooks/useKeyboard';
import { useIsMobile, useIsTablet } from '@/hooks/useMediaQuery';
import { useWebSocket, setGlobalWsSend } from '@/lib/websocket';
import { useLanguage } from '@/hooks/useLanguage';

// Sidebar content tabs
type SidebarTab = 'templates' | 'video' | 'image' | 'audio';

function EditorContent() {
  const { t } = useLanguage();
  const [showShortcuts, setShowShortcuts] = useState(false);
  const [sidebarTab, setSidebarTab] = useState<SidebarTab>('video');
  const isMobile = useIsMobile();
  const isTablet = useIsTablet();

  // Use atoms directly instead of bridge
  const updateDurationFromLipSync = useSetAtom(updateDurationFromLipSyncAtom);
  const loadCaptions = useSetAtom(loadCaptionsAtom);
  const lipSyncVideo = useAtomValue(lipSyncVideoAtom);
  const transcribeVideo = useSetAtom(transcribeVideoAtom);
  const ensureAudioTrack = useSetAtom(ensureAudioTrackAtom);
  const ensureImageTrack = useSetAtom(ensureImageTrackAtom);
  const selectedItemIds = useAtomValue(selectedItemIdsAtom);
  const layoutPreset = useAtomValue(layoutPresetAtom);
  const prevLipSyncRef = useRef<string | null>(null);

  // Get layout config
  const layoutConfig = LAYOUT_PRESETS[layoutPreset] || LAYOUT_PRESETS.classic;
  const showAssets = layoutConfig.showAssets;

  // Enable keyboard shortcuts
  useKeyboardShortcuts();

  // Auto-record history on state changes (for undo/redo)
  useAutoRecordHistory();

  // Run migrations and load initial data on mount
  useEffect(() => {
    ensureAudioTrack(); // Migration: ensure audio track exists for old users
    ensureImageTrack(); // Migration: ensure image track exists for old users
    updateDurationFromLipSync();
    loadCaptions();
  }, [ensureAudioTrack, ensureImageTrack, updateDurationFromLipSync, loadCaptions]);

  // Auto-transcribe when lipSyncVideo changes (not on mount)
  useEffect(() => {
    // Skip on initial mount
    if (prevLipSyncRef.current === null) {
      prevLipSyncRef.current = lipSyncVideo;
      return;
    }

    // Skip if same video
    if (prevLipSyncRef.current === lipSyncVideo) {
      return;
    }

    prevLipSyncRef.current = lipSyncVideo;

    // For default video - just load existing captions
    if (lipSyncVideo === '/lipsync/lipsync.mp4') {
      console.log('[Editor] LipSync reset to default, loading captions');
      loadCaptions();
      return;
    }

    // Auto-transcribe new video
    if (lipSyncVideo) {
      console.log('[Editor] LipSync video changed, starting auto-transcribe:', lipSyncVideo);
      transcribeVideo();
    }
  }, [lipSyncVideo, transcribeVideo, loadCaptions]);

  // Initialize WebSocket for real-time sync
  const { send, isConnected, clientId } = useWebSocket({
    onConnect: () => console.log('[Editor] WebSocket connected'),
    onDisconnect: () => console.log('[Editor] WebSocket disconnected'),
  });

  // Set global send function for use in other components
  useEffect(() => {
    setGlobalWsSend(send);
  }, [send]);

  // Handle '?' key for shortcuts modal
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      const target = e.target as HTMLElement;
      if (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA') {
        return;
      }
      if ((e.shiftKey && e.code === 'Slash') || e.code === 'F1') {
        e.preventDefault();
        setShowShortcuts(true);
      }
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, []);

  return (
    <div
      className={`editor layout-${layoutPreset} ${showAssets ? 'has-assets' : 'no-assets'}`}
      style={{
        '--assets-width': `${layoutConfig.assetsWidth}px`,
        '--timeline-height': `${layoutConfig.timelineHeight}px`,
      } as React.CSSProperties}
    >
      <Header
        wsStatus={isConnected ? 'connected' : 'disconnected'}
        wsClientId={clientId}
      />

      <main id="main-content" className="editor-main" role="main">
        {/* Left sidebar: Tabs for each content group */}
        {showAssets && (
          <aside className="sidebar sidebar-left">
            <ErrorBoundary fallback={<PanelError />}>
              {/* Content tabs */}
              <div className="sidebar-content-tabs">
                <button
                  className={`content-tab ${sidebarTab === 'templates' ? 'active' : ''}`}
                  onClick={() => setSidebarTab('templates')}
                >
                  <span>📋</span>
                  <span>{t('tabs.templates')}</span>
                </button>
                <button
                  className={`content-tab ${sidebarTab === 'video' ? 'active' : ''}`}
                  onClick={() => setSidebarTab('video')}
                >
                  <span>🎬</span>
                  <span>{t('assets.videos')}</span>
                </button>
                <button
                  className={`content-tab ${sidebarTab === 'image' ? 'active' : ''}`}
                  onClick={() => setSidebarTab('image')}
                >
                  <span>🖼️</span>
                  <span>{t('assets.images')}</span>
                </button>
                <button
                  className={`content-tab ${sidebarTab === 'audio' ? 'active' : ''}`}
                  onClick={() => setSidebarTab('audio')}
                >
                  <span>🎵</span>
                  <span>{t('assets.audio')}</span>
                </button>
              </div>
              {/* Tab content */}
              <div className="sidebar-tab-content">
                {sidebarTab === 'templates' && <TemplatesPanel />}
                {sidebarTab !== 'templates' && <AssetsPanel filterType={sidebarTab} />}
              </div>
            </ErrorBoundary>
          </aside>
        )}

        {/* Canvas */}
        <section className="canvas-area">
          <ErrorBoundary fallback={<PanelError />}>
            <InteractiveCanvas />
          </ErrorBoundary>
        </section>

        {/* Right sidebar: Properties (when items selected) */}
        {selectedItemIds.length > 0 && (
          <aside className="sidebar sidebar-right">
            <ErrorBoundary fallback={<PanelError />}>
              <PropertiesPanel />
            </ErrorBoundary>
          </aside>
        )}
      </main>

      {/* Timeline */}
      <footer className="timeline-area">
        <ErrorBoundary fallback={<PanelError />}>
          <Timeline />
        </ErrorBoundary>
      </footer>

      <ShortcutsModal isOpen={showShortcuts} onClose={() => setShowShortcuts(false)} />

      {/* Mobile BottomSheet for properties */}
      {isMobile && !isTablet && selectedItemIds.length > 0 && (
        <BottomSheet
          isOpen={selectedItemIds.length > 0}
          onClose={() => {}}
          title={t('panels.properties')}
          height="half"
        >
          <ErrorBoundary fallback={<PanelError />}>
            <PropertiesPanel />
          </ErrorBoundary>
        </BottomSheet>
      )}
    </div>
  );
}

function EditorPage() {
  return (
    <Suspense fallback={<div className="min-h-screen bg-gray-900 flex items-center justify-center text-white">Loading Editor...</div>}>
      <EditorContent />
    </Suspense>
  );
}

export default EditorPage;
