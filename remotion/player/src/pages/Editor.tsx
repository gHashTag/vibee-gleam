import { useEffect, useState, Suspense, useRef, useCallback, useMemo } from 'react';
import { useSetAtom, useAtomValue, useAtom } from 'jotai';
import { loadCaptionsAtom, updateDurationFromLipSyncAtom, lipSyncVideoAtom, transcribeVideoAtom, ensureAudioTrackAtom, ensureImageTrackAtom, selectedItemIdsAtom, sidebarTabAtom, type SidebarTab } from '@/atoms';
import { useAutoRecordHistory } from '@/atoms/hooks';
import { Header } from '@/components/Header';
import { AssetsPanel } from '@/components/Panels/AssetsPanel';
import { PropertiesPanel } from '@/components/Panels/PropertiesPanel';
import { TemplatesPanel } from '@/components/Panels/TemplatesPanel';
import { GeneratePanel, type GenerateTab } from '@/components/Panels/GeneratePanel';
import { FeedPanel } from '@/components/Panels/FeedPanel';
import { InteractiveCanvas } from '@/components/Canvas/InteractiveCanvas';
import { ResultsGallery } from '@/components/Results/ResultsGallery';
import { Timeline } from '@/components/Timeline/Timeline';
import { ShortcutsModal } from '@/components/Modals/ShortcutsModal';
import { BottomSheet } from '@/components/BottomSheet/BottomSheet';
import { ErrorBoundary } from '@/components/ErrorBoundary';
import { PanelError } from '@/components/Panels/PanelError';
import { useKeyboardShortcuts } from '@/hooks/useKeyboard';
import { useIsMobile, useIsTablet } from '@/hooks/useMediaQuery';
import { useWebSocket, setGlobalWsSend } from '@/lib/websocket';
import { useLanguage } from '@/hooks/useLanguage';

// Unified sidebar tabs configuration (8 tabs - vertical layout)
const SIDEBAR_TABS: { id: SidebarTab; emoji: string; labelKey: string }[] = [
  { id: 'feed', emoji: '🌐', labelKey: 'tabs.feed' },
  { id: 'templates', emoji: '📋', labelKey: 'tabs.templates' },
  { id: 'player', emoji: '▶️', labelKey: 'tabs.player' },
  { id: 'lipsync', emoji: '👄', labelKey: 'tabs.avatar' },
  { id: 'video', emoji: '🎬', labelKey: 'generate.video' },
  { id: 'image', emoji: '📷', labelKey: 'generate.image' },
  { id: 'audio', emoji: '🎤', labelKey: 'generate.audio' },
];

function EditorContent() {
  const { t } = useLanguage();
  const [sidebarTab, setSidebarTab] = useAtom(sidebarTabAtom);
  const [showShortcuts, setShowShortcuts] = useState(false);
  const [mobileSheetOpen, setMobileSheetOpen] = useState(false);
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
  const prevLipSyncRef = useRef<string | null>(null);

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

  // Handle tab click - open BottomSheet on mobile (not tablet)
  const handleTabClick = useCallback((tabId: SidebarTab) => {
    setSidebarTab(tabId);
    // Open BottomSheet only on mobile (not tablet) for tabs that have panels
    if (isMobile && !isTablet && tabId !== 'feed') {
      setMobileSheetOpen(true);
    }
  }, [isMobile, isTablet, setSidebarTab]);

  // Get title for BottomSheet based on current tab (memoized)
  const sheetTitle = useMemo(() => {
    const tab = SIDEBAR_TABS.find(t => t.id === sidebarTab);
    return tab ? t(tab.labelKey) : '';
  }, [sidebarTab, t]);

  return (
    <div className="editor">
      <Header
        wsStatus={isConnected ? 'connected' : 'disconnected'}
        wsClientId={clientId}
      />

      <main id="main-content" className="editor-main" role="main">
        {/* Vertical tabs - separate column (bottom nav on mobile) */}
        <nav className="vertical-tabs">
          {SIDEBAR_TABS.map((tab) => (
            <button
              key={tab.id}
              className={`vertical-tab ${sidebarTab === tab.id ? 'active' : ''}`}
              onClick={() => handleTabClick(tab.id)}
              title={t(tab.labelKey)}
            >
              <span className="tab-emoji">{tab.emoji}</span>
              <span className="tab-label">{t(tab.labelKey)}</span>
            </button>
          ))}
        </nav>

        {/* Hide sidebar when Feed is active on desktop - Feed goes fullscreen */}
        {/* On tablet: always show sidebar with AssetsPanel */}
        {(isTablet || sidebarTab !== 'feed') && (
          <aside className="sidebar sidebar-left">
            <div className="sidebar-content">
              <ErrorBoundary fallback={<PanelError />}>
                {isTablet ? (
                  /* Tablet: always show AssetsPanel */
                  <AssetsPanel />
                ) : (
                  /* Desktop: show based on active tab */
                  <>
                    {sidebarTab === 'templates' && <TemplatesPanel />}
                    {sidebarTab === 'player' && <AssetsPanel />}
                    {['image', 'video', 'audio', 'lipsync'].includes(sidebarTab) && (
                      <GeneratePanel activeTab={sidebarTab as GenerateTab} />
                    )}
                  </>
                )}
              </ErrorBoundary>
            </div>
          </aside>
        )}

        <section className="canvas-area">
          <ErrorBoundary fallback={<PanelError />}>
            {isTablet ? (
              /* Tablet: always show Canvas */
              <InteractiveCanvas />
            ) : sidebarTab === 'feed' ? (
              <FeedPanel fullscreen />
            ) : ['templates', 'player'].includes(sidebarTab) ? (
              <InteractiveCanvas />
            ) : (
              <ResultsGallery tab={sidebarTab as 'image' | 'video' | 'audio' | 'lipsync'} />
            )}
          </ErrorBoundary>
        </section>

        {selectedItemIds.length > 0 && (
          <aside className="sidebar sidebar-right">
            <ErrorBoundary fallback={<PanelError />}>
              <PropertiesPanel />
            </ErrorBoundary>
          </aside>
        )}
      </main>

      {/* Hide timeline when Feed is active (not on tablet) */}
      {(isTablet || sidebarTab !== 'feed') && (
        <footer className="timeline-area">
          <ErrorBoundary fallback={<PanelError />}>
            <Timeline />
          </ErrorBoundary>
        </footer>
      )}

      <ShortcutsModal isOpen={showShortcuts} onClose={() => setShowShortcuts(false)} />

      {/* Mobile BottomSheet for panels (not on tablet) */}
      {isMobile && !isTablet && (
        <BottomSheet
          isOpen={mobileSheetOpen}
          onClose={() => setMobileSheetOpen(false)}
          title={sheetTitle}
          height="half"
        >
          <ErrorBoundary fallback={<PanelError />}>
            {sidebarTab === 'templates' && <TemplatesPanel />}
            {sidebarTab === 'player' && <AssetsPanel />}
            {['image', 'video', 'audio', 'lipsync'].includes(sidebarTab) && (
              <GeneratePanel activeTab={sidebarTab as GenerateTab} />
            )}
          </ErrorBoundary>
        </BottomSheet>
      )}

      {/* Mobile BottomSheet for properties (not on tablet) */}
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

export function EditorPage() {
  return (
    <Suspense fallback={<div className="min-h-screen bg-gray-900 flex items-center justify-center text-white">Loading Editor...</div>}>
      <EditorContent />
    </Suspense>
  );
}
