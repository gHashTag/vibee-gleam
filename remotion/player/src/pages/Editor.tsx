import { useEffect, useState, Suspense, useRef } from 'react';
import { useSetAtom, useAtomValue } from 'jotai';
import { loadCaptionsAtom, updateDurationFromLipSyncAtom, lipSyncVideoAtom, transcribeVideoAtom } from '@/atoms';
import { Header } from '@/components/Header';
import { AssetsPanel } from '@/components/Panels/AssetsPanel';
import { PropertiesPanel } from '@/components/Panels/PropertiesPanel';
import { LayersPanel } from '@/components/Panels/LayersPanel';
import { CaptionsPanel } from '@/components/Panels/CaptionsPanel';
import { ChatPanel } from '@/components/Panels/ChatPanel';
import { TemplatesPanel } from '@/components/Panels/TemplatesPanel';
import { InteractiveCanvas } from '@/components/Canvas/InteractiveCanvas';
import { Timeline } from '@/components/Timeline/Timeline';
import { ShortcutsModal } from '@/components/Modals/ShortcutsModal';
import { useKeyboardShortcuts } from '@/hooks/useKeyboard';
import { useWebSocket, setGlobalWsSend } from '@/lib/websocket';
import { Film, Layers, Settings, Subtitles, LayoutTemplate } from 'lucide-react';

type LeftPanelTab = 'templates' | 'assets' | 'layers' | 'props' | 'captions';

function EditorContent() {
  const [leftTab, setLeftTab] = useState<LeftPanelTab>('templates');
  const [showShortcuts, setShowShortcuts] = useState(false);

  // Use atoms directly instead of bridge
  const updateDurationFromLipSync = useSetAtom(updateDurationFromLipSyncAtom);
  const loadCaptions = useSetAtom(loadCaptionsAtom);
  const lipSyncVideo = useAtomValue(lipSyncVideoAtom);
  const transcribeVideo = useSetAtom(transcribeVideoAtom);
  const prevLipSyncRef = useRef<string | null>(null);

  // Enable keyboard shortcuts
  useKeyboardShortcuts();

  // Auto-detect duration from lipsync video on mount
  useEffect(() => {
    updateDurationFromLipSync();
    loadCaptions();
  }, [updateDurationFromLipSync, loadCaptions]);

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
    <div className="editor">
      <Header
        wsStatus={isConnected ? 'connected' : 'disconnected'}
        wsClientId={clientId}
      />

      <main className="editor-main">
        <aside className="sidebar sidebar-left">
          <div className="sidebar-header sidebar-tabs">
            <button
              className={`sidebar-tab ${leftTab === 'templates' ? 'active' : ''}`}
              onClick={() => setLeftTab('templates')}
              title="Templates"
            >
              <LayoutTemplate size={16} />
            </button>
            <button
              className={`sidebar-tab ${leftTab === 'assets' ? 'active' : ''}`}
              onClick={() => setLeftTab('assets')}
              title="Assets"
            >
              <Film size={16} />
            </button>
            <button
              className={`sidebar-tab ${leftTab === 'layers' ? 'active' : ''}`}
              onClick={() => setLeftTab('layers')}
              title="Layers"
            >
              <Layers size={16} />
            </button>
            <button
              className={`sidebar-tab ${leftTab === 'props' ? 'active' : ''}`}
              onClick={() => setLeftTab('props')}
              title="Properties"
            >
              <Settings size={16} />
            </button>
            <button
              className={`sidebar-tab ${leftTab === 'captions' ? 'active' : ''}`}
              onClick={() => setLeftTab('captions')}
              title="Captions"
            >
              <Subtitles size={16} />
            </button>
          </div>
          <div className="sidebar-content">
            {leftTab === 'templates' && <TemplatesPanel />}
            {leftTab === 'assets' && <AssetsPanel />}
            {leftTab === 'layers' && <LayersPanel />}
            {leftTab === 'props' && <PropertiesPanel />}
            {leftTab === 'captions' && <CaptionsPanel />}
          </div>
        </aside>

        <section className="canvas-area">
          <InteractiveCanvas />
        </section>

        <aside className="sidebar sidebar-right">
          <ChatPanel wsConnected={isConnected} wsSend={send} />
        </aside>
      </main>

      <footer className="timeline-area">
        <Timeline />
      </footer>

      <ShortcutsModal isOpen={showShortcuts} onClose={() => setShowShortcuts(false)} />
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
