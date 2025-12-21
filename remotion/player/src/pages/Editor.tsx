import { useEffect, useState, Suspense } from 'react';
import { useSetAtom } from 'jotai';
import { loadCaptionsAtom, updateDurationFromLipSyncAtom, captionsAtom } from '@/atoms';
import { Header } from '@/components/Header';
import { AssetsPanel } from '@/components/Panels/AssetsPanel';
import { PropertiesPanel } from '@/components/Panels/PropertiesPanel';
import { LayersPanel } from '@/components/Panels/LayersPanel';
import { CaptionsPanel } from '@/components/Panels/CaptionsPanel';
import { ChatPanel } from '@/components/Panels/ChatPanel';
import { InteractiveCanvas } from '@/components/Canvas/InteractiveCanvas';
import { Timeline } from '@/components/Timeline/Timeline';
import { ShortcutsModal } from '@/components/Modals/ShortcutsModal';
import { useKeyboardShortcuts } from '@/hooks/useKeyboard';
import { useWebSocket, setGlobalWsSend } from '@/lib/websocket';
import { Film, Layers, Settings, Subtitles } from 'lucide-react';

type LeftPanelTab = 'assets' | 'layers' | 'props' | 'captions';

function EditorContent() {
  const [leftTab, setLeftTab] = useState<LeftPanelTab>('assets');
  const [showShortcuts, setShowShortcuts] = useState(false);

  // Use atoms directly instead of bridge
  const updateDurationFromLipSync = useSetAtom(updateDurationFromLipSyncAtom);
  const loadCaptions = useSetAtom(loadCaptionsAtom);
  const setCaptions = useSetAtom(captionsAtom);

  // Enable keyboard shortcuts
  useKeyboardShortcuts();

  // Auto-detect duration from lipsync video and force reload captions on mount
  useEffect(() => {
    updateDurationFromLipSync();
    setCaptions([]);
    loadCaptions();
  }, [updateDurationFromLipSync, loadCaptions, setCaptions]);

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
              className={`sidebar-tab ${leftTab === 'assets' ? 'active' : ''}`}
              onClick={() => setLeftTab('assets')}
              title="Assets"
            >
              <Film size={16} />
              <span>Assets</span>
            </button>
            <button
              className={`sidebar-tab ${leftTab === 'layers' ? 'active' : ''}`}
              onClick={() => setLeftTab('layers')}
              title="Layers"
            >
              <Layers size={16} />
              <span>Layers</span>
            </button>
            <button
              className={`sidebar-tab ${leftTab === 'props' ? 'active' : ''}`}
              onClick={() => setLeftTab('props')}
              title="Properties"
            >
              <Settings size={16} />
              <span>Props</span>
            </button>
            <button
              className={`sidebar-tab ${leftTab === 'captions' ? 'active' : ''}`}
              onClick={() => setLeftTab('captions')}
              title="Captions"
            >
              <Subtitles size={16} />
              <span>Captions</span>
            </button>
          </div>
          <div className="sidebar-content">
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
