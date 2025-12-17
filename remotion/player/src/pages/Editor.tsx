import { useEffect, useState, useCallback } from 'react';
import { useEditorStore } from '@/store/editorStore';
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

export function EditorPage() {
  const [leftTab, setLeftTab] = useState<LeftPanelTab>('assets');
  const [showShortcuts, setShowShortcuts] = useState(false);

  // Enable keyboard shortcuts
  useKeyboardShortcuts();

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
      // Ignore if focus is on input/textarea
      const target = e.target as HTMLElement;
      if (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA') {
        return;
      }

      // ? key (Shift + /) or F1
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
      {/* Header with connection status */}
      <Header
        wsStatus={isConnected ? 'connected' : 'disconnected'}
        wsClientId={clientId}
      />

      {/* Main content */}
      <main className="editor-main">
        {/* Left sidebar - Assets, Layers, Properties */}
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

        {/* Center - Canvas */}
        <section className="canvas-area">
          <InteractiveCanvas />
        </section>

        {/* Right sidebar - AI Agent Chat */}
        <aside className="sidebar sidebar-right">
          <ChatPanel
            wsConnected={isConnected}
            wsSend={send}
          />
        </aside>
      </main>

      {/* Bottom - Timeline */}
      <footer className="timeline-area">
        <Timeline />
      </footer>

      {/* Shortcuts Modal */}
      <ShortcutsModal
        isOpen={showShortcuts}
        onClose={() => setShowShortcuts(false)}
      />
    </div>
  );
}
