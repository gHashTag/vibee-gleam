import { useEffect, useRef, useCallback, useState } from 'react';
import { useEditorStore } from '@/store/editorStore';
import type { Asset, TrackItem, LipSyncMainProps } from '@/store/types';

// WebSocket server URL (same as render server)
// DISABLED on production - render server can't handle many WS connections
const WS_URL = import.meta.env.VITE_WS_URL ||
  (window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1'
    ? `${window.location.protocol === 'https:' ? 'wss:' : 'ws:'}//${import.meta.env.VITE_RENDER_SERVER_URL?.replace(/^https?:\/\//, '') || 'localhost:3333'}`
    : null); // Disable WebSocket on production for now

// Message types for real-time sync
export type WSMessageType =
  | { type: 'connected'; payload: { clientId: string } }
  | { type: 'asset_added'; payload: Asset }
  | { type: 'asset_removed'; payload: { id: string } }
  | { type: 'item_added'; payload: { trackId: string; item: TrackItem } }
  | { type: 'item_updated'; payload: { id: string; updates: Partial<TrackItem> } }
  | { type: 'item_deleted'; payload: { ids: string[] } }
  | { type: 'props_changed'; payload: Partial<LipSyncMainProps> }
  | { type: 'frame_changed'; payload: { frame: number } }
  | { type: 'playback_changed'; payload: { isPlaying: boolean } }
  // Agent messages (forwarded from MCP server)
  | { type: 'agent_response'; payload: AgentResponsePayload }
  | { type: 'agent_streaming'; payload: { content: string; isComplete: boolean } }
  | { type: 'agent_error'; payload: { error: string } };

// Agent response payload type
export interface AgentResponsePayload {
  content?: string;
  codeBlock?: { language: string; code: string };
  actions?: Array<{
    id: string;
    type: string;
    label: string;
    payload: Record<string, unknown>;
    status: 'pending' | 'applied' | 'rejected';
  }>;
}

interface UseWebSocketOptions {
  onConnect?: () => void;
  onDisconnect?: () => void;
  onMessage?: (msg: WSMessageType) => void;
}

interface UseWebSocketReturn {
  send: (message: Omit<WSMessageType, 'connected'>) => void;
  isConnected: boolean;
  clientId: string | null;
}

export function useWebSocket(options: UseWebSocketOptions = {}): UseWebSocketReturn {
  const wsRef = useRef<WebSocket | null>(null);
  const reconnectTimeoutRef = useRef<ReturnType<typeof setTimeout> | undefined>(undefined);
  const [isConnected, setIsConnected] = useState(false);
  const [clientId, setClientId] = useState<string | null>(null);


  const connect = useCallback(() => {
    // Skip WebSocket on production (disabled)
    if (!WS_URL) {
      console.log('[WS] Disabled on production');
      return null;
    }

    // Parse WS URL - handle various formats
    let wsUrl = WS_URL;
    if (!wsUrl.startsWith('ws://') && !wsUrl.startsWith('wss://')) {
      wsUrl = `ws://${wsUrl}`;
    }

    console.log('[WS] Connecting to:', wsUrl);

    try {
      const ws = new WebSocket(wsUrl);

      ws.onopen = () => {
        console.log('[WS] Connected');
        wsRef.current = ws;
        setIsConnected(true);
        options.onConnect?.();
      };

      ws.onmessage = (event) => {
        try {
          const msg: WSMessageType = JSON.parse(event.data);
          handleMessage(msg);
          options.onMessage?.(msg);
        } catch (error) {
          console.error('[WS] Failed to parse message:', error);
        }
      };

      ws.onclose = () => {
        console.log('[WS] Disconnected');
        wsRef.current = null;
        setIsConnected(false);
        setClientId(null);
        options.onDisconnect?.();

        // Reconnect after 2 seconds (only if enabled)
        if (WS_URL) {
          reconnectTimeoutRef.current = setTimeout(connect, 2000);
        }
      };

      ws.onerror = (error) => {
        console.error('[WS] Error:', error);
      };

      return ws;
    } catch (error) {
      console.error('[WS] Failed to connect:', error);
      // Retry connection (only if enabled)
      if (WS_URL) {
        reconnectTimeoutRef.current = setTimeout(connect, 2000);
      }
      return null;
    }
  }, [options]);

  const handleMessage = useCallback((msg: WSMessageType) => {
    switch (msg.type) {
      case 'connected':
        setClientId(msg.payload.clientId);
        console.log('[WS] Assigned client ID:', msg.payload.clientId);
        break;

      case 'asset_added':
        console.log('[WS] Remote asset added:', msg.payload.name);
        // Add asset from remote client - use getState/setState to avoid loops
        useEditorStore.setState((state) => ({
          assets: [...state.assets, msg.payload],
        }));
        break;

      case 'asset_removed':
        console.log('[WS] Remote asset removed:', msg.payload.id);
        useEditorStore.setState((state) => ({
          assets: state.assets.filter((a) => a.id !== msg.payload.id),
        }));
        break;

      case 'item_added':
        console.log('[WS] Remote item added to track:', msg.payload.trackId);
        useEditorStore.setState((state) => ({
          tracks: state.tracks.map((track) =>
            track.id === msg.payload.trackId
              ? { ...track, items: [...track.items, msg.payload.item] }
              : track
          ),
        }));
        break;

      case 'item_updated':
        console.log('[WS] Remote item updated:', msg.payload.id);
        useEditorStore.setState((state) => ({
          tracks: state.tracks.map((track) => ({
            ...track,
            items: track.items.map((item) =>
              item.id === msg.payload.id
                ? { ...item, ...msg.payload.updates }
                : item
            ),
          })),
        }));
        break;

      case 'item_deleted':
        console.log('[WS] Remote items deleted:', msg.payload.ids);
        useEditorStore.setState((state) => ({
          tracks: state.tracks.map((track) => ({
            ...track,
            items: track.items.filter((item) => !msg.payload.ids.includes(item.id)),
          })),
          selectedItemIds: state.selectedItemIds.filter(
            (id) => !msg.payload.ids.includes(id)
          ),
        }));
        break;

      case 'props_changed':
        console.log('[WS] Remote props changed');
        useEditorStore.setState((state) => ({
          templateProps: { ...state.templateProps, ...msg.payload },
        }));
        break;

      case 'frame_changed':
        // Only sync frame if not playing (to avoid jitter)
        if (!useEditorStore.getState().isPlaying) {
          useEditorStore.setState({ currentFrame: msg.payload.frame });
        }
        break;

      case 'playback_changed':
        useEditorStore.setState({ isPlaying: msg.payload.isPlaying });
        break;
    }
  }, []);

  const send = useCallback((message: Omit<WSMessageType, 'connected'>) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.send(JSON.stringify(message));
    } else {
      console.warn('[WS] Cannot send - not connected');
    }
  }, []);

  // Connect on mount, disconnect on unmount
  useEffect(() => {
    const ws = connect();

    return () => {
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current);
      }
      if (ws) {
        ws.close();
      }
    };
  }, [connect]);

  return { send, isConnected, clientId };
}

// Singleton for broadcasting from store actions
let globalWsSend: ((msg: Omit<WSMessageType, 'connected'>) => void) | null = null;

export function setGlobalWsSend(send: (msg: Omit<WSMessageType, 'connected'>) => void) {
  globalWsSend = send;
}

export function getGlobalWsSend() {
  return globalWsSend;
}

// Helper to broadcast from anywhere
export function broadcastAssetAdded(asset: Asset) {
  globalWsSend?.({ type: 'asset_added', payload: asset });
}

export function broadcastAssetRemoved(id: string) {
  globalWsSend?.({ type: 'asset_removed', payload: { id } });
}

export function broadcastItemAdded(trackId: string, item: TrackItem) {
  globalWsSend?.({ type: 'item_added', payload: { trackId, item } });
}

export function broadcastItemUpdated(id: string, updates: Partial<TrackItem>) {
  globalWsSend?.({ type: 'item_updated', payload: { id, updates } });
}

export function broadcastItemDeleted(ids: string[]) {
  globalWsSend?.({ type: 'item_deleted', payload: { ids } });
}

export function broadcastPropsChanged(props: Partial<LipSyncMainProps>) {
  globalWsSend?.({ type: 'props_changed', payload: props });
}
