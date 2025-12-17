/**
 * Agent API - Connection to Gleam MCP Server for AI template editing
 */

import { useChatStore, type AgentAction } from '@/store/chatStore';
import { useEditorStore } from '@/store/editorStore';
import type { LipSyncMainProps } from '@/store/types';

// Agent server URL - connects to Gleam MCP server
const AGENT_WS_URL = import.meta.env.VITE_AGENT_WS_URL ||
  (window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1'
    ? 'ws://localhost:8081/agent'
    : 'wss://vibee-mcp.fly.dev/agent');

// Message types for agent communication
export interface AgentRequest {
  type: 'chat' | 'apply_action' | 'get_templates' | 'create_template';
  payload: {
    message?: string;
    actionId?: string;
    context?: AgentContext;
    templateName?: string;
    templateCode?: string;
  };
}

export interface AgentResponse {
  type: 'chat_response' | 'streaming' | 'action_result' | 'template_list' | 'error';
  payload: {
    content?: string;
    isComplete?: boolean;
    codeBlock?: {
      language: string;
      code: string;
    };
    actions?: AgentAction[];
    templates?: TemplateInfo[];
    error?: string;
  };
}

export interface AgentContext {
  logs: Array<{ timestamp: number; level: string; message: string }>;
  props: Record<string, unknown>;
  errors: string[];
  template: string;
  selectedItems: string[];
}

export interface TemplateInfo {
  id: string;
  name: string;
  description: string;
  thumbnail?: string;
  isPremium: boolean;
  priceStars?: number;
}

// Agent WebSocket connection
class AgentConnection {
  private ws: WebSocket | null = null;
  private reconnectTimeout: ReturnType<typeof setTimeout> | null = null;
  private messageId = 0;
  private pendingMessages: Map<number, {
    resolve: (response: AgentResponse) => void;
    reject: (error: Error) => void;
  }> = new Map();

  connect(): void {
    if (this.ws?.readyState === WebSocket.OPEN) return;

    console.log('[Agent] Connecting to:', AGENT_WS_URL);

    try {
      this.ws = new WebSocket(AGENT_WS_URL);

      this.ws.onopen = () => {
        console.log('[Agent] Connected');
        useChatStore.getState().setConnected(true);
      };

      this.ws.onmessage = (event) => {
        try {
          const response: AgentResponse & { id?: number } = JSON.parse(event.data);
          this.handleResponse(response);
        } catch (error) {
          console.error('[Agent] Failed to parse response:', error);
        }
      };

      this.ws.onclose = () => {
        console.log('[Agent] Disconnected');
        useChatStore.getState().setConnected(false);
        this.ws = null;

        // Reconnect after 3 seconds
        this.reconnectTimeout = setTimeout(() => this.connect(), 3000);
      };

      this.ws.onerror = (error) => {
        console.error('[Agent] WebSocket error:', error);
      };
    } catch (error) {
      console.error('[Agent] Failed to connect:', error);
      // Retry after 5 seconds
      this.reconnectTimeout = setTimeout(() => this.connect(), 5000);
    }
  }

  disconnect(): void {
    if (this.reconnectTimeout) {
      clearTimeout(this.reconnectTimeout);
    }
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  private handleResponse(response: AgentResponse & { id?: number }): void {
    // Handle streaming responses
    if (response.type === 'streaming') {
      const { content, isComplete, codeBlock, actions } = response.payload;

      // Get current message being streamed
      const messages = useChatStore.getState().messages;
      const lastMessage = messages[messages.length - 1];

      if (lastMessage?.role === 'assistant' && !isComplete) {
        // Update existing message with streamed content
        useChatStore.getState().updateMessage(lastMessage.id, {
          content: (lastMessage.content || '') + (content || ''),
          codeBlock,
          actions,
        });
      } else if (content) {
        // Add new assistant message
        useChatStore.getState().addMessage('assistant', content, {
          codeBlock,
          actions,
        });
      }

      if (isComplete) {
        useChatStore.getState().setStreaming(false);
      }
      return;
    }

    // Handle chat response (non-streaming)
    if (response.type === 'chat_response') {
      const { content, codeBlock, actions } = response.payload;

      if (content) {
        useChatStore.getState().addMessage('assistant', content, {
          codeBlock,
          actions,
        });
      }
      useChatStore.getState().setStreaming(false);
      return;
    }

    // Handle action results
    if (response.type === 'action_result') {
      const { content, error } = response.payload;
      if (error) {
        useChatStore.getState().addMessage('system', `Action failed: ${error}`);
      } else if (content) {
        useChatStore.getState().addMessage('assistant', content);
      }
      return;
    }

    // Handle errors
    if (response.type === 'error') {
      const { error } = response.payload;
      useChatStore.getState().addMessage('system', `Error: ${error}`);
      useChatStore.getState().setStreaming(false);
      return;
    }

    // Handle pending message responses
    if (response.id && this.pendingMessages.has(response.id)) {
      const { resolve } = this.pendingMessages.get(response.id)!;
      this.pendingMessages.delete(response.id);
      resolve(response);
    }
  }

  async send(request: AgentRequest): Promise<void> {
    if (!this.ws || this.ws.readyState !== WebSocket.OPEN) {
      throw new Error('Not connected to agent server');
    }

    const id = ++this.messageId;
    this.ws.send(JSON.stringify({ ...request, id }));
  }

  isConnected(): boolean {
    return this.ws?.readyState === WebSocket.OPEN;
  }
}

// Singleton instance
const agentConnection = new AgentConnection();

// Initialize connection
export function initAgentConnection(): void {
  agentConnection.connect();
}

// Disconnect
export function disconnectAgent(): void {
  agentConnection.disconnect();
}

// Check connection
export function isAgentConnected(): boolean {
  return agentConnection.isConnected();
}

// Send chat message to agent
export async function sendToAgent(message: string): Promise<void> {
  const chatStore = useChatStore.getState();
  const editorStore = useEditorStore.getState();

  // Build context
  const context: AgentContext = {
    logs: chatStore.context.logs.slice(-50),
    props: editorStore.templateProps as unknown as Record<string, unknown>,
    errors: chatStore.context.errors.map(e => e.message),
    template: chatStore.context.currentTemplate,
    selectedItems: editorStore.selectedItemIds,
  };

  await agentConnection.send({
    type: 'chat',
    payload: { message, context },
  });
}

// Apply agent action
export async function applyAgentAction(action: AgentAction): Promise<void> {
  const editorStore = useEditorStore.getState();

  switch (action.type) {
    case 'update_prop': {
      const { key, value } = action.payload as { key: keyof LipSyncMainProps; value: unknown };
      editorStore.updateTemplateProp(key, value as LipSyncMainProps[typeof key]);
      break;
    }

    case 'apply_style': {
      const updates = action.payload as Partial<LipSyncMainProps>;
      Object.entries(updates).forEach(([key, value]) => {
        editorStore.updateTemplateProp(
          key as keyof LipSyncMainProps,
          value as LipSyncMainProps[keyof LipSyncMainProps]
        );
      });
      break;
    }

    case 'create_template':
    case 'modify_code': {
      // Send to server for file operations
      await agentConnection.send({
        type: action.type === 'create_template' ? 'create_template' : 'apply_action',
        payload: {
          actionId: action.id,
          templateName: action.payload.name as string,
          templateCode: action.payload.code as string,
        },
      });
      break;
    }

    default:
      console.warn('[Agent] Unknown action type:', action.type);
  }
}

// Get available templates
export async function getTemplates(): Promise<void> {
  await agentConnection.send({
    type: 'get_templates',
    payload: {},
  });
}

export { agentConnection };
