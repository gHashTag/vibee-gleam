import { create } from 'zustand';
import { immer } from 'zustand/middleware/immer';
import { nanoid } from 'nanoid';

// ===============================
// Types
// ===============================

export interface Message {
  id: string;
  role: 'user' | 'assistant' | 'system';
  content: string;
  timestamp: number;
  codeBlock?: {
    language: string;
    code: string;
  };
  actions?: AgentAction[];
}

export interface AgentAction {
  id: string;
  type: 'update_prop' | 'create_template' | 'modify_code' | 'apply_style';
  label: string;
  payload: Record<string, unknown>;
  status: 'pending' | 'applied' | 'rejected';
}

export interface LogEntry {
  timestamp: number;
  level: 'log' | 'warn' | 'error' | 'info';
  message: string;
}

export interface AgentContext {
  logs: LogEntry[];
  props: Record<string, unknown>;
  errors: Error[];
  currentTemplate: string;
  selectedItems: string[];
}

interface ChatStore {
  // State
  messages: Message[];
  isStreaming: boolean;
  isConnected: boolean;
  context: AgentContext;

  // Actions
  addMessage: (role: Message['role'], content: string, extras?: Partial<Message>) => string;
  updateMessage: (id: string, updates: Partial<Message>) => void;
  clearMessages: () => void;
  setStreaming: (streaming: boolean) => void;
  setConnected: (connected: boolean) => void;

  // Context management
  addLog: (level: LogEntry['level'], message: string) => void;
  addError: (error: Error) => void;
  updateContext: (updates: Partial<AgentContext>) => void;
  clearLogs: () => void;

  // Action management
  applyAction: (messageId: string, actionId: string) => void;
  rejectAction: (messageId: string, actionId: string) => void;
}

// ===============================
// Store
// ===============================

export const useChatStore = create<ChatStore>()(
  immer((set, get) => ({
    // Initial state
    messages: [
      {
        id: 'welcome',
        role: 'assistant',
        content: 'Hi! I\'m your VIBEE AI assistant. I can help you create and edit video templates. What would you like to build today?',
        timestamp: Date.now(),
      },
    ],
    isStreaming: false,
    isConnected: false,
    context: {
      logs: [],
      props: {},
      errors: [],
      currentTemplate: 'LipSyncMain',
      selectedItems: [],
    },

    // Message actions
    addMessage: (role, content, extras) => {
      const id = nanoid();
      set((state) => {
        state.messages.push({
          id,
          role,
          content,
          timestamp: Date.now(),
          ...extras,
        });
      });
      return id;
    },

    updateMessage: (id, updates) =>
      set((state) => {
        const message = state.messages.find((m) => m.id === id);
        if (message) {
          Object.assign(message, updates);
        }
      }),

    clearMessages: () =>
      set((state) => {
        state.messages = [
          {
            id: 'welcome',
            role: 'assistant',
            content: 'Chat cleared. How can I help you?',
            timestamp: Date.now(),
          },
        ];
      }),

    setStreaming: (streaming) =>
      set((state) => {
        state.isStreaming = streaming;
      }),

    setConnected: (connected) =>
      set((state) => {
        state.isConnected = connected;
      }),

    // Context management
    addLog: (level, message) =>
      set((state) => {
        state.context.logs.push({
          timestamp: Date.now(),
          level,
          message,
        });
        // Keep last 100 logs
        if (state.context.logs.length > 100) {
          state.context.logs.shift();
        }
      }),

    addError: (error) =>
      set((state) => {
        state.context.errors.push(error);
        // Keep last 10 errors
        if (state.context.errors.length > 10) {
          state.context.errors.shift();
        }
      }),

    updateContext: (updates) =>
      set((state) => {
        Object.assign(state.context, updates);
      }),

    clearLogs: () =>
      set((state) => {
        state.context.logs = [];
        state.context.errors = [];
      }),

    // Action management
    applyAction: (messageId, actionId) =>
      set((state) => {
        const message = state.messages.find((m) => m.id === messageId);
        if (message?.actions) {
          const action = message.actions.find((a) => a.id === actionId);
          if (action) {
            action.status = 'applied';
          }
        }
      }),

    rejectAction: (messageId, actionId) =>
      set((state) => {
        const message = state.messages.find((m) => m.id === messageId);
        if (message?.actions) {
          const action = message.actions.find((a) => a.id === actionId);
          if (action) {
            action.status = 'rejected';
          }
        }
      }),
  }))
);

// ===============================
// Log Capture Setup
// ===============================

let isLogCaptureInitialized = false;

export function initLogCapture() {
  if (isLogCaptureInitialized) return;
  isLogCaptureInitialized = true;

  const originalLog = console.log;
  const originalWarn = console.warn;
  const originalError = console.error;
  const originalInfo = console.info;

  console.log = (...args) => {
    originalLog.apply(console, args);
    useChatStore.getState().addLog('log', args.map(String).join(' '));
  };

  console.warn = (...args) => {
    originalWarn.apply(console, args);
    useChatStore.getState().addLog('warn', args.map(String).join(' '));
  };

  console.error = (...args) => {
    originalError.apply(console, args);
    useChatStore.getState().addLog('error', args.map(String).join(' '));
  };

  console.info = (...args) => {
    originalInfo.apply(console, args);
    useChatStore.getState().addLog('info', args.map(String).join(' '));
  };
}
