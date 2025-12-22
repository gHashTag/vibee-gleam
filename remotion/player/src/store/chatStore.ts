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
  type:
    | 'update_prop'
    | 'apply_style'
    | 'create_template'
    | 'modify_code'
    // Timeline actions
    | 'add_track_item'
    | 'update_track_item'
    | 'delete_track_items'
    | 'select_items';
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
// Localization helpers
// ===============================

function getBrowserLanguage(): string {
  if (typeof navigator === 'undefined') return 'en';
  const lang = navigator.language || (navigator as { userLanguage?: string }).userLanguage || 'en';
  return lang.toLowerCase().split('-')[0];
}

function getWelcomeMessage(): string {
  const lang = getBrowserLanguage();
  switch (lang) {
    case 'ru':
      return 'Привет! Я VIBEE AI ассистент. Могу помочь создать и отредактировать видео шаблоны. Что хотите сделать?';
    case 'uk':
      return 'Привіт! Я VIBEE AI асистент. Можу допомогти створити та відредагувати відео шаблони. Що хочете зробити?';
    case 'es':
      return '¡Hola! Soy tu asistente VIBEE AI. Puedo ayudarte a crear y editar plantillas de video. ¿Qué te gustaría hacer?';
    case 'de':
      return 'Hallo! Ich bin dein VIBEE AI Assistent. Ich kann dir helfen, Video-Vorlagen zu erstellen und zu bearbeiten. Was möchtest du machen?';
    case 'fr':
      return 'Salut! Je suis ton assistant VIBEE AI. Je peux t\'aider à créer et modifier des modèles vidéo. Que veux-tu faire?';
    case 'zh':
      return '你好！我是你的 VIBEE AI 助手。我可以帮你创建和编辑视频模板。你想做什么？';
    case 'ja':
      return 'こんにちは！VIBEE AIアシスタントです。動画テンプレートの作成・編集をお手伝いします。何をしましょうか？';
    case 'ko':
      return '안녕하세요! VIBEE AI 어시스턴트입니다. 비디오 템플릿 제작과 편집을 도와드릴 수 있어요. 무엇을 하시겠어요?';
    default:
      return 'Hi! I\'m your VIBEE AI assistant. I can help you create and edit video templates. What would you like to build today?';
  }
}

function getClearMessage(): string {
  const lang = getBrowserLanguage();
  switch (lang) {
    case 'ru':
      return 'Чат очищен. Чем могу помочь?';
    case 'uk':
      return 'Чат очищено. Чим можу допомогти?';
    case 'es':
      return 'Chat borrado. ¿Cómo puedo ayudarte?';
    case 'de':
      return 'Chat gelöscht. Wie kann ich dir helfen?';
    case 'fr':
      return 'Chat effacé. Comment puis-je t\'aider?';
    case 'zh':
      return '聊天已清空。有什么可以帮你的？';
    case 'ja':
      return 'チャットをクリアしました。何かお手伝いできますか？';
    case 'ko':
      return '채팅이 삭제되었습니다. 무엇을 도와드릴까요?';
    default:
      return 'Chat cleared. How can I help you?';
  }
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
        content: getWelcomeMessage(),
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
            content: getClearMessage(),
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
