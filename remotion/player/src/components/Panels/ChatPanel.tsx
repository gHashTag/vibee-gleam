import { useState, useRef, useEffect, useCallback } from 'react';
import { useChatStore, initLogCapture } from '@/store/chatStore';
import { useAtomValue } from 'jotai';
import { templatePropsAtom, selectedItemIdsAtom } from '@/atoms';
import {
  initAgentConnection,
  disconnectAgent,
  sendToAgent,
  applyAgentAction,
  isAgentConnected,
} from '@/lib/agentApi';
import {
  Bot,
  Send,
  Trash2,
  Loader2,
  CheckCircle,
  XCircle,
  Code,
  FileText,
  AlertCircle,
  Wifi,
  WifiOff,
  Sparkles,
  MessageSquare,
  Terminal,
} from 'lucide-react';
import { LogViewer } from '../Debug/LogViewer';
import './ChatPanel.css';

interface ChatPanelProps {
  wsConnected?: boolean;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  wsSend?: (data: any) => void;
}

type Tab = 'chat' | 'logs';

export function ChatPanel({ wsConnected, wsSend }: ChatPanelProps) {
  const [input, setInput] = useState('');
  const [activeTab, setActiveTab] = useState<Tab>('chat');
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLTextAreaElement>(null);

  // Chat store
  const messages = useChatStore((s) => s.messages);
  const isStreaming = useChatStore((s) => s.isStreaming);
  const agentConnected = useChatStore((s) => s.isConnected);
  const context = useChatStore((s) => s.context);
  const addMessage = useChatStore((s) => s.addMessage);
  const clearMessages = useChatStore((s) => s.clearMessages);
  const setStreaming = useChatStore((s) => s.setStreaming);
  const applyAction = useChatStore((s) => s.applyAction);
  const rejectAction = useChatStore((s) => s.rejectAction);

  // Editor store for context
  const templateProps = useAtomValue(templatePropsAtom);
  const selectedItemIds = useAtomValue(selectedItemIdsAtom);

  // Initialize log capture and agent connection
  useEffect(() => {
    initLogCapture();
    initAgentConnection();

    return () => {
      disconnectAgent();
    };
  }, []);

  // Auto-scroll to bottom
  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [messages]);

  // Update context when props change
  useEffect(() => {
    useChatStore.getState().updateContext({
      props: templateProps as unknown as Record<string, unknown>,
      selectedItems: selectedItemIds,
    });
  }, [templateProps, selectedItemIds]);

  const handleSend = useCallback(async () => {
    const text = input.trim();
    if (!text || isStreaming) return;

    // Add user message
    addMessage('user', text);
    setInput('');
    setStreaming(true);

    try {
      // Send to Agent API if connected
      if (agentConnected || isAgentConnected()) {
        await sendToAgent(text);
      } else if (wsConnected && wsSend) {
        // Fallback to WebSocket if agent not connected but WS is
        wsSend({
          type: 'agent_chat',
          payload: {
            message: text,
            context: {
              logs: context.logs.slice(-50),
              props: templateProps,
              errors: context.errors.map((e) => e.message),
              template: context.currentTemplate,
              selectedItems: selectedItemIds,
            },
          },
        });
      } else {
        // No connection - show helpful message
        setTimeout(() => {
          addMessage(
            'assistant',
            'I\'m currently offline. The AI server will connect automatically when available. In the meantime, you can explore the template properties in the left panel.',
          );
          setStreaming(false);
        }, 500);
      }
    } catch (error) {
      console.error('[ChatPanel] Send error:', error);
      addMessage('system', `Error: ${error instanceof Error ? error.message : 'Unknown error'}`);
      setStreaming(false);
    }
  }, [input, isStreaming, agentConnected, wsConnected, wsSend, context, templateProps, selectedItemIds, addMessage, setStreaming]);

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      handleSend();
    }
  };

  const handleApplyAction = async (messageId: string, actionId: string) => {
    // Find the action
    const message = messages.find((m) => m.id === messageId);
    const action = message?.actions?.find((a) => a.id === actionId);

    if (action) {
      try {
        // Apply the action via Agent API
        await applyAgentAction(action);
        // Mark as applied in store
        applyAction(messageId, actionId);
        addMessage('system', `Applied: ${action.label}`);
      } catch (error) {
        console.error('[ChatPanel] Apply action error:', error);
        addMessage('system', `Failed to apply action: ${error instanceof Error ? error.message : 'Unknown error'}`);
      }
    }
  };

  const handleRejectAction = (messageId: string, actionId: string) => {
    rejectAction(messageId, actionId);
  };

  // Connection status - prefer agent connection over WS
  const isConnected = agentConnected || wsConnected;

  // Context counts
  const logCount = context.logs.length;
  const errorCount = context.errors.length;
  const propsCount = Object.keys(templateProps).length;

  return (
    <div className="chat-panel">
      {/* Header with tabs */}
      <div className="chat-header">
        <div className="chat-header-left">
          <div className="chat-avatar">
            <Sparkles size={18} />
          </div>
          <div className="chat-header-info">
            <span className="chat-title">VIBEE Agent</span>
            <span className={`chat-status ${isConnected ? 'connected' : 'disconnected'}`}>
              {isConnected ? (
                <>
                  <Wifi size={10} /> Connected
                </>
              ) : (
                <>
                  <WifiOff size={10} /> Offline
                </>
              )}
            </span>
          </div>
        </div>
        <button className="chat-clear-btn" onClick={clearMessages} title="Clear chat">
          <Trash2 size={14} />
        </button>
      </div>

      {/* Tabs */}
      <div className="chat-tabs">
        <button
          className={`chat-tab ${activeTab === 'chat' ? 'active' : ''}`}
          onClick={() => setActiveTab('chat')}
        >
          <MessageSquare size={14} />
          <span>Chat</span>
        </button>
        <button
          className={`chat-tab ${activeTab === 'logs' ? 'active' : ''}`}
          onClick={() => setActiveTab('logs')}
        >
          <Terminal size={14} />
          <span>Logs</span>
        </button>
      </div>

      {/* Context badges - only show in chat tab */}
      {activeTab === 'chat' && (
        <div className="chat-context">
          <div className="context-badge" title={`${logCount} captured logs`}>
            <FileText size={12} />
            <span>{logCount} logs</span>
          </div>
          <div className="context-badge" title={`${propsCount} template properties`}>
            <Code size={12} />
            <span>{propsCount} props</span>
          </div>
          {errorCount > 0 && (
            <div className="context-badge error" title={`${errorCount} errors`}>
              <AlertCircle size={12} />
              <span>{errorCount} errors</span>
            </div>
          )}
        </div>
      )}

      {/* Chat Tab Content */}
      {activeTab === 'chat' && (
        <>
          {/* Messages */}
          <div className="chat-messages">
            {messages.map((message) => (
              <div key={message.id} className={`chat-message ${message.role}`}>
                {message.role === 'assistant' && (
                  <div className="message-avatar">
                    <Bot size={14} />
                  </div>
                )}
                <div className="message-content">
                  <p className="message-text">{message.content}</p>

                  {/* Code block */}
                  {message.codeBlock && (
                    <div className="message-code">
                      <div className="code-header">
                        <span>{message.codeBlock.language}</span>
                      </div>
                      <pre>
                        <code>{message.codeBlock.code}</code>
                      </pre>
                    </div>
                  )}

                  {/* Actions */}
                  {message.actions && message.actions.length > 0 && (
                    <div className="message-actions">
                      {message.actions.map((action) => (
                        <div key={action.id} className={`action-item ${action.status}`}>
                          <span className="action-label">{action.label}</span>
                          {action.status === 'pending' && (
                            <div className="action-buttons">
                              <button
                                className="action-btn apply"
                                onClick={() => handleApplyAction(message.id, action.id)}
                              >
                                <CheckCircle size={14} />
                              </button>
                              <button
                                className="action-btn reject"
                                onClick={() => handleRejectAction(message.id, action.id)}
                              >
                                <XCircle size={14} />
                              </button>
                            </div>
                          )}
                          {action.status === 'applied' && (
                            <CheckCircle size={14} className="action-status applied" />
                          )}
                          {action.status === 'rejected' && (
                            <XCircle size={14} className="action-status rejected" />
                          )}
                        </div>
                      ))}
                    </div>
                  )}
                </div>
              </div>
            ))}

            {/* Streaming indicator */}
            {isStreaming && (
              <div className="chat-message assistant streaming">
                <div className="message-avatar">
                  <Bot size={14} />
                </div>
                <div className="message-content">
                  <Loader2 size={16} className="spin" />
                </div>
              </div>
            )}

            <div ref={messagesEndRef} />
          </div>

          {/* Input */}
          <div className="chat-input-container">
            <textarea
              ref={inputRef}
              className="chat-input"
              placeholder="Describe what you want to create..."
              value={input}
              onChange={(e) => setInput(e.target.value)}
              onKeyDown={handleKeyDown}
              rows={1}
              disabled={isStreaming}
            />
            <button
              className="chat-send-btn"
              onClick={handleSend}
              disabled={!input.trim() || isStreaming}
            >
              {isStreaming ? <Loader2 size={18} className="spin" /> : <Send size={18} />}
            </button>
          </div>
        </>
      )}

      {/* Logs Tab Content */}
      {activeTab === 'logs' && (
        <div className="chat-logs-container">
          <LogViewer />
        </div>
      )}
    </div>
  );
}
