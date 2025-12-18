import { useState, useEffect, useRef } from 'react';
import './Chat.css';

interface Message {
  text: string;
  isUser: boolean;
  timestamp: number;
}

interface UploadedFiles {
  video: File | null;
  photo: File | null;
  audio: File | null;
}

export function ChatPage() {
  const [messages, setMessages] = useState<Message[]>([]);
  const [input, setInput] = useState('');
  const [suggestions, setSuggestions] = useState<string[]>([]);
  const [uploadedFiles, setUploadedFiles] = useState<UploadedFiles>({
    video: null,
    photo: null,
    audio: null,
  });
  const [isTyping, setIsTyping] = useState(false);
  const [progress, setProgress] = useState<{ text: string; percent: number } | null>(null);
  const chatContainerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    // Initial message
    addAgentMessage(
      "Привет! Я помогу создать крутой рилс. Что будем делать?\n\n" +
      "1. 🎤 Talking head с аватаром\n" +
      "2. ✂️ Split-screen стиль\n" +
      "3. 🏭 Массовая генерация вариаций\n" +
      "4. 📝 Текстовая анимация\n" +
      "5. 🎬 Брендовое интро\n\n" +
      "🔥 НОВИНКА: Клонирование рилса!\n" +
      "6. 📹 Клонировать существующий рилс (отправь видео)"
    );
    
    setSuggestions([
      "Talking head",
      "Split-screen",
      "Клонировать рилс",
      "Вариации"
    ]);
  }, []);

  useEffect(() => {
    if (chatContainerRef.current) {
      chatContainerRef.current.scrollTop = chatContainerRef.current.scrollHeight;
    }
  }, [messages]);

  const addAgentMessage = (text: string) => {
    setMessages(prev => [...prev, { text, isUser: false, timestamp: Date.now() }]);
  };

  const addUserMessage = (text: string) => {
    setMessages(prev => [...prev, { text, isUser: true, timestamp: Date.now() }]);
  };

  const handleSend = async () => {
    if (!input.trim() && !uploadedFiles.video && !uploadedFiles.photo) return;

    if (input.trim()) {
      addUserMessage(input);
      setInput('');
    }

    setIsTyping(true);
    setSuggestions([]);

    // Simulate AI response
    setTimeout(() => {
      setIsTyping(false);
      
      const lowerInput = input.toLowerCase();
      
      if (lowerInput.includes('клон') || uploadedFiles.video) {
        handleCloningFlow();
      } else if (lowerInput.includes('talking') || lowerInput.includes('аватар')) {
        handleTalkingHead();
      } else if (lowerInput.includes('split')) {
        handleSplitScreen();
      } else {
        addAgentMessage("Выбери один из вариантов выше или опиши подробнее!");
        setSuggestions(["Talking head", "Split-screen", "Клонировать рилс"]);
      }
    }, 1000);
  };

  const handleCloningFlow = () => {
    if (!uploadedFiles.video) {
      addAgentMessage("Отлично! Для клонирования загрузи исходное видео (нажми 🎥)");
      return;
    }

    addAgentMessage(
      "🔍 Анализирую видео через Gemini 3 Pro Preview...\n\n" +
      "Это займет 10-15 секунд."
    );

    setTimeout(() => {
      addAgentMessage(
        "✅ Анализ завершен!\n\n" +
        "📊 Что я нашел:\n" +
        "- Layout: Circle overlay (аватар внизу слева)\n" +
        "- Размер аватара: 25% от ширины\n" +
        "- Эффекты: glassmorphism, vignette (0.7)\n" +
        "- Цвета: #1a1a2e, #e94560\n\n" +
        "Для клонирования мне нужны:\n" +
        "🔴 Фото твоего лица (нажми 📷)\n" +
        "🔴 Текст для озвучки (напиши здесь)"
      );
      setSuggestions(["Загрузить фото", "Написать текст"]);
    }, 2000);
  };

  const handleTalkingHead = () => {
    addAgentMessage(
      "🎤 Отлично! Выбрана композиция: Talking Head с круглым аватаром.\n\n" +
      "Теперь настроим параметры:\n" +
      "- Видео с аватаром?\n" +
      "- Фоновые видео (B-roll)?\n" +
      "- Музыка?"
    );
    setSuggestions(["Загрузить видео", "Использовать примеры"]);
  };

  const handleSplitScreen = () => {
    addAgentMessage(
      "✂️ Отлично! Выбрана композиция: Split-screen стиль.\n\n" +
      "Экран делится 50/50 с желтыми субтитрами.\n\n" +
      "Что у тебя есть?"
    );
    setSuggestions(["Загрузить видео", "Настроить параметры"]);
  };

  const handleFileUpload = (file: File, type: keyof UploadedFiles) => {
    setUploadedFiles(prev => ({ ...prev, [type]: file }));
    
    const icons = { video: '🎥', photo: '📷', audio: '🎵' };
    addUserMessage(`Загружен файл: ${icons[type]} ${file.name}`);

    if (type === 'video') {
      setTimeout(() => handleSend(), 500);
    }
  };

  const handleSuggestionClick = (suggestion: string) => {
    setInput(suggestion);
    setTimeout(() => handleSend(), 100);
  };

  return (
    <div className="chat-page">
      <div className="chat-header">
        <h1>🎬 VIBEE Reels Creator</h1>
        <p>AI-powered Instagram Reels клонирование</p>
      </div>

      <div className="chat-container" ref={chatContainerRef}>
        {messages.map((msg, idx) => (
          <div key={idx} className={`message ${msg.isUser ? 'user' : 'agent'}`}>
            <div className="message-content">
              {msg.text.split('\n').map((line, i) => (
                <span key={i}>
                  {line}
                  <br />
                </span>
              ))}
            </div>
          </div>
        ))}
        
        {isTyping && (
          <div className="message agent">
            <div className="message-content typing-indicator">
              <span></span><span></span><span></span>
            </div>
          </div>
        )}
      </div>

      {suggestions.length > 0 && (
        <div className="suggestions">
          {suggestions.map((sug, idx) => (
            <button
              key={idx}
              className="suggestion-btn"
              onClick={() => handleSuggestionClick(sug)}
            >
              {sug}
            </button>
          ))}
        </div>
      )}

      {progress && (
        <div className="progress-container">
          <div className="progress-bar">
            <div className="progress-fill" style={{ width: `${progress.percent}%` }} />
          </div>
          <div className="progress-text">{progress.text}</div>
        </div>
      )}

      <div className="input-container">
        {(uploadedFiles.video || uploadedFiles.photo || uploadedFiles.audio) && (
          <div className="file-preview">
            {uploadedFiles.video && (
              <div className="file-item">
                🎥 {uploadedFiles.video.name}
                <span className="remove" onClick={() => setUploadedFiles(prev => ({ ...prev, video: null }))}>×</span>
              </div>
            )}
            {uploadedFiles.photo && (
              <div className="file-item">
                📷 {uploadedFiles.photo.name}
                <span className="remove" onClick={() => setUploadedFiles(prev => ({ ...prev, photo: null }))}>×</span>
              </div>
            )}
            {uploadedFiles.audio && (
              <div className="file-item">
                🎵 {uploadedFiles.audio.name}
                <span className="remove" onClick={() => setUploadedFiles(prev => ({ ...prev, audio: null }))}>×</span>
              </div>
            )}
          </div>
        )}
        
        <div className="input-wrapper">
          <div className="file-upload">
            <label className="file-btn" title="Загрузить видео">
              🎥
              <input
                type="file"
                accept="video/*"
                onChange={(e) => e.target.files?.[0] && handleFileUpload(e.target.files[0], 'video')}
                style={{ display: 'none' }}
              />
            </label>
            <label className="file-btn" title="Загрузить фото">
              📷
              <input
                type="file"
                accept="image/*"
                onChange={(e) => e.target.files?.[0] && handleFileUpload(e.target.files[0], 'photo')}
                style={{ display: 'none' }}
              />
            </label>
            <label className="file-btn" title="Загрузить аудио">
              🎵
              <input
                type="file"
                accept="audio/*"
                onChange={(e) => e.target.files?.[0] && handleFileUpload(e.target.files[0], 'audio')}
                style={{ display: 'none' }}
              />
            </label>
          </div>
          
          <input
            type="text"
            className="message-input"
            placeholder="Напиши сообщение или загрузи файл..."
            value={input}
            onChange={(e) => setInput(e.target.value)}
            onKeyPress={(e) => e.key === 'Enter' && handleSend()}
          />
          
          <button className="send-btn" onClick={handleSend}>
            Отправить
          </button>
        </div>
      </div>
    </div>
  );
}
