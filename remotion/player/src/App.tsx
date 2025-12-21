import { BrowserRouter, Routes, Route } from 'react-router-dom';
import { LanguageProvider } from '@/hooks/useLanguage';
import { ErrorBoundary } from '@/components/ErrorBoundary';
import { JotaiProvider } from '@/atoms/Provider';
import { HomePage } from '@/pages/Home';
import { EditorPage } from '@/pages/Editor';
import { ChatPage } from '@/pages/Chat';
import './App.css';

function App() {
  return (
    <ErrorBoundary>
      <JotaiProvider>
        <LanguageProvider>
          <BrowserRouter>
            <Routes>
              <Route path="/" element={<HomePage />} />
              <Route path="/editor" element={<EditorPage />} />
              <Route path="/chat" element={<ChatPage />} />
            </Routes>
          </BrowserRouter>
        </LanguageProvider>
      </JotaiProvider>
    </ErrorBoundary>
  );
}

export default App;
