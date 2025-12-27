import { BrowserRouter, Routes, Route } from 'react-router-dom';
import { LanguageProvider } from '@/hooks/useLanguage';
import { ErrorBoundary } from '@/components/ErrorBoundary';
import { JotaiProvider } from '@/atoms/Provider';
import { ToastContainer } from '@/components/Toast/Toast';
import { BottomNavigation } from '@/components/Navigation';
import { PageTransition } from '@/components/PageTransition';
import { HomePage } from '@/pages/Home';
import { EditorPage } from '@/pages/Editor';
import { ChatPage } from '@/pages/Chat';
import { ProfilePage } from '@/pages/Profile';
import { FeedPage } from '@/pages/Feed';
import { SearchPage } from '@/pages/Search';
import './App.css';

function App() {
  return (
    <ErrorBoundary>
      <JotaiProvider>
        <LanguageProvider>
          <BrowserRouter>
            <PageTransition>
              <Routes>
                <Route path="/" element={<HomePage />} />
                <Route path="/feed" element={<FeedPage />} />
                <Route path="/search" element={<SearchPage />} />
                <Route path="/editor" element={<EditorPage />} />
                <Route path="/chat" element={<ChatPage />} />
                <Route path="/:username" element={<ProfilePage />} />
              </Routes>
            </PageTransition>
            <BottomNavigation />
          </BrowserRouter>
          <ToastContainer />
        </LanguageProvider>
      </JotaiProvider>
    </ErrorBoundary>
  );
}

export default App;
