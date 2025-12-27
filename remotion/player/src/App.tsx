import { lazy, Suspense } from 'react';
import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { LanguageProvider } from '@/hooks/useLanguage';
import { ErrorBoundary } from '@/components/ErrorBoundary';
import { JotaiProvider } from '@/atoms/Provider';
import { ToastContainer } from '@/components/Toast/Toast';
import { BottomNavigation } from '@/components/Navigation';
import { PageTransition } from '@/components/PageTransition';
import './App.css';

// Lazy load pages for code splitting
const HomePage = lazy(() => import('@/pages/Home'));
const EditorPage = lazy(() => import('@/pages/Editor'));
const ChatPage = lazy(() => import('@/pages/Chat'));
const ProfilePage = lazy(() => import('@/pages/Profile'));
const FeedPage = lazy(() => import('@/pages/Feed'));
const SearchPage = lazy(() => import('@/pages/Search'));
const GeneratePage = lazy(() => import('@/pages/Generate'));

// Loading fallback
function PageLoader() {
  return (
    <div className="page-loader">
      <div className="page-loader__spinner" />
    </div>
  );
}

function App() {
  return (
    <ErrorBoundary>
      <JotaiProvider>
        <LanguageProvider>
          <BrowserRouter>
            <PageTransition>
              <Suspense fallback={<PageLoader />}>
                <Routes>
                  <Route path="/" element={<HomePage />} />
                  <Route path="/feed" element={<FeedPage />} />
                  <Route path="/search" element={<SearchPage />} />
                  <Route path="/editor" element={<EditorPage />} />
                  <Route path="/generate" element={<Navigate to="/generate/image" replace />} />
                  <Route path="/generate/:tab" element={<GeneratePage />} />
                  <Route path="/chat" element={<ChatPage />} />
                  <Route path="/:username" element={<ProfilePage />} />
                </Routes>
              </Suspense>
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
