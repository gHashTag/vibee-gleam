import React, { Component, type ReactNode } from 'react';
import { useLanguage } from '@/hooks/useLanguage';

interface Props {
  children: ReactNode;
  fallback?: ReactNode;
}

interface State {
  hasError: boolean;
  error: Error | null;
  errorInfo: React.ErrorInfo | null;
}

class ErrorBoundaryClass extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = { hasError: false, error: null, errorInfo: null };
  }

  static getDerivedStateFromError(error: Error): Partial<State> {
    return { hasError: true, error };
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo) {
    console.error('ErrorBoundary caught an error:', error, errorInfo);
    this.setState({ errorInfo });
  }

  handleReset = () => {
    this.setState({ hasError: false, error: null, errorInfo: null });
  };

  handleReload = () => {
    window.location.reload();
  };

  handleClearStorage = () => {
    localStorage.removeItem('vibee-editor-storage-v4');
    window.location.reload();
  };

  render() {
    if (this.state.hasError) {
      if (this.props.fallback) {
        return this.props.fallback;
      }

      return (
        <ErrorFallback
          error={this.state.error}
          errorInfo={this.state.errorInfo}
          onReset={this.handleReset}
          onReload={this.handleReload}
          onClearStorage={this.handleClearStorage}
        />
      );
    }

    return this.props.children;
  }
}

interface ErrorFallbackProps {
  error: Error | null;
  errorInfo: React.ErrorInfo | null;
  onReset: () => void;
  onReload: () => void;
  onClearStorage: () => void;
}

function ErrorFallback({ error, errorInfo, onReset, onReload, onClearStorage }: ErrorFallbackProps) {
  const { t } = useLanguage();
  const [showDetails, setShowDetails] = React.useState(false);

  return (
    <div className="min-h-screen bg-gray-900 flex items-center justify-center p-4">
      <div className="max-w-lg w-full bg-gray-800 rounded-lg shadow-xl p-6 border border-gray-700">
        <div className="flex items-center gap-3 mb-4">
          <div className="w-12 h-12 rounded-full bg-red-500/20 flex items-center justify-center">
            <svg className="w-6 h-6 text-red-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2}
                d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
            </svg>
          </div>
          <div>
            <h2 className="text-lg font-semibold text-white">
              {t('errors.title')}
            </h2>
            <p className="text-sm text-gray-400">
              {t('errors.subtitle')}
            </p>
          </div>
        </div>

        <div className="space-y-3">
          <button
            onClick={onReset}
            className="w-full px-4 py-2 bg-amber-500 hover:bg-amber-400 text-black font-medium rounded-lg transition-colors"
          >
            {t('errors.tryAgain')}
          </button>

          <button
            onClick={onReload}
            className="w-full px-4 py-2 bg-gray-700 hover:bg-gray-600 text-white rounded-lg transition-colors"
          >
            {t('errors.reload')}
          </button>

          <button
            onClick={onClearStorage}
            className="w-full px-4 py-2 bg-gray-700 hover:bg-gray-600 text-white rounded-lg transition-colors"
          >
            {t('errors.clearAndReload')}
          </button>
        </div>

        <button
          onClick={() => setShowDetails(!showDetails)}
          className="mt-4 text-sm text-gray-400 hover:text-gray-300 flex items-center gap-1"
        >
          <svg
            className={`w-4 h-4 transition-transform ${showDetails ? 'rotate-90' : ''}`}
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
          </svg>
          {t('errors.showDetails')}
        </button>

        {showDetails && (
          <div className="mt-3 p-3 bg-gray-900 rounded-lg text-xs font-mono text-gray-400 overflow-auto max-h-48">
            <div className="text-red-400 mb-2">
              {error?.name}: {error?.message}
            </div>
            {errorInfo?.componentStack && (
              <pre className="whitespace-pre-wrap">
                {errorInfo.componentStack}
              </pre>
            )}
          </div>
        )}

        <p className="mt-4 text-xs text-gray-500 text-center">
          {t('errors.support')}
        </p>
      </div>
    </div>
  );
}

export const ErrorBoundary = ErrorBoundaryClass;
