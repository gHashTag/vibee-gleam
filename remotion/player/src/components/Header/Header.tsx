import { useState, useEffect } from 'react';
import { Link, useLocation } from 'react-router-dom';
import { useAtomValue, useSetAtom } from 'jotai';
import { useLanguage } from '@/hooks/useLanguage';
import {
  projectAtom,
  // User & Quota atoms
  userAtom,
  renderQuotaAtom,
  showLoginModalAtom,
  fetchQuotaAtom,
  logoutAtom,
} from '@/atoms';
import { X, Zap, Keyboard } from 'lucide-react';
import { TelegramLoginButton, UserAvatar, PaywallModal } from '@/components/Auth';
import { RemixBadge } from '@/components/RemixBadge';
import './styles.css';

// Page navigation tabs with emojis (same as VerticalTabs)
const NAV_TABS = [
  { id: 'feed', emoji: '🌐', labelKey: 'tabs.feed', route: '/feed' },
  { id: 'editor', emoji: '▶️', labelKey: 'tabs.editor', route: '/editor' },
  { id: 'lipsync', emoji: '👄', labelKey: 'tabs.avatar', route: '/generate/avatar' },
  { id: 'video', emoji: '🎬', labelKey: 'generate.video', route: '/generate/video' },
  { id: 'image', emoji: '📷', labelKey: 'generate.image', route: '/generate/image' },
  { id: 'audio', emoji: '🎤', labelKey: 'generate.audio', route: '/generate/audio' },
] as const;

// Route patterns to match for active state
const ROUTE_PATTERNS: Record<string, RegExp> = {
  'feed': /^\/feed/,
  'editor': /^\/editor/,
  'lipsync': /^\/generate\/avatar/,
  'video': /^\/generate\/video/,
  'image': /^\/generate\/image/,
  'audio': /^\/generate\/audio/,
};

// Export settings stored in localStorage
interface ExportSettings {
  codec: 'h264' | 'h265' | 'vp9' | 'prores';
  quality: 'high' | 'medium' | 'low';
}

const DEFAULT_EXPORT_SETTINGS: ExportSettings = {
  codec: 'h264',
  quality: 'high',
};

function getExportSettings(): ExportSettings {
  try {
    const saved = localStorage.getItem('vibee-export-settings');
    return saved ? { ...DEFAULT_EXPORT_SETTINGS, ...JSON.parse(saved) } : DEFAULT_EXPORT_SETTINGS;
  } catch {
    return DEFAULT_EXPORT_SETTINGS;
  }
}

function saveExportSettings(settings: ExportSettings) {
  localStorage.setItem('vibee-export-settings', JSON.stringify(settings));
}

interface HeaderProps {
  wsStatus?: 'connected' | 'disconnected';
  wsClientId?: string | null;
}

export function Header({ wsStatus, wsClientId }: HeaderProps) {
  // Language hook
  const { lang, setLang, t } = useLanguage();
  const location = useLocation();

  // Jotai atoms
  const project = useAtomValue(projectAtom);

  // User & Quota state
  const user = useAtomValue(userAtom);
  const quota = useAtomValue(renderQuotaAtom);
  const showLoginModal = useAtomValue(showLoginModalAtom);

  // User actions
  const fetchQuota = useSetAtom(fetchQuotaAtom);
  const logout = useSetAtom(logoutAtom);
  const setShowLoginModal = useSetAtom(showLoginModalAtom);

  // Fetch quota on mount if user is logged in
  useEffect(() => {
    if (user) {
      fetchQuota();
    }
  }, [user, fetchQuota]);

  const [showSettings, setShowSettings] = useState(false);
  const [exportSettings, setExportSettings] = useState<ExportSettings>(getExportSettings);

  const handleSettingsChange = (key: keyof ExportSettings, value: string) => {
    const newSettings = { ...exportSettings, [key]: value };
    setExportSettings(newSettings);
    saveExportSettings(newSettings);
  };

  // Note: Undo/Redo, Play, Save, Load, Reset buttons moved to Timeline.tsx

  return (
    <>
      {/* Skip navigation link for keyboard users */}
      <a href="#main-content" className="skip-nav">
        {t('a11y.skipToContent')}
      </a>
      <header className="header" role="banner">
        <div className="header-left">
          <Link to="/" className="logo">
            <span className="logo-icon" aria-hidden="true">🐝</span>
            <span className="logo-text">VIBEE</span>
          </Link>

          {/* Emoji Navigation Tabs */}
          <nav className="header-tabs" aria-label="Main navigation">
            {NAV_TABS.map((tab) => {
              const isActive = ROUTE_PATTERNS[tab.id]?.test(location.pathname);
              return (
                <Link
                  key={tab.id}
                  to={tab.route}
                  className={`header-tab ${isActive ? 'active' : ''}`}
                  title={t(tab.labelKey)}
                >
                  <span className="header-tab-emoji">{tab.emoji}</span>
                </Link>
              );
            })}
          </nav>
        </div>

      <div className="header-center">
        <RemixBadge />
      </div>

      <div className="header-right">
        {/* Language Switcher */}
        <button
          className="header-button lang-toggle"
          onClick={() => setLang(lang === 'en' ? 'ru' : 'en')}
          title={lang === 'en' ? 'Переключить на русский' : 'Switch to English'}
        >
          {lang.toUpperCase()}
        </button>

        {/* User login / Quota display */}
        {user ? (
          <div className="user-section">
            {/* Quota display */}
            {quota && (
              <div
                className={`quota-display ${
                  quota.free_remaining === 0 && !quota.subscription
                    ? 'exhausted'
                    : quota.free_remaining <= 1
                    ? 'warning'
                    : ''
                }`}
                title={`${quota.total_renders} renders used`}
              >
                <Zap size={14} />
                <span>
                  {quota.subscription
                    ? quota.subscription.remaining === null
                      ? t('quota.unlimited')
                      : `${quota.subscription.remaining} ${t('quota.left')}`
                    : `${quota.free_remaining}/3 ${t('quota.free')}`}
                </span>
              </div>
            )}
            <UserAvatar user={user} onLogout={logout} />
          </div>
        ) : (
          <TelegramLoginButton
            onSuccess={() => setShowLoginModal(false)}
          />
        )}
      </div>

      {/* Note: Export button, blob warning dialogs, reset/save dialogs moved to Timeline.tsx */}

      {/* Settings Modal */}
      {showSettings && (
        <div className="settings-overlay" onClick={() => setShowSettings(false)}>
          <div className="settings-modal" onClick={(e) => e.stopPropagation()}>
            <div className="settings-header">
              <h2>{t('settings.title')}</h2>
              <button className="settings-close" onClick={() => setShowSettings(false)}>
                <X size={20} />
              </button>
            </div>

            <div className="settings-content">
              {/* Export Settings */}
              <div className="settings-section">
                <h3>{t('settings.export')}</h3>
                <div className="settings-row">
                  <label>{t('settings.codec')}</label>
                  <select
                    value={exportSettings.codec}
                    onChange={(e) => handleSettingsChange('codec', e.target.value)}
                  >
                    <option value="h264">{t('codec.h264')}</option>
                    <option value="h265">{t('codec.h265')}</option>
                    <option value="vp9">{t('codec.vp9')}</option>
                    <option value="prores">{t('codec.prores')}</option>
                  </select>
                </div>
                <div className="settings-row">
                  <label>{t('settings.quality')}</label>
                  <select
                    value={exportSettings.quality}
                    onChange={(e) => handleSettingsChange('quality', e.target.value)}
                  >
                    <option value="high">{t('quality.high')}</option>
                    <option value="medium">{t('quality.medium')}</option>
                    <option value="low">{t('quality.low')}</option>
                  </select>
                </div>
              </div>

              {/* Keyboard Shortcuts */}
              <div className="settings-section">
                <h3><Keyboard size={16} /> {t('settings.shortcuts')}</h3>
                <div className="shortcuts-grid">
                  <div className="shortcut-item">
                    <kbd>Space</kbd>
                    <span>{t('shortcut.playPause')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>J / K / L</kbd>
                    <span>{t('shortcut.jkl')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + Z</kbd>
                    <span>{t('shortcut.undo')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + Shift + Z</kbd>
                    <span>{t('shortcut.redo')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + A</kbd>
                    <span>{t('shortcut.selectAll')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + C</kbd>
                    <span>{t('shortcut.copy')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + V</kbd>
                    <span>{t('shortcut.paste')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Cmd/Ctrl + D</kbd>
                    <span>{t('shortcut.duplicate')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Delete / Backspace</kbd>
                    <span>{t('shortcut.delete')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Escape</kbd>
                    <span>{t('shortcut.clearSelection')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Arrow Left/Right</kbd>
                    <span>{t('shortcut.move1Frame')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Shift + Arrow</kbd>
                    <span>{t('shortcut.move10Frames')}</span>
                  </div>
                  <div className="shortcut-item">
                    <kbd>Home / End</kbd>
                    <span>{t('shortcut.goToStartEnd')}</span>
                  </div>
                </div>
              </div>

              {/* Project Info */}
              <div className="settings-section">
                <h3>{t('settings.project')}</h3>
                <div className="project-info">
                  <div className="info-row">
                    <span>{t('settings.name')}:</span>
                    <span>{project.name}</span>
                  </div>
                  <div className="info-row">
                    <span>{t('settings.resolution')}:</span>
                    <span>{project.width} x {project.height}</span>
                  </div>
                  <div className="info-row">
                    <span>{t('settings.fps')}:</span>
                    <span>{project.fps}</span>
                  </div>
                  <div className="info-row">
                    <span>{t('settings.duration')}:</span>
                    <span>{(project.durationInFrames / project.fps).toFixed(1)}s ({project.durationInFrames} frames)</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Paywall Modal */}
      <PaywallModal />

      {/* Login Modal */}
      {showLoginModal && (
        <div className="login-modal-overlay" onClick={() => setShowLoginModal(false)}>
          <div className="login-modal" onClick={(e) => e.stopPropagation()}>
            <h2>{t('login.title')}</h2>
            <p>{t('login.subtitle')}</p>
            <div className="login-modal-widget">
              <TelegramLoginButton
                onSuccess={() => setShowLoginModal(false)}
                size="large"
                showFallback={true}
              />
            </div>
          </div>
        </div>
      )}

      </header>
    </>
  );
}
