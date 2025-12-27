// Telegram Login Widget
// Uses official Telegram Login Widget for web authentication

import { useEffect, useRef } from 'react';
import { Link } from 'react-router-dom';
import { useSetAtom } from 'jotai';
import { userAtom, fetchQuotaAtom, fetchMyProfileAtom } from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import type { TelegramUser } from '@/atoms';
import { RENDER_SERVER_URL } from '@/lib/mediaUrl';

declare global {
  interface Window {
    onTelegramAuth: (user: TelegramAuthData) => void;
  }
}

interface TelegramAuthData {
  id: number;
  first_name: string;
  last_name?: string;
  username?: string;
  photo_url?: string;
  auth_date: number;
  hash: string;
}

interface TelegramLoginButtonProps {
  botUsername?: string;
  size?: 'small' | 'medium' | 'large';
  onSuccess?: () => void;
  showFallback?: boolean;
}

// Telegram SVG icon
const TelegramIcon = () => (
  <svg viewBox="0 0 24 24" width="24" height="24" fill="currentColor">
    <path d="M11.944 0A12 12 0 0 0 0 12a12 12 0 0 0 12 12 12 12 0 0 0 12-12A12 12 0 0 0 12 0a12 12 0 0 0-.056 0zm4.962 7.224c.1-.002.321.023.465.14a.506.506 0 0 1 .171.325c.016.093.036.306.02.472-.18 1.898-.962 6.502-1.36 8.627-.168.9-.499 1.201-.82 1.23-.696.065-1.225-.46-1.9-.902-1.056-.693-1.653-1.124-2.678-1.8-1.185-.78-.417-1.21.258-1.91.177-.184 3.247-2.977 3.307-3.23.007-.032.014-.15-.056-.212s-.174-.041-.249-.024c-.106.024-1.793 1.14-5.061 3.345-.48.33-.913.49-1.302.48-.428-.008-1.252-.241-1.865-.44-.752-.245-1.349-.374-1.297-.789.027-.216.325-.437.893-.663 3.498-1.524 5.83-2.529 6.998-3.014 3.332-1.386 4.025-1.627 4.476-1.635z"/>
  </svg>
);

export function TelegramLoginButton({
  botUsername = 'agent_vibecoder_bot',
  size = 'medium',
  onSuccess,
  showFallback = false,
}: TelegramLoginButtonProps) {
  const { t } = useLanguage();
  const containerRef = useRef<HTMLDivElement>(null);
  const setUser = useSetAtom(userAtom);
  const fetchQuota = useSetAtom(fetchQuotaAtom);
  const fetchMyProfile = useSetAtom(fetchMyProfileAtom);

  useEffect(() => {
    // Define global callback for Telegram widget
    window.onTelegramAuth = (data: TelegramAuthData) => {
      console.log('[TelegramAuth] User authenticated:', data);

      const user: TelegramUser = {
        id: data.id,
        first_name: data.first_name,
        last_name: data.last_name,
        username: data.username,
        photo_url: data.photo_url,
        auth_date: data.auth_date,
        hash: data.hash,
      };

      setUser(user);
      fetchQuota();

      // Create/fetch user profile in database
      // Pass user data directly to avoid async state issues
      fetchMyProfile({
        id: data.id,
        username: data.username,
        first_name: data.first_name,
        photo_url: data.photo_url,
      });

      // Notify about new lead
      fetch(`${RENDER_SERVER_URL}/api/notify/lead`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          telegram_id: data.id,
          username: data.username,
          first_name: data.first_name,
        }),
      }).catch(() => {}); // Don't block on notification failure

      onSuccess?.();
    };

    // Only load native widget when not using fallback
    if (!showFallback && containerRef.current) {
      containerRef.current.innerHTML = '';

      const script = document.createElement('script');
      script.src = 'https://telegram.org/js/telegram-widget.js?22';
      script.async = true;
      script.setAttribute('data-telegram-login', botUsername);
      script.setAttribute('data-size', size);
      script.setAttribute('data-onauth', 'onTelegramAuth(user)');
      script.setAttribute('data-request-access', 'write');

      containerRef.current.appendChild(script);
    }

    return () => {
      // Cleanup
      delete (window as any).onTelegramAuth;
    };
  }, [botUsername, size, setUser, fetchQuota, fetchMyProfile, onSuccess, showFallback]);

  // Fallback: Open bot directly in Telegram
  const handleFallbackClick = () => {
    window.open(`https://t.me/${botUsername}?start=login`, '_blank');
  };

  return (
    <div className="telegram-login-wrapper">
      {/* Show custom button when showFallback is true, hide native widget */}
      {showFallback ? (
        <button
          className={`telegram-login-btn ${size === 'small' ? 'small' : ''}`}
          onClick={handleFallbackClick}
          type="button"
        >
          <TelegramIcon />
          <span>{size === 'small' ? t('login.button') : t('login.buttonFull')}</span>
        </button>
      ) : (
        /* Native Telegram widget for header */
        <div ref={containerRef} className="telegram-login-container" />
      )}
    </div>
  );
}

// Compact user avatar display (when logged in)
export function UserAvatar({
  user,
  onLogout
}: {
  user: TelegramUser;
  onLogout: () => void;
}) {
  const { t } = useLanguage();

  // Use username for profile link, fallback to user_{id}
  const profilePath = user.username ? `/${user.username}` : `/user_${user.id}`;

  return (
    <div className="user-avatar-container">
      <Link to={profilePath} className="user-avatar-link" title={t('profile.viewProfile')}>
        {user.photo_url ? (
          <img
            src={user.photo_url}
            alt={user.first_name}
            className="user-avatar-img"
          />
        ) : (
          <div className="user-avatar-placeholder">
            {user.first_name.charAt(0)}
          </div>
        )}
        <span className="user-name">{user.first_name}</span>
      </Link>
      <button
        onClick={onLogout}
        className="logout-btn"
        title={t('auth.logout')}
      >
        &times;
      </button>
    </div>
  );
}
