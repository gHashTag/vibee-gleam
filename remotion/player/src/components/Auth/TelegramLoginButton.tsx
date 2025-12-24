// Telegram Login Widget
// Uses official Telegram Login Widget for web authentication

import { useEffect, useRef } from 'react';
import { useSetAtom } from 'jotai';
import { userAtom, fetchQuotaAtom } from '@/atoms';
import type { TelegramUser } from '@/atoms';

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
}

export function TelegramLoginButton({
  botUsername = 'agent_vibecoder_bot',
  size = 'medium',
  onSuccess,
}: TelegramLoginButtonProps) {
  const containerRef = useRef<HTMLDivElement>(null);
  const setUser = useSetAtom(userAtom);
  const fetchQuota = useSetAtom(fetchQuotaAtom);

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
      onSuccess?.();
    };

    // Dynamically load Telegram widget script
    if (containerRef.current) {
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
  }, [botUsername, size, setUser, fetchQuota, onSuccess]);

  return <div ref={containerRef} className="telegram-login-container" />;
}

// Compact user avatar display (when logged in)
export function UserAvatar({
  user,
  onLogout
}: {
  user: TelegramUser;
  onLogout: () => void;
}) {
  return (
    <div className="user-avatar-container">
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
      <button
        onClick={onLogout}
        className="logout-btn"
        title="Logout"
      >
        &times;
      </button>
    </div>
  );
}
