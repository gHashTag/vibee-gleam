// ===============================
// User Atom - Telegram Auth State
// Freemium model: 3 free renders
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import { STORAGE_KEYS } from './storageKeys';

// Telegram user from Login Widget
export interface TelegramUser {
  id: number;
  first_name: string;
  last_name?: string;
  username?: string;
  photo_url?: string;
  auth_date: number;
  hash: string;
  is_admin?: boolean;
}

// Render quota from API
export interface RenderQuota {
  telegram_id: number;
  total_renders: number;
  free_remaining: number;
  subscription: SubscriptionInfo | null;
}

export interface SubscriptionInfo {
  plan: string; // 'junior' | 'middle' | 'senior'
  generations_limit: number | null;
  generations_used: number;
  remaining: number | null;
}

// Persisted user state
export const userAtom = atomWithStorage<TelegramUser | null>(
  STORAGE_KEYS.user,
  null
);

// Render quota (not persisted - fetched from API)
export const renderQuotaAtom = atom<RenderQuota | null>(null);

// Loading state
export const quotaLoadingAtom = atom<boolean>(false);

// Paywall modal visibility
export const showPaywallAtom = atom<boolean>(false);

// Login modal visibility
export const showLoginModalAtom = atom<boolean>(false);

// API base URL
const API_BASE = import.meta.env.VITE_API_URL || 'https://vibee-mcp.fly.dev';

// Fetch render quota from API
export const fetchQuotaAtom = atom(
  null,
  async (get, set) => {
    const user = get(userAtom);
    if (!user) {
      set(renderQuotaAtom, null);
      return;
    }

    set(quotaLoadingAtom, true);
    try {
      const response = await fetch(
        `${API_BASE}/api/render-quota?telegram_id=${user.id}`
      );
      if (response.ok) {
        const data = await response.json();
        set(renderQuotaAtom, data);
      }
    } catch (error) {
      console.error('Failed to fetch render quota:', error);
    } finally {
      set(quotaLoadingAtom, false);
    }
  }
);

// Log a render to the API
export const logRenderAtom = atom(
  null,
  async (get, set) => {
    const user = get(userAtom);
    if (!user) return false;

    try {
      const response = await fetch(`${API_BASE}/api/render-log`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ telegram_id: user.id }),
      });

      if (response.ok) {
        // Refresh quota after logging
        const quota = get(renderQuotaAtom);
        if (quota) {
          set(renderQuotaAtom, {
            ...quota,
            total_renders: quota.total_renders + 1,
            free_remaining: Math.max(0, quota.free_remaining - 1),
          });
        }
        return true;
      }
      return false;
    } catch (error) {
      console.error('Failed to log render:', error);
      return false;
    }
  }
);

// Check if user can render (has quota)
export const canRenderAtom = atom((get) => {
  const user = get(userAtom);
  const quota = get(renderQuotaAtom);

  // Not logged in - can try for free
  if (!user) return true;

  // No quota loaded yet - allow
  if (!quota) return true;

  // Has free renders remaining
  if (quota.free_remaining > 0) return true;

  // Has subscription with remaining renders
  if (quota.subscription) {
    if (quota.subscription.remaining === null) return true; // Unlimited
    return quota.subscription.remaining > 0;
  }

  return false;
});

// Logout action
export const logoutAtom = atom(
  null,
  (_get, set) => {
    set(userAtom, null);
    set(renderQuotaAtom, null);
    set(showPaywallAtom, false);
  }
);
