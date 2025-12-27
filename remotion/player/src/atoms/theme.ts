// ===============================
// Theme System
// Dark/Light mode with system preference
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import { STORAGE_KEYS } from './storageKeys';

export type ThemeMode = 'light' | 'dark' | 'system';
export type ResolvedTheme = 'light' | 'dark';

// User's theme preference (stored)
export const themeModeAtom = atomWithStorage<ThemeMode>(
  STORAGE_KEYS.theme || 'vibee-theme',
  'system'
);

// System theme detection
export const systemThemeAtom = atom<ResolvedTheme>('dark');

// Resolved theme (what's actually applied)
export const resolvedThemeAtom = atom<ResolvedTheme>((get) => {
  const mode = get(themeModeAtom);
  if (mode === 'system') {
    return get(systemThemeAtom);
  }
  return mode;
});

// Toggle between light and dark
export const toggleThemeAtom = atom(
  null,
  (get, set) => {
    const current = get(resolvedThemeAtom);
    set(themeModeAtom, current === 'dark' ? 'light' : 'dark');
  }
);

// Cycle through all modes: system -> light -> dark -> system
export const cycleThemeAtom = atom(
  null,
  (get, set) => {
    const current = get(themeModeAtom);
    const next: ThemeMode =
      current === 'system' ? 'light' :
      current === 'light' ? 'dark' : 'system';
    set(themeModeAtom, next);
  }
);

// Theme colors (for programmatic access)
export const themeColorsAtom = atom((get) => {
  const theme = get(resolvedThemeAtom);

  if (theme === 'light') {
    return {
      bg: '#ffffff',
      bgElevated: '#f5f5f5',
      bgSecondary: '#e5e5e5',
      text: '#0a0a0a',
      textSecondary: '#666666',
      textMuted: '#999999',
      border: '#e0e0e0',
      borderLight: '#f0f0f0',
      amber: '#f59e0b',
      amberLight: '#fbbf24',
      amberDark: '#d97706',
    };
  }

  return {
    bg: '#0a0a0a',
    bgElevated: '#111111',
    bgSecondary: '#1a1a1a',
    text: '#ffffff',
    textSecondary: '#a0a0a0',
    textMuted: '#666666',
    border: '#1a1a1a',
    borderLight: '#2a2a2a',
    amber: '#f59e0b',
    amberLight: '#fbbf24',
    amberDark: '#d97706',
  };
});
