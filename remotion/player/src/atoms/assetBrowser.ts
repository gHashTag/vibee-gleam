import { atom } from 'jotai';
import { assetsAtom } from './assets';
import type { Asset } from '@/store/types';

// ===============================
// Asset Browser State Atoms
// ===============================

export type AssetCategory = 'all' | 'video' | 'audio' | 'image' | 'avatar';

// Current filter category
export const browserCategoryAtom = atom<AssetCategory>('all');

// Search query
export const browserSearchAtom = atom<string>('');

// Upload state
export const browserUploadingAtom = atom<boolean>(false);
export const browserUploadProgressAtom = atom<number>(0);

// Derived: filtered assets based on category and search
export const filteredAssetsAtom = atom<Asset[]>((get) => {
  const assets = get(assetsAtom);
  const category = get(browserCategoryAtom);
  const search = get(browserSearchAtom).toLowerCase().trim();

  return assets.filter((asset) => {
    // Category filter
    const matchesCategory = category === 'all' || asset.type === category;

    // Search filter
    const matchesSearch =
      !search ||
      asset.name.toLowerCase().includes(search) ||
      asset.type.toLowerCase().includes(search);

    return matchesCategory && matchesSearch;
  });
});

// Action: set category
export const setBrowserCategoryAtom = atom(
  null,
  (get, set, category: AssetCategory) => {
    set(browserCategoryAtom, category);
  }
);

// Action: set search
export const setBrowserSearchAtom = atom(
  null,
  (get, set, search: string) => {
    set(browserSearchAtom, search);
  }
);

// Action: clear filters
export const clearBrowserFiltersAtom = atom(null, (get, set) => {
  set(browserCategoryAtom, 'all');
  set(browserSearchAtom, '');
});

// Category configuration for UI
export const CATEGORY_CONFIG: Record<
  AssetCategory,
  { label: string; labelRu: string; icon: string; color: string }
> = {
  all: { label: 'All', labelRu: 'Все', icon: '📁', color: '#888' },
  video: { label: 'Video', labelRu: 'Видео', icon: '🎬', color: '#3b82f6' },
  audio: { label: 'Audio', labelRu: 'Аудио', icon: '🎵', color: '#22c55e' },
  image: { label: 'Image', labelRu: 'Фото', icon: '🖼️', color: '#f59e0b' },
  avatar: { label: 'Avatar', labelRu: 'Аватар', icon: '👤', color: '#ec4899' },
};

// Get category counts
export const categoryCounts = atom((get) => {
  const assets = get(assetsAtom);
  const counts: Record<AssetCategory, number> = {
    all: assets.length,
    video: 0,
    audio: 0,
    image: 0,
    avatar: 0,
  };

  assets.forEach((asset) => {
    if (asset.type in counts) {
      counts[asset.type as AssetCategory]++;
    }
  });

  return counts;
});
