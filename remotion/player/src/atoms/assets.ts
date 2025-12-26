// ===============================
// Assets Atom - Media files storage
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import { produce } from 'immer';
import { nanoid } from 'nanoid';
import type { Asset } from '@/store/types';
import { STORAGE_KEYS } from './storageKeys';

// Default assets (pre-loaded)
const DEFAULT_ASSETS: Asset[] = [
  {
    id: 'asset-lipsync',
    type: 'video',
    name: 'Lipsync Video',
    url: '/lipsync/lipsync.mp4',
    duration: 900,
  },
  {
    id: 'asset-cover',
    type: 'image',
    name: 'Cover Image',
    url: '/covers/cover.jpeg',
    width: 1080,
    height: 1920,
  },
  {
    id: 'asset-bg-00',
    type: 'video',
    name: 'Background 00',
    url: '/backgrounds/business/00.mp4',
    duration: 300,
  },
  {
    id: 'asset-bg-01',
    type: 'video',
    name: 'Background 01',
    url: '/backgrounds/business/01.mp4',
    duration: 300,
  },
  {
    id: 'asset-bg-02',
    type: 'video',
    name: 'Background 02',
    url: '/backgrounds/business/02.mp4',
    duration: 300,
  },
  {
    id: 'asset-bg-03',
    type: 'video',
    name: 'Background 03',
    url: '/backgrounds/business/03.mp4',
    duration: 300,
  },
  {
    id: 'asset-bg-04',
    type: 'video',
    name: 'Background 04',
    url: '/backgrounds/business/04.mp4',
    duration: 300,
  },
  {
    id: 'asset-music-phonk',
    type: 'audio',
    name: 'Phonk Music',
    url: '/audio/music/phonk_01.mp3',
    duration: 1800,
  },
  {
    id: 'asset-music-business',
    type: 'audio',
    name: 'Business Music',
    url: '/music/business.mp3',
    duration: 1800,
  },
  {
    id: 'asset-music-corporate',
    type: 'audio',
    name: 'Corporate Music',
    url: '/music/corporate.mp3',
    duration: 1800,
  },
  {
    id: 'asset-music-upbeat',
    type: 'audio',
    name: 'Upbeat Music',
    url: '/music/upbeat.mp3',
    duration: 1800,
  },
];

// Core assets atom
export const assetsAtom = atomWithStorage<Asset[]>(
  STORAGE_KEYS.assets,
  DEFAULT_ASSETS
);

// Action: Add asset
export const addAssetAtom = atom(
  null,
  (get, set, assetData: Omit<Asset, 'id'>) => {
    const id = `asset-${nanoid()}`;
    set(assetsAtom, produce(get(assetsAtom), (draft) => {
      draft.push({ ...assetData, id });
    }));
    return id;
  }
);

// Action: Remove asset
export const removeAssetAtom = atom(
  null,
  (get, set, assetId: string) => {
    set(assetsAtom, produce(get(assetsAtom), (draft) => {
      const index = draft.findIndex((a) => a.id === assetId);
      if (index !== -1) draft.splice(index, 1);
    }));
  }
);

// Selector: Get asset by ID
export const getAssetByIdAtom = atom((get) => {
  const assets = get(assetsAtom);
  return (assetId: string) => assets.find((a) => a.id === assetId);
});

// Default asset IDs that should never be removed
const DEFAULT_ASSET_IDS = [
  'asset-lipsync',
  'asset-cover',
  'asset-music-phonk',
  'asset-music-business',
  'asset-music-corporate',
  'asset-music-upbeat',
];

export { DEFAULT_ASSETS, DEFAULT_ASSET_IDS };
