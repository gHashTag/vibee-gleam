// ===============================
// Project Atom - Core state
// ===============================

import { atomWithStorage } from 'jotai/utils';
import type { Project } from '@/store/types';
import { STORAGE_KEYS } from './storageKeys';

const DEFAULT_PROJECT: Project = {
  id: '',
  name: 'Vibee Reel',
  fps: 30,
  width: 1080,
  height: 1920,
  durationInFrames: 825, // ~27.5 seconds at 30fps
};

export const projectAtom = atomWithStorage<Project>(
  STORAGE_KEYS.project,
  DEFAULT_PROJECT
);
