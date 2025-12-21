// ===============================
// Project Atom - Core state
// ===============================

import { atomWithStorage } from 'jotai/utils';
import type { Project } from '@/store/types';

const DEFAULT_PROJECT: Project = {
  id: '',
  name: 'Untitled Project',
  fps: 30,
  width: 1080,
  height: 1920,
  durationInFrames: 825, // ~27.5 seconds at 30fps
};

export const projectAtom = atomWithStorage<Project>(
  'vibee-project-v14',
  DEFAULT_PROJECT
);
