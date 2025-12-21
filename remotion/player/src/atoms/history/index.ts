// ===============================
// History Atoms - Undo/Redo support
// ===============================

import { atom } from 'jotai';
import { produce } from 'immer';
import { tracksAtom } from '../tracks';
import { assetsAtom } from '../assets';
import { projectAtom } from '../project';
import type { Track, Asset, Project } from '@/store/types';

// ===============================
// Snapshot Types
// ===============================

interface HistorySnapshot {
  tracks: Track[];
  assets: Asset[];
  project: Project;
  timestamp: number;
}

// ===============================
// History State
// ===============================

const MAX_HISTORY = 50;

const pastAtom = atom<HistorySnapshot[]>([]);
const futureAtom = atom<HistorySnapshot[]>([]);
const isApplyingAtom = atom(false);

// Debounce timer for recording
let debounceTimer: ReturnType<typeof setTimeout> | null = null;

// ===============================
// Record Snapshot
// ===============================

export const recordSnapshotAtom = atom(
  null,
  (get, set) => {
    if (get(isApplyingAtom)) return;

    // Debounce to avoid recording every keystroke
    if (debounceTimer) clearTimeout(debounceTimer);

    debounceTimer = setTimeout(() => {
      const snapshot: HistorySnapshot = {
        tracks: JSON.parse(JSON.stringify(get(tracksAtom))),
        assets: JSON.parse(JSON.stringify(get(assetsAtom))),
        project: JSON.parse(JSON.stringify(get(projectAtom))),
        timestamp: Date.now(),
      };

      const past = get(pastAtom);
      const lastSnapshot = past[past.length - 1];

      // Skip if identical to last snapshot
      if (lastSnapshot &&
          JSON.stringify(lastSnapshot.tracks) === JSON.stringify(snapshot.tracks) &&
          JSON.stringify(lastSnapshot.assets) === JSON.stringify(snapshot.assets)) {
        return;
      }

      set(pastAtom, produce(past, (draft) => {
        draft.push(snapshot);
        if (draft.length > MAX_HISTORY) draft.shift();
      }));

      // Clear future on new action
      set(futureAtom, []);
    }, 300);
  }
);

// ===============================
// Undo Action
// ===============================

export const undoAtom = atom(
  null,
  (get, set) => {
    const past = get(pastAtom);
    if (past.length === 0) return;

    set(isApplyingAtom, true);

    // Save current state to future
    const currentSnapshot: HistorySnapshot = {
      tracks: JSON.parse(JSON.stringify(get(tracksAtom))),
      assets: JSON.parse(JSON.stringify(get(assetsAtom))),
      project: JSON.parse(JSON.stringify(get(projectAtom))),
      timestamp: Date.now(),
    };
    set(futureAtom, produce(get(futureAtom), (draft) => {
      draft.unshift(currentSnapshot);
    }));

    // Pop from past and apply
    const newPast = [...past];
    const snapshot = newPast.pop()!;
    set(pastAtom, newPast);

    set(tracksAtom, snapshot.tracks);
    set(assetsAtom, snapshot.assets);
    set(projectAtom, snapshot.project);

    set(isApplyingAtom, false);
  }
);

// ===============================
// Redo Action
// ===============================

export const redoAtom = atom(
  null,
  (get, set) => {
    const future = get(futureAtom);
    if (future.length === 0) return;

    set(isApplyingAtom, true);

    // Save current state to past
    const currentSnapshot: HistorySnapshot = {
      tracks: JSON.parse(JSON.stringify(get(tracksAtom))),
      assets: JSON.parse(JSON.stringify(get(assetsAtom))),
      project: JSON.parse(JSON.stringify(get(projectAtom))),
      timestamp: Date.now(),
    };
    set(pastAtom, produce(get(pastAtom), (draft) => {
      draft.push(currentSnapshot);
    }));

    // Pop from future and apply
    const newFuture = [...future];
    const snapshot = newFuture.shift()!;
    set(futureAtom, newFuture);

    set(tracksAtom, snapshot.tracks);
    set(assetsAtom, snapshot.assets);
    set(projectAtom, snapshot.project);

    set(isApplyingAtom, false);
  }
);

// ===============================
// Can Undo/Redo Selectors
// ===============================

export const canUndoAtom = atom((get) => get(pastAtom).length > 0);
export const canRedoAtom = atom((get) => get(futureAtom).length > 0);

// ===============================
// Clear History
// ===============================

export const clearHistoryAtom = atom(
  null,
  (get, set) => {
    set(pastAtom, []);
    set(futureAtom, []);
  }
);

export { isApplyingAtom };
