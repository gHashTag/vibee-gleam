import { create } from 'zustand';
import type { EditorState } from './types';

/**
 * History/Undo-Redo system for the editor
 *
 * Tracks state snapshots and provides undo/redo functionality.
 * Only tracks specific state slices to avoid noise from playback state.
 */

// State to track for undo/redo (excluding playback state)
type HistorySnapshot = {
  tracks: EditorState['tracks'];
  assets: EditorState['assets'];
  templateProps: EditorState['templateProps'];
  project: EditorState['project'];
};

interface HistoryState {
  // Stack of past states (most recent at end)
  past: HistorySnapshot[];

  // Stack of future states for redo (most recent at end)
  future: HistorySnapshot[];

  // Maximum history size to prevent memory issues
  maxSize: number;

  // Is currently applying undo/redo (prevents recording)
  isApplying: boolean;
}

interface HistoryActions {
  // Record a new state snapshot
  record: (snapshot: HistorySnapshot) => void;

  // Undo to previous state
  undo: () => HistorySnapshot | null;

  // Redo to next state
  redo: () => HistorySnapshot | null;

  // Clear all history
  clear: () => void;

  // Check if undo/redo is available
  canUndo: () => boolean;
  canRedo: () => boolean;

  // Set applying flag
  setApplying: (applying: boolean) => void;
}

type HistoryStore = HistoryState & HistoryActions;

export const useHistoryStore = create<HistoryStore>((set, get) => ({
  past: [],
  future: [],
  maxSize: 50,
  isApplying: false,

  record: (snapshot) => {
    const state = get();

    // Don't record if we're currently applying undo/redo
    if (state.isApplying) return;

    // Compare with last snapshot to avoid duplicates
    const lastSnapshot = state.past[state.past.length - 1];
    if (lastSnapshot && JSON.stringify(lastSnapshot) === JSON.stringify(snapshot)) {
      return;
    }

    set((s) => {
      const newPast = [...s.past, snapshot];

      // Trim if over max size
      if (newPast.length > s.maxSize) {
        newPast.shift();
      }

      return {
        past: newPast,
        future: [], // Clear redo stack on new action
      };
    });
  },

  undo: () => {
    const state = get();

    if (state.past.length <= 1) {
      return null; // Need at least 2 items (current + one to go back to)
    }

    // Get the current state (last item) and the previous state
    const newPast = [...state.past];
    const current = newPast.pop()!; // Remove current from past
    const previous = newPast[newPast.length - 1]; // This becomes current

    set({
      past: newPast,
      future: [current, ...state.future], // Add current to future for redo
    });

    return previous;
  },

  redo: () => {
    const state = get();

    if (state.future.length === 0) {
      return null;
    }

    // Get the next state from future
    const [next, ...remainingFuture] = state.future;

    set({
      past: [...state.past, next],
      future: remainingFuture,
    });

    return next;
  },

  clear: () => {
    set({ past: [], future: [] });
  },

  canUndo: () => get().past.length > 1,
  canRedo: () => get().future.length > 0,

  setApplying: (applying) => {
    set({ isApplying: applying });
  },
}));

// Helper to extract history-relevant state
export function extractHistorySnapshot(state: EditorState): HistorySnapshot {
  return {
    tracks: JSON.parse(JSON.stringify(state.tracks)),
    assets: JSON.parse(JSON.stringify(state.assets)),
    templateProps: JSON.parse(JSON.stringify(state.templateProps)),
    project: JSON.parse(JSON.stringify(state.project)),
  };
}
