// ===============================
// Selection Atoms - Item selection state
// ===============================

import { atom } from 'jotai';
import type { TrackItem } from '@/store/types';

// Selected item IDs
export const selectedItemIdsAtom = atom<string[]>([]);

// Selection anchor for shift+click range selection
export const selectionAnchorAtom = atom<string | null>(null);

// Clipboard for copy/paste
export const clipboardAtom = atom<TrackItem[]>([]);

// Action: Select items
export const selectItemsAtom = atom(
  null,
  (get, set, { itemIds, addToSelection = false }: { itemIds: string[]; addToSelection?: boolean }) => {
    if (addToSelection) {
      const current = get(selectedItemIdsAtom);
      const newIds = itemIds.filter((id) => !current.includes(id));
      set(selectedItemIdsAtom, [...current, ...newIds]);
    } else {
      set(selectedItemIdsAtom, itemIds);
    }
    // Set anchor to first selected item
    if (itemIds.length > 0) {
      set(selectionAnchorAtom, itemIds[0]);
    }
  }
);

// Action: Clear selection
export const clearSelectionAtom = atom(
  null,
  (get, set) => {
    set(selectedItemIdsAtom, []);
    set(selectionAnchorAtom, null);
  }
);

// Action: Copy items to clipboard
export const copyItemsAtom = atom(
  null,
  (get, set, items: TrackItem[]) => {
    set(clipboardAtom, items.map((item) => JSON.parse(JSON.stringify(item))));
  }
);

// Need tracksAtom for derived atoms
import { tracksAtom } from './tracks';
import { currentFrameAtom } from './playback';
import { nanoid } from 'nanoid';
import { produce } from 'immer';

// Derived: Get selected items (actual TrackItem objects)
export const getSelectedItemsAtom = atom((get) => {
  const selectedIds = get(selectedItemIdsAtom);
  const tracks = get(tracksAtom);
  const items: TrackItem[] = [];

  for (const track of tracks) {
    for (const item of track.items) {
      if (selectedIds.includes(item.id)) {
        items.push(item);
      }
    }
  }
  return items;
});

// Action: Select all items
export const selectAllAtom = atom(
  null,
  (get, set) => {
    const tracks = get(tracksAtom);
    const allIds: string[] = [];
    for (const track of tracks) {
      for (const item of track.items) {
        allIds.push(item.id);
      }
    }
    set(selectedItemIdsAtom, allIds);
  }
);

// Action: Select range (shift+click)
export const selectRangeAtom = atom(
  null,
  (get, set, targetId: string) => {
    const anchor = get(selectionAnchorAtom);
    const tracks = get(tracksAtom);

    if (!anchor) {
      // No anchor - just select this item
      set(selectedItemIdsAtom, [targetId]);
      set(selectionAnchorAtom, targetId);
      return;
    }

    // Find anchor and target items
    let anchorItem: TrackItem | null = null;
    let targetItem: TrackItem | null = null;
    let anchorTrack: typeof tracks[0] | null = null;

    for (const track of tracks) {
      for (const item of track.items) {
        if (item.id === anchor) {
          anchorItem = item;
          anchorTrack = track;
        }
        if (item.id === targetId) {
          targetItem = item;
        }
      }
    }

    if (!anchorItem || !targetItem || !anchorTrack) {
      set(selectedItemIdsAtom, [targetId]);
      set(selectionAnchorAtom, targetId);
      return;
    }

    // Select all items between anchor and target on the same track
    const startFrame = Math.min(anchorItem.startFrame, targetItem.startFrame);
    const endFrame = Math.max(
      anchorItem.startFrame + anchorItem.durationInFrames,
      targetItem.startFrame + targetItem.durationInFrames
    );

    const rangeIds: string[] = [];
    for (const item of anchorTrack.items) {
      const itemEnd = item.startFrame + item.durationInFrames;
      if (item.startFrame < endFrame && itemEnd > startFrame) {
        rangeIds.push(item.id);
      }
    }

    set(selectedItemIdsAtom, rangeIds);
  }
);

// Action: Paste items from clipboard
export const pasteItemsAtom = atom(
  null,
  (get, set) => {
    const clipboard = get(clipboardAtom);
    if (clipboard.length === 0) return;

    const currentFrame = get(currentFrameAtom);
    const tracks = get(tracksAtom);
    const newIds: string[] = [];

    // Group clipboard items by track type
    const itemsByType = new Map<string, TrackItem[]>();
    for (const item of clipboard) {
      const type = item.type;
      if (!itemsByType.has(type)) {
        itemsByType.set(type, []);
      }
      itemsByType.get(type)!.push(item);
    }

    // Find the earliest start frame in clipboard
    const minStartFrame = Math.min(...clipboard.map(i => i.startFrame));

    // Paste to appropriate tracks
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      for (const [type, items] of itemsByType) {
        // Find matching track
        const track = draft.find(t => t.type === type);
        if (!track) continue;

        for (const item of items) {
          const newId = `item-${nanoid()}`;
          const offset = item.startFrame - minStartFrame;
          const pastedItem = {
            ...JSON.parse(JSON.stringify(item)),
            id: newId,
            trackId: track.id,
            startFrame: currentFrame + offset,
          };
          track.items.push(pastedItem);
          newIds.push(newId);
        }
      }
    }));

    // Select pasted items
    set(selectedItemIdsAtom, newIds);
  }
);
