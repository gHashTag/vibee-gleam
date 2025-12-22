// ===============================
// UI Atoms - Interface state
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import type { SnapSettings, Marker, MarkerColor } from '@/store/types';
import { nanoid } from 'nanoid';
import { produce } from 'immer';

// Sidebar tab
export const sidebarTabAtom = atom<'assets' | 'properties'>('assets');

// Canvas zoom (0 = auto fit to height)
export const canvasZoomAtom = atom(0);

// Timeline zoom (persisted)
export const timelineZoomAtom = atomWithStorage('vibee-timeline-zoom', 1);

// Snap settings (persisted)
export const snapSettingsAtom = atomWithStorage<SnapSettings>('vibee-snap-settings', {
  enabled: false,
  interval: 15, // 0.5s at 30fps
});

// In/Out points for playback range
export const inPointAtom = atom<number | null>(null);
export const outPointAtom = atom<number | null>(null);

// Timeline markers
export const markersAtom = atom<Marker[]>([]);

// Export state
export const isExportingAtom = atom(false);
export const exportProgressAtom = atom(0);

// Volume popup - which track item is showing volume popup (null = closed)
export const volumePopupItemIdAtom = atom<string | null>(null);

// Action: Set snap enabled
export const setSnapEnabledAtom = atom(
  null,
  (get, set, enabled: boolean) => {
    set(snapSettingsAtom, { ...get(snapSettingsAtom), enabled });
  }
);

// Action: Set snap interval
export const setSnapIntervalAtom = atom(
  null,
  (get, set, interval: number) => {
    set(snapSettingsAtom, { ...get(snapSettingsAtom), interval: Math.max(1, interval) });
  }
);

// Action: Add marker
export const addMarkerAtom = atom(
  null,
  (get, set, { frame, name, color = 'yellow' }: { frame: number; name?: string; color?: MarkerColor }) => {
    const id = `marker-${nanoid()}`;
    set(markersAtom, produce(get(markersAtom), (draft) => {
      // Toggle: remove if marker exists at this frame
      const existingIndex = draft.findIndex((m) => m.frame === frame);
      if (existingIndex !== -1) {
        draft.splice(existingIndex, 1);
        return;
      }
      draft.push({
        id,
        frame,
        name: name || `Marker ${draft.length + 1}`,
        color,
      });
      draft.sort((a, b) => a.frame - b.frame);
    }));
    return id;
  }
);

// Action: Remove marker
export const removeMarkerAtom = atom(
  null,
  (get, set, markerId: string) => {
    set(markersAtom, get(markersAtom).filter((m) => m.id !== markerId));
  }
);

// Action: Clear in/out points
export const clearInOutPointsAtom = atom(
  null,
  (get, set) => {
    set(inPointAtom, null);
    set(outPointAtom, null);
  }
);

// Action: Set exporting state
export const setExportingAtom = atom(
  null,
  (get, set, { exporting, progress = 0 }: { exporting: boolean; progress?: number }) => {
    set(isExportingAtom, exporting);
    set(exportProgressAtom, progress);
  }
);

// Import for marker navigation
import { currentFrameAtom } from './playback';

// Action: Go to next marker
export const goToNextMarkerAtom = atom(
  null,
  (get, set) => {
    const markers = get(markersAtom);
    const currentFrame = get(currentFrameAtom);

    // Find next marker after current frame
    const nextMarker = markers.find((m) => m.frame > currentFrame);
    if (nextMarker) {
      set(currentFrameAtom, nextMarker.frame);
    }
  }
);

// Action: Go to previous marker
export const goToPrevMarkerAtom = atom(
  null,
  (get, set) => {
    const markers = get(markersAtom);
    const currentFrame = get(currentFrameAtom);

    // Find previous marker before current frame
    const prevMarkers = markers.filter((m) => m.frame < currentFrame);
    if (prevMarkers.length > 0) {
      const prevMarker = prevMarkers[prevMarkers.length - 1];
      set(currentFrameAtom, prevMarker.frame);
    }
  }
);
