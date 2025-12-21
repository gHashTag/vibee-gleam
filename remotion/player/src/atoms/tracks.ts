// ===============================
// Tracks Atom - THE Single Source of Truth
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import { produce } from 'immer';
import { nanoid } from 'nanoid';
import type { Track, TrackItem, TrackType, LipSyncMainProps } from '@/store/types';

// Default template props for track generation
const DEFAULT_TEMPLATE: Partial<LipSyncMainProps> = {
  coverDuration: 0.5,
  circleSizePercent: 25.2,
  circleBottomPercent: 15,
  circleLeftPx: 40,
};

// Helper to create initial tracks
function createDefaultTracks(fps: number, durationInFrames: number): Track[] {
  const coverFrames = Math.floor((DEFAULT_TEMPLATE.coverDuration || 0.5) * fps);
  const gapFrames = Math.floor(1.5 * fps);
  const segmentFrames = Math.floor(4 * fps);

  const backgroundVideos = [
    '/backgrounds/business/00.mp4',
    '/backgrounds/business/01.mp4',
    '/backgrounds/business/02.mp4',
    '/backgrounds/business/03.mp4',
    '/backgrounds/business/04.mp4',
  ];

  // Video track items
  const videoItems: TrackItem[] = [];
  let currentFrame = coverFrames + gapFrames;

  backgroundVideos.forEach((_, index) => {
    if (currentFrame < durationInFrames) {
      const duration = Math.min(segmentFrames, durationInFrames - currentFrame);
      videoItems.push({
        id: `item-bg-${index}`,
        trackId: 'track-video',
        assetId: `asset-bg-${String(index).padStart(2, '0')}`,
        type: 'video',
        startFrame: currentFrame,
        durationInFrames: duration,
        x: 0,
        y: 0,
        width: 1080,
        height: 1920,
        rotation: 0,
        opacity: 1,
        volume: 0,
        playbackRate: 1,
      });
      currentFrame += duration + gapFrames;
    }
  });

  return [
    {
      id: 'track-video',
      type: 'video',
      name: 'Video',
      items: videoItems,
      locked: false,
      visible: true,
    },
    {
      id: 'track-avatar',
      type: 'avatar',
      name: 'Avatar',
      items: [
        {
          id: 'item-lipsync',
          trackId: 'track-avatar',
          assetId: 'asset-lipsync',
          type: 'avatar',
          startFrame: 0,
          durationInFrames: durationInFrames,
          x: 0,
          y: 0,
          width: 1080,
          height: 1920,
          rotation: 0,
          opacity: 1,
          circleSizePercent: DEFAULT_TEMPLATE.circleSizePercent || 25.2,
          circleBottomPercent: DEFAULT_TEMPLATE.circleBottomPercent || 15,
          circleLeftPx: DEFAULT_TEMPLATE.circleLeftPx || 40,
        },
      ],
      locked: false,
      visible: true,
    },
    {
      id: 'track-text',
      type: 'text',
      name: 'Text',
      items: [],
      locked: false,
      visible: true,
    },
    {
      id: 'track-audio',
      type: 'audio',
      name: 'Audio',
      items: [
        {
          id: 'item-music',
          trackId: 'track-audio',
          assetId: 'asset-music-phonk',
          type: 'audio',
          startFrame: 0,
          durationInFrames: durationInFrames,
          x: 0,
          y: 0,
          width: 0,
          height: 0,
          rotation: 0,
          opacity: 1,
          volume: 0.13,
        },
      ],
      locked: false,
      visible: true,
    },
  ];
}

const DEFAULT_TRACKS = createDefaultTracks(30, 825);

// ===============================
// Core Tracks Atom
// ===============================

export const tracksAtom = atomWithStorage<Track[]>(
  'vibee-tracks-v14',
  DEFAULT_TRACKS
);

// ===============================
// Track Selectors
// ===============================

export const videoTrackAtom = atom((get) =>
  get(tracksAtom).find((t) => t.type === 'video')
);

export const avatarTrackAtom = atom((get) =>
  get(tracksAtom).find((t) => t.type === 'avatar')
);

export const audioTrackAtom = atom((get) =>
  get(tracksAtom).find((t) => t.type === 'audio')
);

export const textTrackAtom = atom((get) =>
  get(tracksAtom).find((t) => t.type === 'text')
);

// Get track by ID
export const getTrackByIdAtom = atom((get) => {
  const tracks = get(tracksAtom);
  return (trackId: string) => tracks.find((t) => t.id === trackId);
});

// Get item by ID (searches all tracks)
export const getItemByIdAtom = atom((get) => {
  const tracks = get(tracksAtom);
  return (itemId: string) => {
    for (const track of tracks) {
      const item = track.items.find((i) => i.id === itemId);
      if (item) return item;
    }
    return undefined;
  };
});

// ===============================
// Track Actions
// ===============================

export const addTrackAtom = atom(
  null,
  (get, set, { type, name }: { type: TrackType; name?: string }) => {
    const id = `track-${nanoid()}`;
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      draft.push({
        id,
        type,
        name: name || type.charAt(0).toUpperCase() + type.slice(1),
        items: [],
        locked: false,
        visible: true,
      });
    }));
    return id;
  }
);

export const removeTrackAtom = atom(
  null,
  (get, set, trackId: string) => {
    set(tracksAtom, get(tracksAtom).filter((t) => t.id !== trackId));
  }
);

export const updateTrackAtom = atom(
  null,
  (get, set, { trackId, updates }: { trackId: string; updates: Partial<Track> }) => {
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      const track = draft.find((t) => t.id === trackId);
      if (track) {
        Object.assign(track, updates);
      }
    }));
  }
);

// ===============================
// Item Actions
// ===============================

export const addItemAtom = atom(
  null,
  (get, set, { trackId, itemData }: { trackId: string; itemData: Omit<TrackItem, 'id' | 'trackId'> }) => {
    const id = `item-${nanoid()}`;
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      const track = draft.find((t) => t.id === trackId);
      if (track) {
        track.items.push({
          ...itemData,
          id,
          trackId,
        } as TrackItem);
      }
    }));
    // NO MANUAL backgroundVideos SYNC - it's derived automatically!
    return id;
  }
);

export const updateItemAtom = atom(
  null,
  (get, set, { itemId, updates }: { itemId: string; updates: Partial<TrackItem> }) => {
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      for (const track of draft) {
        const item = track.items.find((i) => i.id === itemId);
        if (item) {
          Object.assign(item, updates);
          break;
        }
      }
    }));
    // NO MANUAL backgroundVideos SYNC - it's derived automatically!
  }
);

export const deleteItemsAtom = atom(
  null,
  (get, set, itemIds: string[]) => {
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      for (const track of draft) {
        track.items = track.items.filter((i) => !itemIds.includes(i.id));
      }
    }));
    // NO MANUAL backgroundVideos SYNC - it's derived automatically!
  }
);

export const moveItemAtom = atom(
  null,
  (get, set, { itemId, newStartFrame, snapSettings }: { itemId: string; newStartFrame: number; snapSettings?: { enabled: boolean; interval: number } }) => {
    let snappedFrame = newStartFrame;
    if (snapSettings?.enabled && snapSettings.interval > 0) {
      snappedFrame = Math.round(newStartFrame / snapSettings.interval) * snapSettings.interval;
    }

    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      for (const track of draft) {
        const item = track.items.find((i) => i.id === itemId);
        if (item) {
          item.startFrame = Math.max(0, snappedFrame);
          break;
        }
      }
    }));
    // NO MANUAL backgroundVideos SYNC - it's derived automatically!
  }
);

export const resizeItemAtom = atom(
  null,
  (get, set, { itemId, newDuration }: { itemId: string; newDuration: number }) => {
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      for (const track of draft) {
        const item = track.items.find((i) => i.id === itemId);
        if (item) {
          item.durationInFrames = Math.max(1, newDuration);
          break;
        }
      }
    }));
  }
);

export const splitItemAtom = atom(
  null,
  (get, set, { itemId, atFrame }: { itemId: string; atFrame: number }) => {
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      for (const track of draft) {
        const itemIndex = track.items.findIndex((i) => i.id === itemId);
        if (itemIndex === -1) continue;

        const item = track.items[itemIndex];
        const itemEnd = item.startFrame + item.durationInFrames;

        // Check if split point is within item bounds
        if (atFrame <= item.startFrame || atFrame >= itemEnd) continue;

        const firstDuration = atFrame - item.startFrame;
        const secondDuration = itemEnd - atFrame;

        // Create second part
        const secondPart = {
          ...JSON.parse(JSON.stringify(item)),
          id: `item-${nanoid()}`,
          startFrame: atFrame,
          durationInFrames: secondDuration,
        };

        // Modify first part
        item.durationInFrames = firstDuration;

        // Insert second part
        track.items.splice(itemIndex + 1, 0, secondPart);
        break;
      }
    }));
  }
);

export const duplicateItemsAtom = atom(
  null,
  (get, set, { itemIds, fps = 30 }: { itemIds: string[]; fps?: number }) => {
    const offset = Math.floor(fps * 0.5);
    const newIds: string[] = [];

    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      for (const track of draft) {
        const itemsToDuplicate = track.items.filter((i) => itemIds.includes(i.id));

        for (const item of itemsToDuplicate) {
          const newId = `item-${nanoid()}`;
          const duplicated = {
            ...JSON.parse(JSON.stringify(item)),
            id: newId,
            startFrame: item.startFrame + item.durationInFrames + offset,
          };
          track.items.push(duplicated);
          newIds.push(newId);
        }
      }
    }));

    return newIds;
  }
);

export const moveItemToTrackAtom = atom(
  null,
  (get, set, { itemId, newTrackId }: { itemId: string; newTrackId: string }) => {
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      let movedItem: TrackItem | undefined;

      // Remove from current track
      for (const track of draft) {
        const index = track.items.findIndex((i) => i.id === itemId);
        if (index !== -1) {
          movedItem = track.items[index];
          track.items.splice(index, 1);
          break;
        }
      }

      // Add to new track
      if (movedItem) {
        const newTrack = draft.find((t) => t.id === newTrackId);
        if (newTrack) {
          movedItem.trackId = newTrackId;
          newTrack.items.push(movedItem);
        }
      }
    }));
  }
);

export const rippleDeleteAtom = atom(
  null,
  (get, set, itemIds: string[]) => {
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      for (const track of draft) {
        const itemsToDelete = track.items
          .filter((i) => itemIds.includes(i.id))
          .sort((a, b) => a.startFrame - b.startFrame);

        if (itemsToDelete.length === 0) continue;

        for (const deletedItem of itemsToDelete) {
          const gapSize = deletedItem.durationInFrames;
          const gapStart = deletedItem.startFrame;

          // Shift subsequent items
          for (const item of track.items) {
            if (!itemIds.includes(item.id) && item.startFrame > gapStart) {
              item.startFrame = Math.max(0, item.startFrame - gapSize);
            }
          }
        }

        track.items = track.items.filter((i) => !itemIds.includes(i.id));
      }
    }));
  }
);

export const reorderItemsAtom = atom(
  null,
  (get, set, { trackId, activeId, overId, fps = 30, coverDuration = 0.5 }: { trackId: string; activeId: string; overId: string; fps?: number; coverDuration?: number }) => {
    set(tracksAtom, produce(get(tracksAtom), (draft) => {
      const track = draft.find((t) => t.id === trackId);
      if (!track) return;

      const oldIndex = track.items.findIndex((i) => i.id === activeId);
      const newIndex = track.items.findIndex((i) => i.id === overId);

      if (oldIndex === -1 || newIndex === -1 || oldIndex === newIndex) return;

      const [removed] = track.items.splice(oldIndex, 1);
      track.items.splice(newIndex, 0, removed);

      // Update startFrame values for video track
      if (track.type === 'video') {
        const gapFrames = Math.floor(1.5 * fps);
        const coverFrames = Math.floor(coverDuration * fps);
        let currentFrame = coverFrames + gapFrames;

        for (const item of track.items) {
          item.startFrame = currentFrame;
          currentFrame += item.durationInFrames + gapFrames;
        }
      }
    }));
  }
);

// ===============================
// Reset Action
// ===============================

export const resetTracksAtom = atom(
  null,
  (get, set, { fps = 30, durationInFrames = 825 }: { fps?: number; durationInFrames?: number }) => {
    set(tracksAtom, createDefaultTracks(fps, durationInFrames));
  }
);

export { DEFAULT_TRACKS, createDefaultTracks };
