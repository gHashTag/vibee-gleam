import { create } from 'zustand';
import { persist, subscribeWithSelector } from 'zustand/middleware';
import { immer } from 'zustand/middleware/immer';
import { nanoid } from 'nanoid';
import type {
  EditorStore,
  Project,
  Track,
  TrackItem,
  TrackType,
  Asset,
  LipSyncMainProps,
  Marker,
  MarkerColor,
} from './types';
import { useHistoryStore, extractHistorySnapshot } from './history';

// ===============================
// Default Values
// ===============================

const DEFAULT_PROJECT: Project = {
  id: nanoid(),
  name: 'Untitled Project',
  fps: 30,
  width: 1080,
  height: 1920,
  durationInFrames: 900, // 30 seconds at 30fps
};

// Helper to create initial tracks from template props
function createTracksFromTemplate(
  templateProps: LipSyncMainProps,
  fps: number,
  durationInFrames: number
): Track[] {
  const coverFrames = Math.floor(templateProps.coverDuration * fps);
  const gapFrames = Math.floor(1.5 * fps); // 1.5 sec gap between segments
  const segmentFrames = Math.floor(4 * fps); // 4 sec per background

  // Video track: background videos as segments
  const videoItems: TrackItem[] = [];
  let currentFrame = coverFrames + gapFrames;

  templateProps.backgroundVideos.forEach((url, index) => {
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

  // Avatar track: lipsync video full duration
  const avatarItems: TrackItem[] = [
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
      circleSizePercent: templateProps.circleSizePercent,
      circleBottomPercent: templateProps.circleBottomPercent,
      circleLeftPx: templateProps.circleLeftPx,
    },
  ];

  // Audio track: background music if set
  const audioItems: TrackItem[] = templateProps.backgroundMusic
    ? [
        {
          id: 'item-music',
          trackId: 'track-audio',
          type: 'audio',
          startFrame: 0,
          durationInFrames: durationInFrames,
          x: 0,
          y: 0,
          width: 0,
          height: 0,
          rotation: 0,
          opacity: 1,
          volume: templateProps.musicVolume,
        },
      ]
    : [];

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
      items: avatarItems,
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
      items: audioItems,
      locked: false,
      visible: true,
    },
  ];
}

const DEFAULT_TRACKS: Track[] = createTracksFromTemplate(
  {
    lipSyncVideo: '/lipsync/lipsync.mp4',
    coverImage: '/covers/cover.jpeg',
    backgroundMusic: '',
    backgroundVideos: [
      '/backgrounds/business/00.mp4',
      '/backgrounds/business/01.mp4',
      '/backgrounds/business/02.mp4',
      '/backgrounds/business/03.mp4',
      '/backgrounds/business/04.mp4',
    ],
    musicVolume: 0,
    coverDuration: 0.5,
    vignetteStrength: 0.7,
    colorCorrection: 1.2,
    circleSizePercent: 25.2,
    circleBottomPercent: 15,
    circleLeftPx: 40,
  },
  30, // fps
  900 // durationInFrames (30 sec)
);

// Default template props for LipSyncMain
const DEFAULT_TEMPLATE_PROPS: LipSyncMainProps = {
  lipSyncVideo: '/lipsync/lipsync.mp4',
  coverImage: '/covers/cover.jpeg',
  backgroundMusic: '',
  backgroundVideos: [
    '/backgrounds/business/00.mp4',
    '/backgrounds/business/01.mp4',
    '/backgrounds/business/02.mp4',
    '/backgrounds/business/03.mp4',
    '/backgrounds/business/04.mp4',
  ],
  musicVolume: 0,
  coverDuration: 0.5,
  vignetteStrength: 0.7,
  colorCorrection: 1.2,
  circleSizePercent: 25.2,
  circleBottomPercent: 15,
  circleLeftPx: 40,
  // Default captions (TikTok-style)
  captions: [],
  captionStyle: {
    fontSize: 48,
    textColor: '#ffffff',
    highlightColor: '#f59e0b', // VIBEE amber
    backgroundColor: 'rgba(0, 0, 0, 0.6)',
    bottomPercent: 20,
    maxWidthPercent: 85,
    fontWeight: 700,
    showShadow: true,
  },
  showCaptions: true,
};

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
];

// ===============================
// Store
// ===============================

export const useEditorStore = create<EditorStore>()(
  subscribeWithSelector(
  persist(
    immer((set, get) => ({
      // ========== State ==========
      project: DEFAULT_PROJECT,
      tracks: DEFAULT_TRACKS,
      currentFrame: 0,
      isPlaying: false,
      playbackRate: 1,
      timelineZoom: 1,
      selectedItemIds: [],
      selectionAnchor: null,
      clipboard: [],
      assets: DEFAULT_ASSETS,
      templateProps: DEFAULT_TEMPLATE_PROPS,
      sidebarTab: 'assets',
      canvasZoom: 0, // 0 = auto fit to height
      isExporting: false,
      exportProgress: 0,
      snapSettings: { enabled: false, interval: 15 }, // 0.5s at 30fps
      inPoint: null,
      outPoint: null,
      markers: [],

      // ========== Undo/Redo ==========
      undo: () => {
        const historyStore = useHistoryStore.getState();
        const snapshot = historyStore.undo();
        if (snapshot) {
          historyStore.setApplying(true);
          set((state) => {
            state.tracks = snapshot.tracks;
            state.assets = snapshot.assets;
            state.templateProps = snapshot.templateProps;
            state.project = snapshot.project;
          });
          historyStore.setApplying(false);
        }
      },

      redo: () => {
        const historyStore = useHistoryStore.getState();
        const snapshot = historyStore.redo();
        if (snapshot) {
          historyStore.setApplying(true);
          set((state) => {
            state.tracks = snapshot.tracks;
            state.assets = snapshot.assets;
            state.templateProps = snapshot.templateProps;
            state.project = snapshot.project;
          });
          historyStore.setApplying(false);
        }
      },

      canUndo: () => useHistoryStore.getState().canUndo(),
      canRedo: () => useHistoryStore.getState().canRedo(),

      // ========== Project ==========
      setProject: (updates) =>
        set((state) => {
          Object.assign(state.project, updates);
        }),

      // ========== Playback ==========
      setCurrentFrame: (frame) =>
        set((state) => {
          state.currentFrame = Math.max(0, Math.min(frame, state.project.durationInFrames - 1));
        }),

      setIsPlaying: (playing) =>
        set((state) => {
          state.isPlaying = playing;
        }),

      setPlaybackRate: (rate) =>
        set((state) => {
          state.playbackRate = Math.max(0.25, Math.min(rate, 2));
        }),

      play: () =>
        set((state) => {
          state.isPlaying = true;
        }),

      pause: () =>
        set((state) => {
          state.isPlaying = false;
        }),

      seekTo: (frame) =>
        set((state) => {
          state.currentFrame = Math.max(0, Math.min(frame, state.project.durationInFrames - 1));
          state.isPlaying = false;
        }),

      // ========== Tracks ==========
      addTrack: (type, name) => {
        const id = `track-${nanoid()}`;
        set((state) => {
          state.tracks.push({
            id,
            type,
            name: name || type.charAt(0).toUpperCase() + type.slice(1),
            items: [],
            locked: false,
            visible: true,
          });
        });
        return id;
      },

      removeTrack: (trackId) =>
        set((state) => {
          state.tracks = state.tracks.filter((t) => t.id !== trackId);
          // Remove selected items from this track
          const track = state.tracks.find((t) => t.id === trackId);
          if (track) {
            const itemIds = track.items.map((i) => i.id);
            state.selectedItemIds = state.selectedItemIds.filter(
              (id) => !itemIds.includes(id)
            );
          }
        }),

      updateTrack: (trackId, updates) =>
        set((state) => {
          const track = state.tracks.find((t) => t.id === trackId);
          if (track) {
            Object.assign(track, updates);
          }
        }),

      // ========== Items ==========
      addItem: (trackId, itemData) => {
        const id = `item-${nanoid()}`;
        set((state) => {
          const track = state.tracks.find((t) => t.id === trackId);
          if (track) {
            track.items.push({
              ...itemData,
              id,
              trackId,
            } as TrackItem);

            // Auto-sync backgroundVideos if video track was modified
            if (track.type === 'video') {
              const videoTrack = state.tracks.find((t) => t.type === 'video');
              if (videoTrack) {
                const sortedItems = [...videoTrack.items].sort(
                  (a, b) => a.startFrame - b.startFrame
                );
                state.templateProps.backgroundVideos = sortedItems
                  .map((item) => {
                    if (item.assetId) {
                      const asset = state.assets.find((a) => a.id === item.assetId);
                      return asset?.url;
                    }
                    return undefined;
                  })
                  .filter((url): url is string => !!url);
              }
            }
          }
        });
        return id;
      },

      updateItem: (itemId, updates) =>
        set((state) => {
          for (const track of state.tracks) {
            const item = track.items.find((i) => i.id === itemId);
            if (item) {
              Object.assign(item, updates);
              break;
            }
          }
        }),

      deleteItems: (itemIds) =>
        set((state) => {
          for (const track of state.tracks) {
            track.items = track.items.filter((i) => !itemIds.includes(i.id));
          }
          state.selectedItemIds = state.selectedItemIds.filter(
            (id) => !itemIds.includes(id)
          );

          // Auto-sync backgroundVideos after delete
          const videoTrack = state.tracks.find((t) => t.type === 'video');
          if (videoTrack) {
            const sortedItems = [...videoTrack.items].sort(
              (a, b) => a.startFrame - b.startFrame
            );
            state.templateProps.backgroundVideos = sortedItems
              .map((item) => {
                if (item.assetId) {
                  const asset = state.assets.find((a) => a.id === item.assetId);
                  return asset?.url;
                }
                return undefined;
              })
              .filter((url): url is string => !!url);
          }
        }),

      rippleDelete: (itemIds) =>
        set((state) => {
          for (const track of state.tracks) {
            // Find items to delete in this track
            const itemsToDelete = track.items.filter((i) => itemIds.includes(i.id));
            if (itemsToDelete.length === 0) continue;

            // Sort by startFrame
            itemsToDelete.sort((a, b) => a.startFrame - b.startFrame);

            // For each deleted item, calculate gap and shift subsequent items
            for (const deletedItem of itemsToDelete) {
              const gapSize = deletedItem.durationInFrames;
              const gapStart = deletedItem.startFrame;

              // Shift all items that start after the deleted item
              for (const item of track.items) {
                if (!itemIds.includes(item.id) && item.startFrame > gapStart) {
                  item.startFrame = Math.max(0, item.startFrame - gapSize);
                }
              }
            }

            // Remove the deleted items
            track.items = track.items.filter((i) => !itemIds.includes(i.id));
          }

          state.selectedItemIds = state.selectedItemIds.filter(
            (id) => !itemIds.includes(id)
          );

          // Auto-sync backgroundVideos
          const videoTrack = state.tracks.find((t) => t.type === 'video');
          if (videoTrack) {
            const sortedItems = [...videoTrack.items].sort(
              (a, b) => a.startFrame - b.startFrame
            );
            state.templateProps.backgroundVideos = sortedItems
              .map((item) => {
                if (item.assetId) {
                  const asset = state.assets.find((a) => a.id === item.assetId);
                  return asset?.url;
                }
                return undefined;
              })
              .filter((url): url is string => !!url);
          }
        }),

      duplicateItems: (itemIds) =>
        set((state) => {
          const newIds: string[] = [];
          const fps = state.project.fps;
          const offset = Math.floor(fps * 0.5); // Offset by 0.5 seconds

          for (const track of state.tracks) {
            const itemsToDuplicate = track.items.filter((i) =>
              itemIds.includes(i.id)
            );

            for (const item of itemsToDuplicate) {
              const newId = `item-${nanoid()}`;
              const duplicatedItem = {
                ...JSON.parse(JSON.stringify(item)), // Deep clone
                id: newId,
                startFrame: item.startFrame + item.durationInFrames + offset,
              };
              track.items.push(duplicatedItem);
              newIds.push(newId);
            }
          }

          // Select the new duplicated items
          state.selectedItemIds = newIds;

          // Auto-sync backgroundVideos if video track was affected
          const videoTrack = state.tracks.find((t) => t.type === 'video');
          if (videoTrack) {
            const sortedItems = [...videoTrack.items].sort(
              (a, b) => a.startFrame - b.startFrame
            );
            state.templateProps.backgroundVideos = sortedItems
              .map((item) => {
                if (item.assetId) {
                  const asset = state.assets.find((a) => a.id === item.assetId);
                  return asset?.url;
                }
                return undefined;
              })
              .filter((url): url is string => !!url);
          }
        }),

      copyItems: (itemIds) =>
        set((state) => {
          const itemsToCopy: typeof state.clipboard = [];
          for (const track of state.tracks) {
            for (const item of track.items) {
              if (itemIds.includes(item.id)) {
                itemsToCopy.push(JSON.parse(JSON.stringify(item)));
              }
            }
          }
          state.clipboard = itemsToCopy;
        }),

      pasteItems: () =>
        set((state) => {
          if (state.clipboard.length === 0) return;

          const newIds: string[] = [];
          const currentFrame = state.currentFrame;

          for (const clipboardItem of state.clipboard) {
            // Find the track for this item type
            const track = state.tracks.find((t) => t.id === clipboardItem.trackId) ||
                          state.tracks.find((t) => t.type === clipboardItem.type);
            if (!track) continue;

            const newId = `item-${nanoid()}`;
            const pastedItem = {
              ...clipboardItem,
              id: newId,
              trackId: track.id,
              startFrame: currentFrame,
            };
            track.items.push(pastedItem);
            newIds.push(newId);
          }

          // Select pasted items
          state.selectedItemIds = newIds;

          // Auto-sync backgroundVideos
          const videoTrack = state.tracks.find((t) => t.type === 'video');
          if (videoTrack) {
            const sortedItems = [...videoTrack.items].sort(
              (a, b) => a.startFrame - b.startFrame
            );
            state.templateProps.backgroundVideos = sortedItems
              .map((item) => {
                if (item.assetId) {
                  const asset = state.assets.find((a) => a.id === item.assetId);
                  return asset?.url;
                }
                return undefined;
              })
              .filter((url): url is string => !!url);
          }
        }),

      moveItem: (itemId, newStartFrame) =>
        set((state) => {
          let wasVideoTrack = false;
          // Apply snap if enabled
          let snappedFrame = newStartFrame;
          if (state.snapSettings.enabled && state.snapSettings.interval > 0) {
            snappedFrame = Math.round(newStartFrame / state.snapSettings.interval) * state.snapSettings.interval;
          }

          for (const track of state.tracks) {
            const item = track.items.find((i) => i.id === itemId);
            if (item) {
              item.startFrame = Math.max(0, snappedFrame);
              if (track.type === 'video') wasVideoTrack = true;
              break;
            }
          }

          // Auto-sync backgroundVideos after move (order may have changed)
          if (wasVideoTrack) {
            const videoTrack = state.tracks.find((t) => t.type === 'video');
            if (videoTrack) {
              const sortedItems = [...videoTrack.items].sort(
                (a, b) => a.startFrame - b.startFrame
              );
              state.templateProps.backgroundVideos = sortedItems
                .map((item) => {
                  if (item.assetId) {
                    const asset = state.assets.find((a) => a.id === item.assetId);
                    return asset?.url;
                  }
                  return undefined;
                })
                .filter((url): url is string => !!url);
            }
          }
        }),

      resizeItem: (itemId, newDuration) =>
        set((state) => {
          for (const track of state.tracks) {
            const item = track.items.find((i) => i.id === itemId);
            if (item) {
              item.durationInFrames = Math.max(1, newDuration);
              break;
            }
          }
        }),

      splitItem: (itemId, atFrame) =>
        set((state) => {
          for (const track of state.tracks) {
            const itemIndex = track.items.findIndex((i) => i.id === itemId);
            if (itemIndex === -1) continue;

            const item = track.items[itemIndex];
            const itemEnd = item.startFrame + item.durationInFrames;

            // Check if playhead is within the item bounds
            if (atFrame <= item.startFrame || atFrame >= itemEnd) continue;

            // Calculate durations for both parts
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

            // Insert second part after first
            track.items.splice(itemIndex + 1, 0, secondPart);

            // Select the second part
            state.selectedItemIds = [secondPart.id];
            break;
          }
        }),

      moveItemToTrack: (itemId, newTrackId) =>
        set((state) => {
          let movedItem: TrackItem | undefined;

          // Remove from current track
          for (const track of state.tracks) {
            const index = track.items.findIndex((i) => i.id === itemId);
            if (index !== -1) {
              movedItem = track.items[index];
              track.items.splice(index, 1);
              break;
            }
          }

          // Add to new track
          if (movedItem) {
            const newTrack = state.tracks.find((t) => t.id === newTrackId);
            if (newTrack) {
              movedItem.trackId = newTrackId;
              newTrack.items.push(movedItem);
            }
          }
        }),

      reorderItems: (trackId, activeId, overId) =>
        set((state) => {
          const track = state.tracks.find((t) => t.id === trackId);
          if (!track) return;

          const oldIndex = track.items.findIndex((i) => i.id === activeId);
          const newIndex = track.items.findIndex((i) => i.id === overId);

          if (oldIndex === -1 || newIndex === -1 || oldIndex === newIndex) return;

          // Remove from old position and insert at new position
          const [removed] = track.items.splice(oldIndex, 1);
          track.items.splice(newIndex, 0, removed);

          // Update startFrame values based on new order (for video track)
          if (track.type === 'video') {
            let currentFrame = 0;
            const fps = state.project.fps;
            const gapFrames = Math.floor(1.5 * fps); // 1.5 sec gap
            const coverFrames = Math.floor(state.templateProps.coverDuration * fps);
            currentFrame = coverFrames + gapFrames;

            for (const item of track.items) {
              item.startFrame = currentFrame;
              currentFrame += item.durationInFrames + gapFrames;
            }

            // Sync backgroundVideos after reorder
            const sortedItems = [...track.items];
            state.templateProps.backgroundVideos = sortedItems
              .map((item) => {
                if (item.assetId) {
                  const asset = state.assets.find((a) => a.id === item.assetId);
                  return asset?.url;
                }
                return undefined;
              })
              .filter((url): url is string => !!url);
          }
        }),

      // ========== Selection ==========
      selectItems: (itemIds, addToSelection = false) =>
        set((state) => {
          if (addToSelection) {
            // Add to existing selection (Cmd+click)
            const newIds = itemIds.filter(
              (id) => !state.selectedItemIds.includes(id)
            );
            state.selectedItemIds.push(...newIds);
          } else {
            state.selectedItemIds = itemIds;
          }
          // Set anchor to first selected item
          if (itemIds.length > 0) {
            state.selectionAnchor = itemIds[0];
          }
        }),

      selectRange: (toItemId) =>
        set((state) => {
          if (!state.selectionAnchor) {
            // No anchor - just select this item
            state.selectedItemIds = [toItemId];
            state.selectionAnchor = toItemId;
            return;
          }

          // Find all items sorted by timeline position
          const allItems: { id: string; startFrame: number; trackId: string }[] = [];
          for (const track of state.tracks) {
            for (const item of track.items) {
              allItems.push({ id: item.id, startFrame: item.startFrame, trackId: track.id });
            }
          }
          allItems.sort((a, b) => a.startFrame - b.startFrame);

          // Find anchor and target indices
          const anchorIdx = allItems.findIndex((i) => i.id === state.selectionAnchor);
          const targetIdx = allItems.findIndex((i) => i.id === toItemId);

          if (anchorIdx === -1 || targetIdx === -1) {
            state.selectedItemIds = [toItemId];
            state.selectionAnchor = toItemId;
            return;
          }

          // Select all items between anchor and target (inclusive)
          const start = Math.min(anchorIdx, targetIdx);
          const end = Math.max(anchorIdx, targetIdx);
          state.selectedItemIds = allItems.slice(start, end + 1).map((i) => i.id);
        }),

      clearSelection: () =>
        set((state) => {
          state.selectedItemIds = [];
        }),

      selectAll: () =>
        set((state) => {
          state.selectedItemIds = state.tracks.flatMap((t) =>
            t.items.map((i) => i.id)
          );
        }),

      // ========== Assets ==========
      addAsset: (assetData) => {
        const id = `asset-${nanoid()}`;
        set((state) => {
          state.assets.push({ ...assetData, id });
        });
        return id;
      },

      removeAsset: (assetId) =>
        set((state) => {
          state.assets = state.assets.filter((a) => a.id !== assetId);
        }),

      // ========== Timeline ==========
      setTimelineZoom: (zoom) =>
        set((state) => {
          state.timelineZoom = Math.max(0.1, Math.min(zoom, 5));
        }),

      // ========== Canvas ==========
      setCanvasZoom: (zoom) =>
        set((state) => {
          state.canvasZoom = Math.max(0.1, Math.min(zoom, 2));
        }),

      // ========== UI ==========
      setSidebarTab: (tab) =>
        set((state) => {
          state.sidebarTab = tab;
        }),

      // ========== Snap ==========
      setSnapEnabled: (enabled) =>
        set((state) => {
          state.snapSettings.enabled = enabled;
        }),

      setSnapInterval: (frames) =>
        set((state) => {
          state.snapSettings.interval = Math.max(1, frames);
        }),

      // ========== In/Out Points ==========
      setInPoint: (frame) =>
        set((state) => {
          state.inPoint = frame;
        }),

      setOutPoint: (frame) =>
        set((state) => {
          state.outPoint = frame;
        }),

      clearInOutPoints: () =>
        set((state) => {
          state.inPoint = null;
          state.outPoint = null;
        }),

      // ========== Markers ==========
      addMarker: (frame, name, color = 'yellow') => {
        const id = `marker-${nanoid()}`;
        set((state) => {
          // Check if marker already exists at this frame
          const existingIndex = state.markers.findIndex((m) => m.frame === frame);
          if (existingIndex !== -1) {
            // Remove existing marker at this frame (toggle behavior)
            state.markers.splice(existingIndex, 1);
            return;
          }
          state.markers.push({
            id,
            frame,
            name: name || `Marker ${state.markers.length + 1}`,
            color,
          });
          // Sort markers by frame
          state.markers.sort((a, b) => a.frame - b.frame);
        });
        return id;
      },

      removeMarker: (markerId) =>
        set((state) => {
          state.markers = state.markers.filter((m) => m.id !== markerId);
        }),

      updateMarker: (markerId, updates) =>
        set((state) => {
          const marker = state.markers.find((m) => m.id === markerId);
          if (marker) {
            Object.assign(marker, updates);
          }
        }),

      goToNextMarker: () =>
        set((state) => {
          const nextMarker = state.markers.find((m) => m.frame > state.currentFrame);
          if (nextMarker) {
            state.currentFrame = nextMarker.frame;
          }
        }),

      goToPrevMarker: () =>
        set((state) => {
          // Find markers before current frame
          const prevMarkers = state.markers.filter((m) => m.frame < state.currentFrame);
          if (prevMarkers.length > 0) {
            state.currentFrame = prevMarkers[prevMarkers.length - 1].frame;
          }
        }),

      // ========== Export ==========
      setExporting: (exporting, progress = 0) =>
        set((state) => {
          state.isExporting = exporting;
          state.exportProgress = progress;
        }),

      // ========== Utils ==========
      getItemById: (itemId) => {
        const state = get();
        for (const track of state.tracks) {
          const item = track.items.find((i) => i.id === itemId);
          if (item) return item;
        }
        return undefined;
      },

      getTrackById: (trackId) => {
        const state = get();
        return state.tracks.find((t) => t.id === trackId);
      },

      getAssetById: (assetId) => {
        const state = get();
        return state.assets.find((a) => a.id === assetId);
      },

      getSelectedItems: () => {
        const state = get();
        const items: TrackItem[] = [];
        for (const track of state.tracks) {
          for (const item of track.items) {
            if (state.selectedItemIds.includes(item.id)) {
              items.push(item);
            }
          }
        }
        return items;
      },

      // ========== Template Props ==========
      updateTemplateProp: (key, value) =>
        set((state) => {
          (state.templateProps as any)[key] = value;
        }),

      syncBackgroundVideosFromTimeline: () =>
        set((state) => {
          const videoTrack = state.tracks.find((t) => t.type === 'video');
          if (!videoTrack) return;

          // Sort items by startFrame to maintain order
          const sortedItems = [...videoTrack.items].sort(
            (a, b) => a.startFrame - b.startFrame
          );

          // Extract URLs from assets
          const backgroundVideos = sortedItems
            .map((item) => {
              if (item.assetId) {
                const asset = state.assets.find((a) => a.id === item.assetId);
                return asset?.url;
              }
              return undefined;
            })
            .filter((url): url is string => !!url);

          state.templateProps.backgroundVideos = backgroundVideos;
        }),
    })),
    {
      name: 'vibee-editor-storage-v4', // Bump version for auto-populated tracks
      partialize: (state) => ({
        project: state.project,
        tracks: state.tracks,
        assets: state.assets,
        templateProps: state.templateProps,
        timelineZoom: state.timelineZoom,
        canvasZoom: state.canvasZoom,
        playbackRate: state.playbackRate,
      }),
    }
  ))
);

// ===============================
// History Subscription
// ===============================

// Subscribe to state changes and record snapshots
// Only record changes to tracks, assets, templateProps, project
let historyDebounceTimer: ReturnType<typeof setTimeout> | null = null;

useEditorStore.subscribe(
  (state) => ({
    tracks: state.tracks,
    assets: state.assets,
    templateProps: state.templateProps,
    project: state.project,
  }),
  (slice) => {
    // Debounce to avoid recording every keystroke
    if (historyDebounceTimer) {
      clearTimeout(historyDebounceTimer);
    }
    historyDebounceTimer = setTimeout(() => {
      const historyStore = useHistoryStore.getState();
      if (!historyStore.isApplying) {
        historyStore.record(extractHistorySnapshot(useEditorStore.getState()));
      }
    }, 300);
  },
  { equalityFn: (a, b) => JSON.stringify(a) === JSON.stringify(b) }
);

// Record initial state
setTimeout(() => {
  useHistoryStore.getState().record(extractHistorySnapshot(useEditorStore.getState()));
}, 100);

// ===============================
// Legacy exports for compatibility
// ===============================

export type TabId = 'assets' | 'properties';

// Re-export LipSyncMainProps from types
export type { LipSyncMainProps } from './types';

// Hook to get template props directly from store
export function useLipSyncProps() {
  return useEditorStore((s) => s.templateProps);
}
