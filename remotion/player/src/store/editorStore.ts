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
  Segment,
} from './types';
import { useHistoryStore, extractHistorySnapshot } from './history';

// Helper to get video duration from URL
async function getVideoDuration(videoUrl: string): Promise<number> {
  return new Promise((resolve, reject) => {
    const video = document.createElement('video');
    video.preload = 'metadata';

    video.onloadedmetadata = () => {
      resolve(video.duration);
      video.remove();
    };

    video.onerror = () => {
      reject(new Error(`Failed to load video: ${videoUrl}`));
      video.remove();
    };

    // Handle relative URLs
    if (videoUrl.startsWith('/')) {
      // Use render server URL for local files
      const RENDER_SERVER_URL = import.meta.env.VITE_RENDER_SERVER_URL || 'https://vibee-remotion.fly.dev';
      video.src = `${RENDER_SERVER_URL}${videoUrl}`;
    } else {
      video.src = videoUrl;
    }
  });
}

// ===============================
// Default Values
// ===============================

const DEFAULT_PROJECT: Project = {
  id: nanoid(),
  name: 'Untitled Project',
  fps: 30,
  width: 1080,
  height: 1920,
  durationInFrames: 825, // ~27.5 seconds at 30fps (matches lipsync.mp4)
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
    backgroundMusic: '/audio/music/phonk_01.mp3',
    backgroundVideos: [
      '/backgrounds/business/00.mp4',
      '/backgrounds/business/01.mp4',
      '/backgrounds/business/02.mp4',
      '/backgrounds/business/03.mp4',
      '/backgrounds/business/04.mp4',
    ],
    musicVolume: 0.8, // Background music - loud
    coverDuration: 0.5,
    vignetteStrength: 0.7,
    colorCorrection: 1.2,
    circleSizePercent: 25.2,
    circleBottomPercent: 15,
    circleLeftPx: 40,
  },
  30, // fps
  825 // durationInFrames (~27.5 sec, matches lipsync.mp4)
);

// Default template props for LipSyncMain
const DEFAULT_TEMPLATE_PROPS: LipSyncMainProps = {
  lipSyncVideo: '/lipsync/lipsync.mp4',
  coverImage: '/covers/cover.jpeg',
  backgroundMusic: '/audio/music/phonk_01.mp3',
  backgroundVideos: [
    '/backgrounds/business/00.mp4',
    '/backgrounds/business/01.mp4',
    '/backgrounds/business/02.mp4',
    '/backgrounds/business/03.mp4',
    '/backgrounds/business/04.mp4',
  ],
  musicVolume: 0.15, // Background music - subtle
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
  {
    id: 'asset-music-phonk',
    type: 'audio',
    name: 'Phonk Music',
    url: '/audio/music/phonk_01.mp3',
    duration: 1800, // ~60 seconds at 30fps
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
      isMuted: false,
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

      setIsMuted: (muted) =>
        set((state) => {
          state.isMuted = muted;
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

      // Player ref for direct control (needed for autoplay policy)
      playerRef: null as any,
      setPlayerRef: (ref: any) => {
        console.log('[setPlayerRef] Setting playerRef:', ref);
        set((state) => {
          state.playerRef = ref;
        });
      },

      // Direct play/pause that calls playerRef (for user gesture context)
      // CRITICAL: event parameter is required for browser autoplay policy!
      // See: https://www.remotion.dev/docs/player/autoplay
      playDirect: (event?: React.MouseEvent) => {
        const state = get();
        if (state.playerRef?.current) {
          const player = state.playerRef.current;
          if (player.unmute) player.unmute();
          if (player.setVolume) player.setVolume(1);
          player.play(event);
        }
        set((s) => { s.isPlaying = true; });
      },

      pauseDirect: () => {
        const state = get();
        if (state.playerRef?.current) {
          console.log('[pauseDirect] Calling player.pause()');
          state.playerRef.current.pause();
        }
        set((s) => { s.isPlaying = false; });
      },

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

              // Auto-sync backgroundVideos if video track item was updated (e.g., assetId changed)
              if (track.type === 'video' && updates.assetId) {
                const videoTrack = state.tracks.find((t) => t.type === 'video');
                if (videoTrack) {
                  const sortedItems = [...videoTrack.items].sort(
                    (a, b) => a.startFrame - b.startFrame
                  );
                  state.templateProps.backgroundVideos = sortedItems
                    .map((i) => {
                      if (i.assetId) {
                        const asset = state.assets.find((a) => a.id === i.assetId);
                        return asset?.url;
                      }
                      return undefined;
                    })
                    .filter((url): url is string => !!url);
                }
              }
              break;
            }
          }
        }),

      deleteItems: (itemIds) => {
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
        });
        // Clean up any orphaned assets after deletion
        get().cleanupOrphanedAssets();
      },

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

      // Clean up orphaned assets (assets not referenced by any track item)
      cleanupOrphanedAssets: () =>
        set((state) => {
          // Collect all used assetIds from track items
          const usedAssetIds = new Set<string>();
          for (const track of state.tracks) {
            for (const item of track.items) {
              if (item.assetId) usedAssetIds.add(item.assetId);
            }
          }

          // Default assets that should never be removed
          const defaultAssetIds = [
            'asset-lipsync',
            'asset-cover',
            'asset-music-phonk',
            'asset-music-business',
            'asset-music-corporate',
            'asset-music-upbeat',
          ];

          // Filter: keep used + defaults
          const before = state.assets.length;
          state.assets = state.assets.filter(
            (a) => usedAssetIds.has(a.id) || defaultAssetIds.includes(a.id)
          );
          const after = state.assets.length;
          if (before !== after) {
            console.log(`[cleanupOrphanedAssets] Removed ${before - after} orphaned assets`);
          }
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

      getSegmentsFromTimeline: (): Segment[] => {
        const state = get();
        const videoTrack = state.tracks.find((t) => t.type === 'video');
        if (!videoTrack) return [];

        const sortedItems = [...videoTrack.items].sort(
          (a, b) => a.startFrame - b.startFrame
        );
        const segments: Segment[] = [];
        let lastEndFrame = 0;

        sortedItems.forEach((item) => {
          // Add fullscreen gap if there's a gap before this item
          if (item.startFrame > lastEndFrame) {
            segments.push({
              type: 'fullscreen',
              startFrame: lastEndFrame,
              durationFrames: item.startFrame - lastEndFrame,
            });
          }

          // Add split segment for B-roll
          const asset = state.assets.find((a) => a.id === item.assetId);
          segments.push({
            type: 'split',
            startFrame: item.startFrame,
            durationFrames: item.durationInFrames,
            bRollUrl: asset?.url,
            bRollType: asset?.type === 'image' ? 'image' : 'video',
          });

          lastEndFrame = item.startFrame + item.durationInFrames;
        });

        // Add final fullscreen segment if needed
        if (lastEndFrame < state.project.durationInFrames) {
          segments.push({
            type: 'fullscreen',
            startFrame: lastEndFrame,
            durationFrames: state.project.durationInFrames - lastEndFrame,
          });
        }

        return segments;
      },

      // ========== Template Props ==========
      updateTemplateProp: (key, value) => {
        console.log(`[Store] updateTemplateProp called: ${String(key)} =`, value);
        set((state) => {
          console.log(`[Store] Setting templateProps.${String(key)}`);
          (state.templateProps as any)[key] = value;
        });
        console.log(`[Store] updateTemplateProp done: ${String(key)}`);
      },

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

      // Reset to default template state
      resetToDefaults: () => {
        set((state) => {
          state.tracks = createTracksFromTemplate(
            DEFAULT_TEMPLATE_PROPS,
            DEFAULT_PROJECT.fps,
            DEFAULT_PROJECT.durationInFrames
          );
          state.assets = [...DEFAULT_ASSETS];
          state.templateProps = { ...DEFAULT_TEMPLATE_PROPS };
          state.project = { ...DEFAULT_PROJECT, id: state.project.id, name: state.project.name };
          state.selectedItemIds = [];
          state.clipboard = [];
          state.currentFrame = 0;
          state.isPlaying = false;
          state.markers = [];
          state.inPoint = null;
          state.outPoint = null;
        });
        // Reload captions and duration from server after reset
        get().loadCaptions();
        get().updateDurationFromLipSync();
      },

      // Auto-detect duration from lipSync video
      updateDurationFromLipSync: async () => {
        const state = get();
        const videoUrl = state.templateProps.lipSyncVideo;

        if (!videoUrl) return;

        try {
          console.log('[Duration] Getting duration for:', videoUrl);
          const duration = await getVideoDuration(videoUrl);
          const fps = state.project.fps;
          const durationInFrames = Math.ceil(duration * fps);

          console.log(`[Duration] Video: ${duration.toFixed(2)}s = ${durationInFrames} frames`);

          set((state) => {
            state.project.durationInFrames = durationInFrames;

            // Also update Avatar track duration
            const avatarTrack = state.tracks.find((t) => t.type === 'avatar');
            if (avatarTrack && avatarTrack.items.length > 0) {
              avatarTrack.items[0].durationInFrames = durationInFrames;
            }
          });
        } catch (error) {
          console.error('[Duration] Failed to get video duration:', error);
        }
      },

      // Load captions from captions.json next to lipsync video
      loadCaptions: async () => {
        console.log('[loadCaptions] ========== START ==========');
        const state = get();
        const videoUrl = state.templateProps.lipSyncVideo;
        console.log('[loadCaptions] Current lipSyncVideo:', videoUrl);
        console.log('[loadCaptions] Current captions count:', state.templateProps.captions?.length || 0);

        if (!videoUrl) {
          console.log('[loadCaptions] No videoUrl, returning early');
          return;
        }

        // Build captions URL (same directory as lipsync video)
        const videoDir = videoUrl.substring(0, videoUrl.lastIndexOf('/'));
        const captionsUrl = `${videoDir}/captions.json`;
        console.log('[loadCaptions] videoDir:', videoDir);
        console.log('[loadCaptions] captionsUrl:', captionsUrl);

        const RENDER_SERVER_URL = import.meta.env.VITE_RENDER_SERVER_URL || 'https://vibee-remotion.fly.dev';
        const fullUrl = captionsUrl.startsWith('/')
          ? `${RENDER_SERVER_URL}${captionsUrl}`
          : captionsUrl;
        console.log('[loadCaptions] RENDER_SERVER_URL:', RENDER_SERVER_URL);
        console.log('[loadCaptions] fullUrl:', fullUrl);

        try {
          // Cache-busting: add timestamp to force fresh fetch
          const urlWithCacheBust = `${fullUrl}?_=${Date.now()}`;
          console.log('[loadCaptions] Fetching with cache-bust:', urlWithCacheBust);
          const response = await fetch(urlWithCacheBust);

          if (!response.ok) {
            console.warn('[Captions] No captions.json found at:', fullUrl);
            console.warn('[Captions] Clearing old captions - user needs to transcribe');
            // Clear old captions when new video doesn't have captions.json
            set((state) => {
              state.templateProps.captions = [];
            });
            return;
          }

          const captions = await response.json();

          if (Array.isArray(captions) && captions.length > 0) {
            console.log(`[loadCaptions] Loaded ${captions.length} captions, first:`, captions[0]);
            set((state) => {
              state.templateProps.captions = captions;
            });
            console.log('[loadCaptions] ========== SUCCESS ==========');
          } else {
            console.warn('[loadCaptions] Empty or invalid captions, clearing');
            set((state) => {
              state.templateProps.captions = [];
            });
            console.log('[loadCaptions] ========== CLEARED (empty) ==========');
          }
        } catch (error) {
          console.error('[loadCaptions] Failed to load captions:', error);
          // Clear old captions on error
          set((state) => {
            state.templateProps.captions = [];
          });
          console.log('[loadCaptions] ========== ERROR ==========');
        }
      },
    })),
    {
      name: 'vibee-editor-storage-v12', // v12: louder audio (0.8 instead of 0.15)
      partialize: (state) => ({
        project: state.project,
        tracks: state.tracks,
        assets: state.assets,
        templateProps: {
          ...state.templateProps,
          captions: [], // Don't persist captions - always load fresh from server
        },
        timelineZoom: state.timelineZoom,
        canvasZoom: state.canvasZoom,
        playbackRate: state.playbackRate,
      }),
      // Migration: ensure Audio track exists
      merge: (persistedState: any, currentState) => {
        console.log('[MERGE] persistedState tracks:', persistedState?.tracks?.length, persistedState?.tracks?.map((t: any) => t.type));
        console.log('[MERGE] currentState tracks:', currentState?.tracks?.length, currentState?.tracks?.map((t: any) => t.type));
        const state = { ...currentState, ...persistedState };
        console.log('[MERGE] merged tracks:', state.tracks?.length, state.tracks?.map((t: any) => t.type));
        // Add Audio track if missing
        if (state.tracks && !state.tracks.find((t: Track) => t.type === 'audio')) {
          console.log('[MERGE] Adding missing Audio track!');
          state.tracks = [
            ...state.tracks,
            {
              id: 'track-audio',
              type: 'audio',
              name: 'Audio',
              items: [{
                id: 'item-music',
                trackId: 'track-audio',
                assetId: 'asset-music-phonk',
                type: 'audio',
                startFrame: 0,
                durationInFrames: state.project?.durationInFrames || 825,
                x: 0, y: 0, width: 0, height: 0,
                rotation: 0, opacity: 1,
                volume: 0.8,
              }],
              locked: false,
              visible: true,
            },
          ];
        }

        // Validate assetId references - remove items with non-existent asset
        if (state.tracks && state.assets) {
          const assetIds = new Set(state.assets.map((a: Asset) => a.id));
          let removedCount = 0;
          state.tracks = state.tracks.map((track: Track) => ({
            ...track,
            items: track.items.filter((item: TrackItem) => {
              // Keep items without assetId (text items, etc.)
              if (!item.assetId) return true;
              // Keep items with valid assetId
              if (assetIds.has(item.assetId)) return true;
              // Remove items with invalid assetId
              removedCount++;
              return false;
            }),
          }));
          if (removedCount > 0) {
            console.log(`[MERGE] Removed ${removedCount} items with invalid assetId`);
          }
        }

        return state;
      },
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
// LipSyncVideo Change Subscription
// ===============================

// Auto-reload captions when lipSyncVideo changes
console.log('[Subscription] Setting up lipSyncVideo subscription...');
useEditorStore.subscribe(
  (state) => state.templateProps.lipSyncVideo,
  (lipSyncVideo, prevLipSyncVideo) => {
    console.log('[Subscription] lipSyncVideo callback triggered!');
    console.log('[Subscription] Current:', lipSyncVideo);
    console.log('[Subscription] Previous:', prevLipSyncVideo);
    console.log('[Subscription] Are different?', lipSyncVideo !== prevLipSyncVideo);

    if (lipSyncVideo !== prevLipSyncVideo && lipSyncVideo) {
      console.log('[Subscription] CHANGE DETECTED! Calling loadCaptions...');
      // Also update duration from the new video
      useEditorStore.getState().updateDurationFromLipSync();
      useEditorStore.getState().loadCaptions();
    } else {
      console.log('[Subscription] No change detected or empty video');
    }
  },
  { equalityFn: (a, b) => a === b }
);
console.log('[Subscription] lipSyncVideo subscription set up!');

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
