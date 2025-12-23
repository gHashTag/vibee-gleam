import { useState, useCallback, memo } from 'react';
import { useAtomValue, useSetAtom } from 'jotai';
import { selectedItemIdsAtom, addItemAtom, updateItemAtom, lipSyncVideoAtom } from '@/atoms';
import { TrackItem } from './TrackItem';
import type { Track as TrackType, Asset } from '@/store/types';

interface TrackProps {
  track: TrackType;
  pxPerFrame: number;
}

export const Track = memo(function Track({ track, pxPerFrame }: TrackProps) {
  const selectedItemIds = useAtomValue(selectedItemIdsAtom);
  const addItem = useSetAtom(addItemAtom);
  const updateItem = useSetAtom(updateItemAtom);
  const setLipSyncVideo = useSetAtom(lipSyncVideoAtom);
  const [isDragOver, setIsDragOver] = useState(false);

  const handleDragOver = useCallback((e: React.DragEvent) => {
    e.preventDefault();
    e.dataTransfer.dropEffect = 'copy';
    setIsDragOver(true);
  }, []);

  const handleDragLeave = useCallback(() => {
    setIsDragOver(false);
  }, []);

  const handleDrop = useCallback((e: React.DragEvent) => {
    e.preventDefault();
    setIsDragOver(false);

    try {
      const data = e.dataTransfer.getData('application/json');
      if (!data) return;

      const asset: Asset = JSON.parse(data);

      // Calculate drop position in frames
      const rect = e.currentTarget.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const dropFrame = Math.max(0, Math.round(x / pxPerFrame));

      // Check if asset type matches track type
      const isCompatible =
        (track.type === 'video' && (asset.type === 'video' || asset.type === 'image')) ||
        (track.type === 'audio' && asset.type === 'audio') ||
        (track.type === 'avatar' && asset.type === 'video') ||
        (track.type === 'image' && asset.type === 'image');

      if (!isCompatible) {
        console.warn(`Asset type "${asset.type}" not compatible with track "${track.name}"`);
        // Flash red to indicate rejection
        e.currentTarget.classList.add('drop-rejected');
        setTimeout(() => e.currentTarget.classList.remove('drop-rejected'), 300);
        return;
      }

      // Check if dropping on an existing item - REPLACE instead of ADD
      const existingItem = track.items.find(
        (item) =>
          dropFrame >= item.startFrame &&
          dropFrame < item.startFrame + item.durationInFrames
      );

      if (existingItem) {
        // REPLACE: Update existing item's asset
        console.log(`Replacing ${existingItem.id} with ${asset.name}`);
        updateItem({ itemId: existingItem.id, updates: { assetId: asset.id } });

        // Update lipSyncVideoAtom if this is avatar track
        if (track.type === 'avatar' && asset.url) {
          console.log(`[Track] Avatar video changed to: ${asset.url}`);
          setLipSyncVideo(asset.url);
        }
        return;
      }

      // ADD NEW: Create item based on asset type
      const itemType = asset.type === 'image' ? 'image' : track.type;

      addItem({
        trackId: track.id,
        itemData: {
          type: itemType as any,
          assetId: asset.id,
          startFrame: dropFrame,
          durationInFrames: asset.duration || 150, // Default 5 seconds at 30fps
          x: 0,
          y: 0,
          width: asset.width || 1080,
          height: asset.height || 1920,
          rotation: 0,
          opacity: 1,
          ...(itemType === 'video' && { volume: 1, playbackRate: 1 }),
          ...(itemType === 'audio' && { volume: 1 }),
          ...(itemType === 'avatar' && {
            circleSizePercent: 25.2,
            circleBottomPercent: 15,
            circleLeftPx: 40,
          }),
        },
      });

      // Update lipSyncVideoAtom if this is avatar track
      if (track.type === 'avatar' && asset.url) {
        console.log(`[Track] Avatar video set to: ${asset.url}`);
        setLipSyncVideo(asset.url);
      }

      console.log(`Added ${asset.name} to ${track.name} at frame ${dropFrame}`);
    } catch (error) {
      console.error('Drop error:', error);
    }
  }, [track, pxPerFrame, addItem, updateItem, setLipSyncVideo]);

  return (
    <div
      className={`track-row ${isDragOver ? 'drag-over' : ''}`}
      data-track-id={track.id}
      onDragOver={handleDragOver}
      onDragLeave={handleDragLeave}
      onDrop={handleDrop}
    >
      {track.items.map((item) => (
        <TrackItem
          key={item.id}
          item={item}
          pxPerFrame={pxPerFrame}
          isSelected={selectedItemIds.includes(item.id)}
          isLocked={track.locked}
        />
      ))}
    </div>
  );
}, (prevProps, nextProps) => {
  // Shallow compare track items
  if (prevProps.pxPerFrame !== nextProps.pxPerFrame) return false;
  if (prevProps.track.id !== nextProps.track.id) return false;
  if (prevProps.track.locked !== nextProps.track.locked) return false;
  if (prevProps.track.items.length !== nextProps.track.items.length) return false;

  // Check if items changed (positions/durations/assets)
  for (let i = 0; i < prevProps.track.items.length; i++) {
    const prev = prevProps.track.items[i];
    const next = nextProps.track.items[i];
    if (
      prev.id !== next.id ||
      prev.startFrame !== next.startFrame ||
      prev.durationInFrames !== next.durationInFrames ||
      prev.assetId !== next.assetId
    ) {
      return false;
    }
  }

  return true;
});
