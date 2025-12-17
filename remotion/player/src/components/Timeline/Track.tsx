import { useState, useCallback, memo } from 'react';
import { useEditorStore } from '@/store/editorStore';
import { TrackItem } from './TrackItem';
import type { Track as TrackType, Asset } from '@/store/types';

interface TrackProps {
  track: TrackType;
  pxPerFrame: number;
}

export const Track = memo(function Track({ track, pxPerFrame }: TrackProps) {
  const selectedItemIds = useEditorStore((s) => s.selectedItemIds);
  const addItem = useEditorStore((s) => s.addItem);
  const syncBackgroundVideosFromTimeline = useEditorStore((s) => s.syncBackgroundVideosFromTimeline);
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

      // Create item based on asset type
      const itemType = asset.type === 'image' ? 'image' : track.type;

      addItem(track.id, {
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
      });

      // Sync backgroundVideos with timeline if this is video track
      if (track.type === 'video') {
        // Use setTimeout to ensure the item is added first
        setTimeout(() => syncBackgroundVideosFromTimeline(), 0);
      }

      console.log(`Added ${asset.name} to ${track.name} at frame ${dropFrame}`);
    } catch (error) {
      console.error('Drop error:', error);
    }
  }, [track, pxPerFrame, addItem, syncBackgroundVideosFromTimeline]);

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

  // Check if items changed (positions/durations)
  for (let i = 0; i < prevProps.track.items.length; i++) {
    const prev = prevProps.track.items[i];
    const next = nextProps.track.items[i];
    if (
      prev.id !== next.id ||
      prev.startFrame !== next.startFrame ||
      prev.durationInFrames !== next.durationInFrames
    ) {
      return false;
    }
  }

  return true;
});
