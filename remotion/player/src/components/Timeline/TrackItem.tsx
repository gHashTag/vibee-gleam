import { useState, useCallback, useEffect, memo } from 'react';
import { createPortal } from 'react-dom';
import { useAtomValue, useSetAtom } from 'jotai';
import { selectItemsAtom, selectRangeAtom, moveItemAtom, resizeItemAtom, getAssetByIdAtom } from '@/atoms';
import { ContextMenu } from './ContextMenu';
import type { TrackItem as TrackItemType } from '@/store/types';

interface TrackItemProps {
  item: TrackItemType;
  pxPerFrame: number;
  isSelected: boolean;
  isLocked?: boolean;
}

export const TrackItem = memo(function TrackItem({ item, pxPerFrame, isSelected, isLocked = false }: TrackItemProps) {
  const selectItems = useSetAtom(selectItemsAtom);
  const selectRange = useSetAtom(selectRangeAtom);
  const moveItem = useSetAtom(moveItemAtom);
  const resizeItem = useSetAtom(resizeItemAtom);
  const getAssetById = useAtomValue(getAssetByIdAtom);

  const [isDragging, setIsDragging] = useState(false);
  const [isResizing, setIsResizing] = useState<'left' | 'right' | null>(null);
  const [isDragOver, setIsDragOver] = useState(false);
  const [dragStartX, setDragStartX] = useState(0);
  const [initialFrame, setInitialFrame] = useState(0);
  const [initialDuration, setInitialDuration] = useState(0);
  const [contextMenu, setContextMenu] = useState<{ x: number; y: number } | null>(null);

  const asset = item.assetId ? getAssetById(item.assetId) : null;
  const label = asset?.name || item.type;

  // Get item-specific props for badges
  const playbackRate = item.type === 'video' ? (item as any).playbackRate : null;
  const volume = (item.type === 'video' || item.type === 'audio') ? (item as any).volume : null;

  const handleClick = (e: React.MouseEvent) => {
    e.stopPropagation();
    if (e.shiftKey) {
      // Shift+click: range selection
      selectRange(item.id);
    } else if (e.metaKey || e.ctrlKey) {
      // Cmd/Ctrl+click: toggle selection
      selectItems({ itemIds: [item.id], addToSelection: true });
    } else {
      // Regular click: single select
      selectItems({ itemIds: [item.id], addToSelection: false });
    }
  };

  const handleContextMenu = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();

    // Select item if not already selected
    if (!isSelected) {
      selectItems({ itemIds: [item.id], addToSelection: false });
    }

    setContextMenu({ x: e.clientX, y: e.clientY });
  };

  const closeContextMenu = useCallback(() => {
    setContextMenu(null);
  }, []);

  const handleMouseDown = (e: React.MouseEvent, type: 'drag' | 'resize-left' | 'resize-right') => {
    e.stopPropagation();
    e.preventDefault();

    // Prevent drag/resize if track is locked
    if (isLocked) return;

    setDragStartX(e.clientX);
    setInitialFrame(item.startFrame);
    setInitialDuration(item.durationInFrames);

    if (type === 'drag') {
      setIsDragging(true);
    } else {
      setIsResizing(type === 'resize-left' ? 'left' : 'right');
    }

    // Select item if not already selected (for drag operations)
    if (!isSelected) {
      selectItems({ itemIds: [item.id], addToSelection: false });
    }
  };

  const handleMouseMove = useCallback(
    (e: MouseEvent) => {
      const deltaX = e.clientX - dragStartX;
      const deltaFrames = Math.round(deltaX / pxPerFrame);

      if (isDragging) {
        const newStart = Math.max(0, initialFrame + deltaFrames);
        moveItem({ itemId: item.id, newStartFrame: newStart, snapSettings: { enabled: false, interval: 15 } });
      } else if (isResizing === 'left') {
        const newStart = Math.max(0, initialFrame + deltaFrames);
        const newDuration = initialDuration - deltaFrames;
        if (newDuration > 0) {
          moveItem({ itemId: item.id, newStartFrame: newStart, snapSettings: { enabled: false, interval: 15 } });
          resizeItem({ itemId: item.id, newDuration });
        }
      } else if (isResizing === 'right') {
        const newDuration = Math.max(1, initialDuration + deltaFrames);
        resizeItem({ itemId: item.id, newDuration });
      }
    },
    [isDragging, isResizing, dragStartX, initialFrame, initialDuration, pxPerFrame, item.id, moveItem, resizeItem]
  );

  const handleMouseUp = useCallback(() => {
    setIsDragging(false);
    setIsResizing(null);
  }, []);

  // Add/remove global event listeners for drag/resize
  useEffect(() => {
    if (isDragging || isResizing) {
      window.addEventListener('mousemove', handleMouseMove);
      window.addEventListener('mouseup', handleMouseUp);
      return () => {
        window.removeEventListener('mousemove', handleMouseMove);
        window.removeEventListener('mouseup', handleMouseUp);
      };
    }
  }, [isDragging, isResizing, handleMouseMove, handleMouseUp]);

  const colorTag = item.colorTag || 'none';

  // Drag over handlers for visual feedback when dropping to replace
  // DO NOT use stopPropagation - let parent Track receive all drag events
  const handleDragOver = useCallback((e: React.DragEvent) => {
    e.preventDefault();
    setIsDragOver(true);
  }, []);

  const handleDragLeave = useCallback((e: React.DragEvent) => {
    setIsDragOver(false);
  }, []);

  const handleDrop = useCallback((e: React.DragEvent) => {
    // DO NOT call stopPropagation - let parent Track handle the drop
    // Parent Track.handleDrop detects if drop is on existing item and replaces
    setIsDragOver(false);
  }, []);

  return (
    <div
      className={`track-item ${isSelected ? 'selected' : ''} ${isLocked ? 'locked' : ''} ${isDragOver ? 'drag-replace' : ''}`}
      data-type={item.type}
      data-color={colorTag}
      style={{
        left: item.startFrame * pxPerFrame,
        width: item.durationInFrames * pxPerFrame,
      }}
      onClick={handleClick}
      onMouseDown={(e) => handleMouseDown(e, 'drag')}
      onContextMenu={handleContextMenu}
      onDragOver={handleDragOver}
      onDragLeave={handleDragLeave}
      onDrop={handleDrop}
    >
      {/* Color tag stripe */}
      {colorTag !== 'none' && <div className="track-item-color-tag" />}

      {/* Left resize handle */}
      <div
        className="track-item-handle left"
        onMouseDown={(e) => handleMouseDown(e, 'resize-left')}
      />

      {/* Content */}
      <div className="track-item-content">{label}</div>

      {/* Badges */}
      <div className="track-item-badges">
        {playbackRate !== null && playbackRate !== 1 && (
          <span className={`track-badge speed ${playbackRate > 1 ? 'fast' : 'slow'}`}>
            {playbackRate}x
          </span>
        )}
        {volume !== null && volume !== 1 && (
          <span className={`track-badge volume ${volume === 0 ? 'muted' : ''}`}>
            {volume === 0 ? '🔇' : `${Math.round(volume * 100)}%`}
          </span>
        )}
      </div>

      {/* Right resize handle */}
      <div
        className="track-item-handle right"
        onMouseDown={(e) => handleMouseDown(e, 'resize-right')}
      />

      {/* Context Menu */}
      {contextMenu && createPortal(
        <ContextMenu
          x={contextMenu.x}
          y={contextMenu.y}
          itemId={item.id}
          onClose={closeContextMenu}
        />,
        document.body
      )}
    </div>
  );
}, (prevProps, nextProps) => {
  // Custom comparison for optimal re-renders
  return (
    prevProps.isSelected === nextProps.isSelected &&
    prevProps.isLocked === nextProps.isLocked &&
    prevProps.pxPerFrame === nextProps.pxPerFrame &&
    prevProps.item.id === nextProps.item.id &&
    prevProps.item.startFrame === nextProps.item.startFrame &&
    prevProps.item.durationInFrames === nextProps.item.durationInFrames &&
    prevProps.item.assetId === nextProps.item.assetId &&
    prevProps.item.colorTag === nextProps.item.colorTag
  );
});
