import { useEffect } from 'react';
import { useEditorStore } from '@/store/editorStore';

export function useKeyboardShortcuts() {
  const isPlaying = useEditorStore((s) => s.isPlaying);
  const play = useEditorStore((s) => s.play);
  const pause = useEditorStore((s) => s.pause);
  const deleteItems = useEditorStore((s) => s.deleteItems);
  const rippleDelete = useEditorStore((s) => s.rippleDelete);
  const duplicateItems = useEditorStore((s) => s.duplicateItems);
  const copyItems = useEditorStore((s) => s.copyItems);
  const pasteItems = useEditorStore((s) => s.pasteItems);
  const clipboard = useEditorStore((s) => s.clipboard);
  const selectedItemIds = useEditorStore((s) => s.selectedItemIds);
  const selectAll = useEditorStore((s) => s.selectAll);
  const clearSelection = useEditorStore((s) => s.clearSelection);
  const currentFrame = useEditorStore((s) => s.currentFrame);
  const setCurrentFrame = useEditorStore((s) => s.setCurrentFrame);
  const project = useEditorStore((s) => s.project);
  const undo = useEditorStore((s) => s.undo);
  const redo = useEditorStore((s) => s.redo);
  const getSelectedItems = useEditorStore((s) => s.getSelectedItems);
  const setInPoint = useEditorStore((s) => s.setInPoint);
  const setOutPoint = useEditorStore((s) => s.setOutPoint);
  const inPoint = useEditorStore((s) => s.inPoint);
  const outPoint = useEditorStore((s) => s.outPoint);
  const clearInOutPoints = useEditorStore((s) => s.clearInOutPoints);
  const splitItem = useEditorStore((s) => s.splitItem);
  const tracks = useEditorStore((s) => s.tracks);
  const setTimelineZoom = useEditorStore((s) => s.setTimelineZoom);
  const addMarker = useEditorStore((s) => s.addMarker);
  const goToNextMarker = useEditorStore((s) => s.goToNextMarker);
  const goToPrevMarker = useEditorStore((s) => s.goToPrevMarker);

  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      // Ignore if focus is on input/textarea
      const target = e.target as HTMLElement;
      if (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA') {
        return;
      }

      // Ctrl/Cmd + Z - Undo
      if ((e.ctrlKey || e.metaKey) && e.code === 'KeyZ' && !e.shiftKey) {
        e.preventDefault();
        undo();
        return;
      }

      // Ctrl/Cmd + Shift + Z or Ctrl/Cmd + Y - Redo
      if ((e.ctrlKey || e.metaKey) && ((e.code === 'KeyZ' && e.shiftKey) || e.code === 'KeyY')) {
        e.preventDefault();
        redo();
        return;
      }

      // Space - Play/Pause
      if (e.code === 'Space') {
        e.preventDefault();
        if (isPlaying) {
          pause();
        } else {
          play();
        }
        return;
      }

      // Delete/Backspace - Delete selected items
      // Shift+Delete/Backspace - Ripple delete (close gaps)
      if (e.code === 'Delete' || e.code === 'Backspace') {
        if (selectedItemIds.length > 0) {
          e.preventDefault();
          if (e.shiftKey) {
            rippleDelete(selectedItemIds);
          } else {
            deleteItems(selectedItemIds);
          }
        }
        return;
      }

      // Ctrl/Cmd + A - Select all
      if ((e.ctrlKey || e.metaKey) && e.code === 'KeyA') {
        e.preventDefault();
        selectAll();
        return;
      }

      // Ctrl/Cmd + D - Duplicate selected items
      if ((e.ctrlKey || e.metaKey) && e.code === 'KeyD') {
        e.preventDefault();
        if (selectedItemIds.length > 0) {
          duplicateItems(selectedItemIds);
        }
        return;
      }

      // Ctrl/Cmd + C - Copy selected items
      if ((e.ctrlKey || e.metaKey) && e.code === 'KeyC') {
        e.preventDefault();
        if (selectedItemIds.length > 0) {
          copyItems(selectedItemIds);
        }
        return;
      }

      // Ctrl/Cmd + V - Paste items
      if ((e.ctrlKey || e.metaKey) && e.code === 'KeyV') {
        e.preventDefault();
        if (clipboard.length > 0) {
          pasteItems();
        }
        return;
      }

      // Escape - Clear selection
      if (e.code === 'Escape') {
        clearSelection();
        return;
      }

      // Arrow Left - Previous frame
      if (e.code === 'ArrowLeft') {
        e.preventDefault();
        const step = e.shiftKey ? 10 : 1;
        setCurrentFrame(Math.max(0, currentFrame - step));
        return;
      }

      // Arrow Right - Next frame
      if (e.code === 'ArrowRight') {
        e.preventDefault();
        const step = e.shiftKey ? 10 : 1;
        setCurrentFrame(Math.min(project.durationInFrames - 1, currentFrame + step));
        return;
      }

      // Home - Go to start
      if (e.code === 'Home') {
        e.preventDefault();
        setCurrentFrame(0);
        return;
      }

      // End - Go to end
      if (e.code === 'End') {
        e.preventDefault();
        setCurrentFrame(project.durationInFrames - 1);
        return;
      }

      // J-K-L Scrubbing (Pro video editor shortcuts)
      // J - Rewind 1 second
      if (e.code === 'KeyJ') {
        e.preventDefault();
        pause();
        setCurrentFrame(Math.max(0, currentFrame - project.fps));
        return;
      }

      // K - Pause
      if (e.code === 'KeyK') {
        e.preventDefault();
        pause();
        return;
      }

      // L - Forward 1 second
      if (e.code === 'KeyL') {
        e.preventDefault();
        pause();
        setCurrentFrame(Math.min(project.durationInFrames - 1, currentFrame + project.fps));
        return;
      }

      // I - Set In point (mark current position)
      if (e.code === 'KeyI') {
        e.preventDefault();
        // Toggle in point at current frame
        if (inPoint === currentFrame) {
          setInPoint(null);
        } else {
          setInPoint(currentFrame);
        }
        return;
      }

      // O - Set Out point
      if (e.code === 'KeyO') {
        e.preventDefault();
        // Toggle out point at current frame
        if (outPoint === currentFrame) {
          setOutPoint(null);
        } else {
          setOutPoint(currentFrame);
        }
        return;
      }

      // Alt + X - Clear In/Out points
      if (e.altKey && e.code === 'KeyX') {
        e.preventDefault();
        clearInOutPoints();
        return;
      }

      // [ - Jump to start of selection
      if (e.code === 'BracketLeft') {
        e.preventDefault();
        const items = getSelectedItems();
        if (items.length > 0) {
          const minStart = Math.min(...items.map((i) => i.startFrame));
          setCurrentFrame(minStart);
        }
        return;
      }

      // ] - Jump to end of selection
      if (e.code === 'BracketRight') {
        e.preventDefault();
        const items = getSelectedItems();
        if (items.length > 0) {
          const maxEnd = Math.max(...items.map((i) => i.startFrame + i.durationInFrames));
          setCurrentFrame(Math.min(maxEnd, project.durationInFrames - 1));
        }
        return;
      }

      // S - Split items at playhead
      if (e.code === 'KeyS' && !e.ctrlKey && !e.metaKey) {
        e.preventDefault();
        // Find all items under the playhead and split them
        for (const track of tracks) {
          if (track.locked) continue;
          for (const item of track.items) {
            const itemEnd = item.startFrame + item.durationInFrames;
            if (currentFrame > item.startFrame && currentFrame < itemEnd) {
              splitItem(item.id, currentFrame);
            }
          }
        }
        return;
      }

      // , (comma) - Previous frame (single step)
      if (e.code === 'Comma') {
        e.preventDefault();
        pause();
        setCurrentFrame(Math.max(0, currentFrame - 1));
        return;
      }

      // . (period) - Next frame (single step)
      if (e.code === 'Period') {
        e.preventDefault();
        pause();
        setCurrentFrame(Math.min(project.durationInFrames - 1, currentFrame + 1));
        return;
      }

      // Shift + Z - Fit timeline to view
      if (e.shiftKey && e.code === 'KeyZ' && !e.ctrlKey && !e.metaKey) {
        e.preventDefault();
        // Calculate zoom to fit entire duration in ~800px viewport
        // Base is 2px per frame, so: zoom = viewportWidth / (duration * 2)
        const viewportWidth = 800; // Approximate timeline viewport
        const optimalZoom = viewportWidth / (project.durationInFrames * 2);
        setTimelineZoom(Math.max(0.25, Math.min(optimalZoom, 5)));
        return;
      }

      // M - Add/remove marker at current frame
      if (e.code === 'KeyM' && !e.ctrlKey && !e.metaKey && !e.shiftKey && !e.altKey) {
        e.preventDefault();
        addMarker(currentFrame);
        return;
      }

      // Shift + M - Go to next marker
      if (e.shiftKey && e.code === 'KeyM' && !e.ctrlKey && !e.metaKey) {
        e.preventDefault();
        goToNextMarker();
        return;
      }

      // Alt + M - Go to previous marker
      if (e.altKey && e.code === 'KeyM' && !e.ctrlKey && !e.metaKey) {
        e.preventDefault();
        goToPrevMarker();
        return;
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [
    isPlaying,
    play,
    pause,
    deleteItems,
    rippleDelete,
    duplicateItems,
    copyItems,
    pasteItems,
    clipboard,
    selectedItemIds,
    selectAll,
    clearSelection,
    currentFrame,
    setCurrentFrame,
    project.durationInFrames,
    project.fps,
    undo,
    redo,
    getSelectedItems,
    setInPoint,
    setOutPoint,
    inPoint,
    outPoint,
    clearInOutPoints,
    splitItem,
    tracks,
    setTimelineZoom,
    addMarker,
    goToNextMarker,
    goToPrevMarker,
  ]);
}
