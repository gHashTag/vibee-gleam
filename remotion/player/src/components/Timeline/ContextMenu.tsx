import { useEffect, useRef, memo } from 'react';
import { Copy, Trash2, ClipboardPaste, Layers, Palette } from 'lucide-react';
import { useAtomValue, useSetAtom } from 'jotai';
import {
  copyItemsAtom,
  pasteItemsAtom,
  deleteItemsAtom,
  duplicateItemsAtom,
  updateItemAtom,
  clipboardAtom,
  selectedItemIdsAtom,
  getItemByIdAtom,
  projectAtom,
} from '@/atoms';
import type { ColorTag } from '@/store/types';
import './ContextMenu.css';

const COLOR_OPTIONS: { value: ColorTag; color: string; label: string }[] = [
  { value: 'none', color: 'transparent', label: 'Без цвета' },
  { value: 'red', color: '#ef4444', label: 'Красный' },
  { value: 'orange', color: '#f97316', label: 'Оранжевый' },
  { value: 'yellow', color: '#eab308', label: 'Жёлтый' },
  { value: 'green', color: '#22c55e', label: 'Зелёный' },
  { value: 'blue', color: '#3b82f6', label: 'Синий' },
  { value: 'purple', color: '#a855f7', label: 'Фиолетовый' },
  { value: 'pink', color: '#ec4899', label: 'Розовый' },
];

interface ContextMenuProps {
  x: number;
  y: number;
  itemId: string;
  onClose: () => void;
}

export const ContextMenu = memo(function ContextMenu({ x, y, itemId, onClose }: ContextMenuProps) {
  const menuRef = useRef<HTMLDivElement>(null);

  const copyItems = useSetAtom(copyItemsAtom);
  const pasteItems = useSetAtom(pasteItemsAtom);
  const deleteItems = useSetAtom(deleteItemsAtom);
  const duplicateItems = useSetAtom(duplicateItemsAtom);
  const updateItem = useSetAtom(updateItemAtom);
  const clipboard = useAtomValue(clipboardAtom);
  const selectedItemIds = useAtomValue(selectedItemIdsAtom);
  const getItemById = useAtomValue(getItemByIdAtom);
  const project = useAtomValue(projectAtom);

  // Get actual items to operate on (selected or clicked item)
  const targetIds = selectedItemIds.includes(itemId) ? selectedItemIds : [itemId];

  useEffect(() => {
    const handleClickOutside = (e: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(e.target as Node)) {
        onClose();
      }
    };

    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === 'Escape') {
        onClose();
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [onClose]);

  // Adjust position if menu would go off screen
  useEffect(() => {
    if (menuRef.current) {
      const rect = menuRef.current.getBoundingClientRect();
      const viewportWidth = window.innerWidth;
      const viewportHeight = window.innerHeight;

      if (rect.right > viewportWidth) {
        menuRef.current.style.left = `${x - rect.width}px`;
      }
      if (rect.bottom > viewportHeight) {
        menuRef.current.style.top = `${y - rect.height}px`;
      }
    }
  }, [x, y]);

  const handleCopy = () => {
    const items = targetIds.map(id => getItemById(id)).filter((i): i is import('@/store/types').TrackItem => !!i);
    copyItems(items);
    onClose();
  };

  const handlePaste = () => {
    pasteItems();
    onClose();
  };

  const handleDuplicate = () => {
    duplicateItems({ itemIds: targetIds, fps: project.fps });
    onClose();
  };

  const handleDelete = () => {
    deleteItems(targetIds);
    onClose();
  };

  const handleColorTag = (color: ColorTag) => {
    targetIds.forEach((id) => {
      updateItem({ itemId: id, updates: { colorTag: color } });
    });
    onClose();
  };

  const itemCount = targetIds.length;

  return (
    <div
      ref={menuRef}
      className="context-menu"
      style={{ left: x, top: y }}
    >
      <button className="context-menu-item" onClick={handleCopy}>
        <Copy size={14} />
        <span>Копировать</span>
        <span className="shortcut">⌘C</span>
      </button>

      <button
        className="context-menu-item"
        onClick={handlePaste}
        disabled={clipboard.length === 0}
      >
        <ClipboardPaste size={14} />
        <span>Вставить</span>
        <span className="shortcut">⌘V</span>
      </button>

      <button className="context-menu-item" onClick={handleDuplicate}>
        <Layers size={14} />
        <span>Дублировать</span>
        <span className="shortcut">⌘D</span>
      </button>

      <div className="context-menu-divider" />

      {/* Color Tags */}
      <div className="context-menu-item color-picker-row">
        <Palette size={14} />
        <span>Цвет</span>
        <div className="color-swatches">
          {COLOR_OPTIONS.map((opt) => (
            <button
              key={opt.value}
              className={`color-swatch ${opt.value === 'none' ? 'none' : ''}`}
              style={{ backgroundColor: opt.color }}
              onClick={() => handleColorTag(opt.value)}
              title={opt.label}
            />
          ))}
        </div>
      </div>

      <div className="context-menu-divider" />

      <button className="context-menu-item danger" onClick={handleDelete}>
        <Trash2 size={14} />
        <span>Удалить{itemCount > 1 ? ` (${itemCount})` : ''}</span>
        <span className="shortcut">⌫</span>
      </button>
    </div>
  );
});
