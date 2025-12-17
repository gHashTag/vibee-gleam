import { memo } from 'react';
import { X } from 'lucide-react';
import './ShortcutsModal.css';

interface ShortcutsModalProps {
  isOpen: boolean;
  onClose: () => void;
}

const SHORTCUTS = [
  {
    category: 'Воспроизведение',
    shortcuts: [
      { keys: ['Space'], description: 'Плей/Пауза' },
      { keys: ['J'], description: 'Назад на 1 секунду' },
      { keys: ['K'], description: 'Пауза' },
      { keys: ['L'], description: 'Вперёд на 1 секунду' },
      { keys: [','], description: 'Предыдущий кадр' },
      { keys: ['.'], description: 'Следующий кадр' },
      { keys: ['←'], description: 'Назад на 1 кадр' },
      { keys: ['→'], description: 'Вперёд на 1 кадр' },
      { keys: ['Shift', '←'], description: 'Назад на 10 кадров' },
      { keys: ['Shift', '→'], description: 'Вперёд на 10 кадров' },
      { keys: ['Home'], description: 'Перейти в начало' },
      { keys: ['End'], description: 'Перейти в конец' },
    ],
  },
  {
    category: 'Редактирование',
    shortcuts: [
      { keys: ['⌘', 'Z'], description: 'Отменить' },
      { keys: ['⌘', 'Shift', 'Z'], description: 'Повторить' },
      { keys: ['⌘', 'C'], description: 'Копировать' },
      { keys: ['⌘', 'V'], description: 'Вставить' },
      { keys: ['⌘', 'D'], description: 'Дублировать' },
      { keys: ['Delete'], description: 'Удалить' },
      { keys: ['Shift', 'Delete'], description: 'Удалить с закрытием gap' },
      { keys: ['S'], description: 'Разрезать на playhead' },
    ],
  },
  {
    category: 'Выделение',
    shortcuts: [
      { keys: ['⌘', 'A'], description: 'Выделить всё' },
      { keys: ['Escape'], description: 'Снять выделение' },
      { keys: ['⌘', 'Click'], description: 'Добавить к выделению' },
      { keys: ['Shift', 'Click'], description: 'Выделить диапазон' },
    ],
  },
  {
    category: 'Навигация',
    shortcuts: [
      { keys: ['['], description: 'К началу выделения' },
      { keys: [']'], description: 'К концу выделения' },
      { keys: ['I'], description: 'Установить In point' },
      { keys: ['O'], description: 'Установить Out point' },
      { keys: ['Alt', 'X'], description: 'Сбросить In/Out points' },
      { keys: ['M'], description: 'Добавить/удалить маркер' },
      { keys: ['Shift', 'M'], description: 'К следующему маркеру' },
      { keys: ['Alt', 'M'], description: 'К предыдущему маркеру' },
    ],
  },
  {
    category: 'Вид',
    shortcuts: [
      { keys: ['Shift', 'Z'], description: 'Вместить в окно' },
      { keys: ['?'], description: 'Показать горячие клавиши' },
    ],
  },
];

export const ShortcutsModal = memo(function ShortcutsModal({ isOpen, onClose }: ShortcutsModalProps) {
  if (!isOpen) return null;

  return (
    <div className="shortcuts-modal-overlay" onClick={onClose}>
      <div className="shortcuts-modal" onClick={(e) => e.stopPropagation()}>
        <div className="shortcuts-modal-header">
          <h2>Горячие клавиши</h2>
          <button className="shortcuts-close-btn" onClick={onClose}>
            <X size={20} />
          </button>
        </div>

        <div className="shortcuts-modal-content">
          {SHORTCUTS.map((section) => (
            <div key={section.category} className="shortcuts-section">
              <h3 className="shortcuts-category">{section.category}</h3>
              <div className="shortcuts-list">
                {section.shortcuts.map((shortcut, index) => (
                  <div key={index} className="shortcut-row">
                    <div className="shortcut-keys">
                      {shortcut.keys.map((key, keyIndex) => (
                        <span key={keyIndex}>
                          <kbd className="shortcut-key">{key}</kbd>
                          {keyIndex < shortcut.keys.length - 1 && (
                            <span className="shortcut-plus">+</span>
                          )}
                        </span>
                      ))}
                    </div>
                    <span className="shortcut-description">{shortcut.description}</span>
                  </div>
                ))}
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
});
