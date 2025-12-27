import { useAtomValue, useSetAtom } from 'jotai';
import { templatesAtom, selectedTemplateIdAtom, selectTemplateAtom, removeTemplateAtom } from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import { LayoutTemplate, Check, Trash2 } from 'lucide-react';
import './TemplatesPanel.css';

export function TemplatesPanel() {
  const { t } = useLanguage();
  const templates = useAtomValue(templatesAtom);
  const selectedTemplateId = useAtomValue(selectedTemplateIdAtom);
  const selectTemplate = useSetAtom(selectTemplateAtom);
  const removeTemplate = useSetAtom(removeTemplateAtom);

  const handleDelete = (e: React.MouseEvent, templateId: string) => {
    e.stopPropagation();
    if (confirm(t('templates.confirmDelete'))) {
      removeTemplate(templateId);
    }
  };

  return (
    <div className="templates-panel">
      {/* Grid of template cards */}
      <div className="template-grid">
        {templates.map((template) => {
          const isSelected = template.id === selectedTemplateId;

          return (
            <div
              key={template.id}
              className={`template-card ${isSelected ? 'selected' : ''}`}
              onClick={() => selectTemplate(template.id)}
              title={template.description}
            >
              <div className="template-card-thumbnail">
                {template.thumbnail ? (
                  <img src={template.thumbnail} alt={template.name} />
                ) : (
                  <LayoutTemplate size={24} />
                )}
                {isSelected && (
                  <div className="template-card-check">
                    <Check size={14} />
                  </div>
                )}
                {/* Overlay with name */}
                <div className="template-card-overlay">
                  <span className="template-card-name">{template.name}</span>
                </div>
              </div>
              {template.isUserCreated && (
                <button
                  className="template-card-delete"
                  onClick={(e) => handleDelete(e, template.id)}
                  title={t('templates.delete')}
                >
                  <Trash2 size={10} />
                </button>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
}
