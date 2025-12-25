import { useAtomValue, useSetAtom } from 'jotai';
import { templatesAtom, selectedTemplateIdAtom, selectTemplateAtom } from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import { LayoutTemplate, Check } from 'lucide-react';
import './TemplatesPanel.css';

export function TemplatesPanel() {
  const { t } = useLanguage();
  const templates = useAtomValue(templatesAtom);
  const selectedTemplateId = useAtomValue(selectedTemplateIdAtom);
  const selectTemplate = useSetAtom(selectTemplateAtom);

  return (
    <div className="templates-panel">
      <div className="panel-header">
        <LayoutTemplate size={14} />
        <span>{t('templates.title')}</span>
      </div>

      <div className="template-list">
        {templates.map((template) => {
          const isSelected = template.id === selectedTemplateId;

          return (
            <div
              key={template.id}
              className={`template-item ${isSelected ? 'selected' : ''}`}
              onClick={() => selectTemplate(template.id)}
              title={template.description}
            >
              <div className="template-icon">
                <LayoutTemplate size={16} />
              </div>
              <span className="template-name">{template.name}</span>
              {isSelected && (
                <div className="template-check">
                  <Check size={14} />
                </div>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
}
