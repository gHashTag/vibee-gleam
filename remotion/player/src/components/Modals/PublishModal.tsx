// PublishModal - Share template to community feed
import { useState, useCallback } from 'react';
import { useNavigate } from 'react-router-dom';
import { useSetAtom, useAtomValue } from 'jotai';
import { publishToFeedAtom, currentRemixSourceAtom } from '@/atoms';
import { templatesAtom, selectedTemplateIdAtom } from '@/atoms/templates';
import { useLanguage } from '@/hooks/useLanguage';
import { X, Globe, Loader2, Sparkles, Check } from 'lucide-react';
import './PublishModal.css';

interface PublishModalProps {
  isOpen: boolean;
  onClose: () => void;
  videoUrl?: string;
  thumbnailUrl?: string;
}

export function PublishModal({ isOpen, onClose, videoUrl, thumbnailUrl }: PublishModalProps) {
  const { t } = useLanguage();
  const navigate = useNavigate();
  const publishToFeed = useSetAtom(publishToFeedAtom);
  const templates = useAtomValue(templatesAtom);
  const selectedTemplateId = useAtomValue(selectedTemplateIdAtom);
  const remixSource = useAtomValue(currentRemixSourceAtom);

  const [name, setName] = useState('');
  const [description, setDescription] = useState('');
  const [isPublishing, setIsPublishing] = useState(false);
  const [isPublished, setIsPublished] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Get current template name as default
  const currentTemplate = templates.find(t => t.id === selectedTemplateId);

  const handlePublish = useCallback(async () => {
    if (!videoUrl) {
      setError(t('publish.noVideo'));
      return;
    }

    if (!name.trim()) {
      setError(t('publish.nameRequired'));
      return;
    }

    setIsPublishing(true);
    setError(null);

    try {
      await publishToFeed({
        name: name.trim(),
        description: description.trim() || undefined,
        videoUrl,
        thumbnailUrl,
      });

      setIsPublished(true);

      // Auto-switch to feed after 1.5s
      setTimeout(() => {
        navigate('/feed');
        onClose();
      }, 1500);
    } catch (err) {
      setError(err instanceof Error ? err.message : t('publish.failed'));
    } finally {
      setIsPublishing(false);
    }
  }, [name, description, videoUrl, thumbnailUrl, publishToFeed, navigate, onClose, t]);

  if (!isOpen) return null;

  return (
    <div className="publish-modal-overlay" onClick={onClose}>
      <div className="publish-modal" onClick={e => e.stopPropagation()}>
        <button className="publish-modal-close" onClick={onClose}>
          <X size={20} />
        </button>

        {isPublished ? (
          <div className="publish-success">
            <div className="publish-success-icon">
              <Check size={48} />
            </div>
            <h2>{t('publish.success')}</h2>
            <p>{t('publish.successDesc')}</p>
          </div>
        ) : (
          <>
            <div className="publish-modal-header">
              <Globe size={24} />
              <h2>{t('publish.title')}</h2>
            </div>

            <p className="publish-modal-subtitle">{t('publish.subtitle')}</p>

            {remixSource && (
              <div className="publish-remix-badge">
                <Sparkles size={14} />
                <span>{t('publish.remixOf')} {remixSource.creatorName}</span>
              </div>
            )}

            <div className="publish-preview">
              {thumbnailUrl ? (
                <img src={thumbnailUrl} alt="Preview" />
              ) : videoUrl ? (
                <video src={videoUrl} muted loop autoPlay playsInline />
              ) : (
                <div className="publish-preview-placeholder">
                  <Globe size={32} />
                </div>
              )}
            </div>

            <div className="publish-form">
              <div className="publish-field">
                <label>{t('publish.name')}</label>
                <input
                  type="text"
                  value={name}
                  onChange={e => setName(e.target.value)}
                  placeholder={currentTemplate?.name || t('publish.namePlaceholder')}
                  maxLength={100}
                  autoFocus
                />
              </div>

              <div className="publish-field">
                <label>{t('publish.description')}</label>
                <textarea
                  value={description}
                  onChange={e => setDescription(e.target.value)}
                  placeholder={t('publish.descPlaceholder')}
                  maxLength={500}
                  rows={3}
                />
              </div>
            </div>

            {error && (
              <div className="publish-error">{error}</div>
            )}

            <div className="publish-actions">
              <button className="publish-cancel" onClick={onClose}>
                {t('dialog.cancel')}
              </button>
              <button
                className="publish-submit"
                onClick={handlePublish}
                disabled={isPublishing || !name.trim()}
              >
                {isPublishing ? (
                  <>
                    <Loader2 size={16} className="spinning" />
                    {t('publish.publishing')}
                  </>
                ) : (
                  <>
                    <Globe size={16} />
                    {t('publish.share')}
                  </>
                )}
              </button>
            </div>
          </>
        )}
      </div>
    </div>
  );
}
