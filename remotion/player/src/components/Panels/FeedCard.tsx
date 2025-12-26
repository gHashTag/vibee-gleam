import { useCallback, useState } from 'react';
import { useSetAtom } from 'jotai';
import { likeTemplateAtom, useTemplateAtom, type FeedTemplate } from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import { Heart, Eye, Users, Play, Loader2 } from 'lucide-react';
import './FeedPanel.css';

interface FeedCardProps {
  template: FeedTemplate;
}

export function FeedCard({ template }: FeedCardProps) {
  const { t } = useLanguage();
  const likeTemplate = useSetAtom(likeTemplateAtom);
  const useTemplate = useSetAtom(useTemplateAtom);
  const [isUsing, setIsUsing] = useState(false);
  const [isHovering, setIsHovering] = useState(false);

  const handleLike = useCallback((e: React.MouseEvent) => {
    e.stopPropagation();
    likeTemplate(template.id);
  }, [likeTemplate, template.id]);

  const handleUse = useCallback(async () => {
    setIsUsing(true);
    try {
      await useTemplate(template.id);
    } finally {
      setIsUsing(false);
    }
  }, [useTemplate, template.id]);

  const formatCount = (count: number): string => {
    if (count >= 1000000) return `${(count / 1000000).toFixed(1)}M`;
    if (count >= 1000) return `${(count / 1000).toFixed(1)}K`;
    return count.toString();
  };

  const formatDate = (dateStr: string): string => {
    const date = new Date(dateStr);
    const now = new Date();
    const diff = now.getTime() - date.getTime();
    const days = Math.floor(diff / (1000 * 60 * 60 * 24));

    if (days === 0) return t('feed.today');
    if (days === 1) return t('feed.yesterday');
    if (days < 7) return `${days}d`;
    if (days < 30) return `${Math.floor(days / 7)}w`;
    return `${Math.floor(days / 30)}mo`;
  };

  return (
    <div
      className="feed-card"
      onMouseEnter={() => setIsHovering(true)}
      onMouseLeave={() => setIsHovering(false)}
    >
      <div className="feed-card-thumbnail">
        {template.thumbnailUrl ? (
          <img src={template.thumbnailUrl} alt={template.name} />
        ) : (
          <div className="feed-card-placeholder">
            <Play size={24} />
          </div>
        )}

        {isHovering && template.videoUrl && (
          <video
            src={template.videoUrl}
            autoPlay
            muted
            loop
            playsInline
            className="feed-card-preview"
          />
        )}

        {template.isFeatured && (
          <div className="feed-card-featured">Featured</div>
        )}
      </div>

      <div className="feed-card-info">
        <div className="feed-card-header">
          {template.creatorAvatar && (
            <img
              src={template.creatorAvatar}
              alt={template.creatorName}
              className="feed-card-avatar"
            />
          )}
          <div className="feed-card-meta">
            <span className="feed-card-name">{template.name}</span>
            <span className="feed-card-creator">
              {template.creatorName} · {formatDate(template.createdAt)}
            </span>
          </div>
        </div>

        {template.description && (
          <p className="feed-card-description">{template.description}</p>
        )}

        <div className="feed-card-stats">
          <button
            className={`stat-btn like-btn ${template.isLiked ? 'liked' : ''}`}
            onClick={handleLike}
            title={t('feed.like')}
          >
            <Heart size={14} fill={template.isLiked ? 'currentColor' : 'none'} />
            <span>{formatCount(template.likesCount)}</span>
          </button>
          <div className="stat-item">
            <Eye size={14} />
            <span>{formatCount(template.viewsCount)}</span>
          </div>
          <div className="stat-item">
            <Users size={14} />
            <span>{formatCount(template.usesCount)}</span>
          </div>
        </div>

        <button
          className="feed-card-use"
          onClick={handleUse}
          disabled={isUsing}
        >
          {isUsing ? (
            <>
              <Loader2 size={14} className="spinning" />
              {t('feed.using')}
            </>
          ) : (
            t('feed.useTemplate')
          )}
        </button>
      </div>
    </div>
  );
}
