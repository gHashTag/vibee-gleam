import { useCallback, useState } from 'react';
import { Link } from 'react-router-dom';
import { useSetAtom } from 'jotai';
import { likeTemplateAtom, useTemplateAtom, type FeedTemplate } from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import { LikeAnimation } from '@/components/LikeAnimation';
import { Heart, Eye, Users, Play, Loader2, Sparkles } from 'lucide-react';
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

  // Double-tap like handler
  const handleDoubleTapLike = useCallback(() => {
    if (!template.isLiked) {
      likeTemplate(template.id);
    }
  }, [likeTemplate, template.id, template.isLiked]);

  const handleUse = useCallback(async (e: React.MouseEvent) => {
    e.stopPropagation();
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
      {/* Inner wrapper for max-width centering */}
      <div className="feed-card-inner">
        <LikeAnimation onDoubleTap={handleDoubleTapLike}>
          <div className="feed-card-thumbnail">
            {template.thumbnailUrl ? (
              <img src={template.thumbnailUrl} alt={template.name} />
            ) : template.videoUrl ? (
              <video
                src={template.videoUrl}
                muted
                playsInline
                className="feed-card-thumbnail-video"
              />
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
        </LikeAnimation>

        {/* TikTok-style right action bar */}
        <div className="feed-card-actions">
          <button
            className={`action-btn like-btn ${template.isLiked ? 'liked' : ''}`}
            onClick={handleLike}
            title={t('feed.like')}
          >
            <Heart size={32} fill={template.isLiked ? 'currentColor' : 'none'} />
            <span>{formatCount(template.likesCount)}</span>
          </button>

          <div className="action-btn views-btn">
            <Eye size={32} />
            <span>{formatCount(template.viewsCount)}</span>
          </div>

          <div className="action-btn uses-btn">
            <Users size={32} />
            <span>{formatCount(template.usesCount)}</span>
          </div>

          <button
            className="action-btn remix-btn"
            onClick={handleUse}
            disabled={isUsing}
            title="Remix"
          >
            {isUsing ? (
              <Loader2 size={32} className="spinning" />
            ) : (
              <Sparkles size={32} />
            )}
            <span>Remix</span>
          </button>
        </div>

        {/* Info overlay at bottom */}
        <div className="feed-card-info">
          <div className="feed-card-header">
            {template.creatorUsername ? (
              <Link
                to={`/${template.creatorUsername}`}
                className="feed-card-creator-link"
                onClick={(e) => e.stopPropagation()}
              >
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
                    @{template.creatorUsername} · {formatDate(template.createdAt)}
                  </span>
                </div>
              </Link>
            ) : (
              <>
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
              </>
            )}
          </div>

          {template.description && (
            <p className="feed-card-description">{template.description}</p>
          )}
        </div>
      </div>
    </div>
  );
}
