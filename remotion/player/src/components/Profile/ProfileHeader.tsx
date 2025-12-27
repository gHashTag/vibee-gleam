import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { User, Users, Video, Eye, Heart, Settings } from 'lucide-react';
import {
  viewedProfileAtom,
  userAtom,
  followUserAtom,
  unfollowUserAtom,
  showLoginModalAtom,
} from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import { FollowButton } from './FollowButton';
import { SocialLinks } from './SocialLinks';

interface ProfileHeaderProps {
  onEditClick?: () => void;
}

export function ProfileHeader({ onEditClick }: ProfileHeaderProps) {
  const { t } = useLanguage();
  const profile = useAtomValue(viewedProfileAtom);
  const user = useAtomValue(userAtom);
  const follow = useSetAtom(followUserAtom);
  const unfollow = useSetAtom(unfollowUserAtom);
  const [, setShowLogin] = useAtom(showLoginModalAtom);

  if (!profile) return null;

  const handleFollowClick = async () => {
    if (!user) {
      setShowLogin(true);
      return;
    }

    if (profile.is_following) {
      await unfollow(profile.username);
    } else {
      await follow(profile.username);
    }
  };

  const formatNumber = (num: number): string => {
    if (num >= 1000000) return `${(num / 1000000).toFixed(1)}M`;
    if (num >= 1000) return `${(num / 1000).toFixed(1)}K`;
    return num.toString();
  };

  return (
    <div className="profile-header">
      <div className="profile-header__top">
        <div className="profile-header__avatar">
          {profile.avatar_url ? (
            <img src={profile.avatar_url} alt={profile.display_name || profile.username} />
          ) : (
            <div className="profile-header__avatar-placeholder">
              <User size={48} />
            </div>
          )}
        </div>

        <div className="profile-header__info">
          <h1 className="profile-header__name">
            {profile.display_name || profile.username}
          </h1>
          <p className="profile-header__username">@{profile.username}</p>

          {profile.bio && (
            <p className="profile-header__bio">{profile.bio}</p>
          )}

          <SocialLinks links={profile.social_links} />
        </div>

        <div className="profile-header__actions">
          {profile.is_own_profile ? (
            <button className="profile-header__edit-btn" onClick={onEditClick}>
              <Settings size={18} />
              <span>{t('profile.edit')}</span>
            </button>
          ) : (
            <FollowButton
              isFollowing={profile.is_following}
              onClick={handleFollowClick}
            />
          )}
        </div>
      </div>

      <div className="profile-header__stats">
        <div className="profile-header__stat">
          <Users size={18} />
          <span className="profile-header__stat-value">
            {formatNumber(profile.followers_count)}
          </span>
          <span className="profile-header__stat-label">{t('profile.followers')}</span>
        </div>

        <div className="profile-header__stat">
          <Users size={18} />
          <span className="profile-header__stat-value">
            {formatNumber(profile.following_count)}
          </span>
          <span className="profile-header__stat-label">{t('profile.following')}</span>
        </div>

        <div className="profile-header__stat">
          <Video size={18} />
          <span className="profile-header__stat-value">
            {formatNumber(profile.templates_count)}
          </span>
          <span className="profile-header__stat-label">{t('profile.videos')}</span>
        </div>

        <div className="profile-header__stat">
          <Eye size={18} />
          <span className="profile-header__stat-value">
            {formatNumber(profile.total_views)}
          </span>
          <span className="profile-header__stat-label">{t('profile.views')}</span>
        </div>

        <div className="profile-header__stat">
          <Heart size={18} />
          <span className="profile-header__stat-value">
            {formatNumber(profile.total_likes)}
          </span>
          <span className="profile-header__stat-label">{t('profile.likes')}</span>
        </div>
      </div>
    </div>
  );
}
