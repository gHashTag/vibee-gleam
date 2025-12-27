import { Link } from 'react-router-dom';
import { User } from 'lucide-react';
import { useAtomValue, useSetAtom } from 'jotai';
import { userAtom, followUserAtom, unfollowUserAtom, showLoginModalAtom } from '@/atoms';
import type { FollowUser } from '@/atoms';
import { FollowButton } from './FollowButton';

interface UserCardProps {
  user: FollowUser;
}

export function UserCard({ user }: UserCardProps) {
  const currentUser = useAtomValue(userAtom);
  const follow = useSetAtom(followUserAtom);
  const unfollow = useSetAtom(unfollowUserAtom);
  const setShowLogin = useSetAtom(showLoginModalAtom);

  const isOwnProfile = currentUser?.id !== undefined && String(currentUser.id) === user.id;

  const handleFollowClick = async () => {
    if (!currentUser) {
      setShowLogin(true);
      return;
    }

    if (user.is_following) {
      await unfollow(user.username);
    } else {
      await follow(user.username);
    }
  };

  return (
    <div className="user-card">
      <Link to={`/${user.username}`} className="user-card__link">
        <div className="user-card__avatar">
          {user.avatar_url ? (
            <img src={user.avatar_url} alt={user.display_name || user.username} />
          ) : (
            <div className="user-card__avatar-placeholder">
              <User size={24} />
            </div>
          )}
        </div>

        <div className="user-card__info">
          <span className="user-card__name">
            {user.display_name || user.username}
          </span>
          <span className="user-card__username">@{user.username}</span>
        </div>
      </Link>

      {!isOwnProfile && (
        <FollowButton
          isFollowing={user.is_following}
          onClick={handleFollowClick}
          size="small"
        />
      )}
    </div>
  );
}
