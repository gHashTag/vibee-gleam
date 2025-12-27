import { useState, useEffect } from 'react';
import { useAtomValue, useSetAtom } from 'jotai';
import { Grid, Users } from 'lucide-react';
import {
  viewedProfileAtom,
  userAtom,
  followersAtom,
  followersLoadingAtom,
  followingAtom,
  followingLoadingAtom,
  loadFollowersAtom,
  loadFollowingAtom,
} from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import { UserCard } from './UserCard';
import { ProfileTemplatesGrid } from './ProfileTemplatesGrid';

type TabId = 'templates' | 'followers' | 'following';

export function ProfileTabs() {
  const { t } = useLanguage();
  const [activeTab, setActiveTab] = useState<TabId>('templates');

  const profile = useAtomValue(viewedProfileAtom);
  const currentUser = useAtomValue(userAtom);

  const followers = useAtomValue(followersAtom);
  const followersLoading = useAtomValue(followersLoadingAtom);
  const loadFollowers = useSetAtom(loadFollowersAtom);

  const following = useAtomValue(followingAtom);
  const followingLoading = useAtomValue(followingLoadingAtom);
  const loadFollowing = useSetAtom(loadFollowingAtom);

  useEffect(() => {
    if (!profile) return;

    if (activeTab === 'followers') {
      loadFollowers(profile.username);
    } else if (activeTab === 'following') {
      loadFollowing(profile.username);
    }
  }, [activeTab, profile?.username]);

  if (!profile) return null;

  const tabs = [
    { id: 'templates' as const, icon: <Grid size={18} />, label: t('profile.templates'), count: profile.templates_count },
    { id: 'followers' as const, icon: <Users size={18} />, label: t('profile.followers'), count: profile.followers_count },
    { id: 'following' as const, icon: <Users size={18} />, label: t('profile.following'), count: profile.following_count },
  ];

  return (
    <div className="profile-tabs">
      <div className="profile-tabs__header">
        {tabs.map((tab) => (
          <button
            key={tab.id}
            className={`profile-tabs__tab ${activeTab === tab.id ? 'profile-tabs__tab--active' : ''}`}
            onClick={() => setActiveTab(tab.id)}
          >
            {tab.icon}
            <span>{tab.label}</span>
            <span className="profile-tabs__count">{tab.count}</span>
          </button>
        ))}
      </div>

      <div className="profile-tabs__content">
        {activeTab === 'templates' && (
          <ProfileTemplatesGrid username={profile.username} />
        )}

        {activeTab === 'followers' && (
          <div className="profile-tabs__users">
            {followersLoading ? (
              <div className="profile-tabs__loading">Loading...</div>
            ) : followers.length === 0 ? (
              <div className="profile-tabs__empty">
                {t('profile.no_followers')}
              </div>
            ) : (
              followers.map((user) => (
                <UserCard key={user.id} user={user} />
              ))
            )}
          </div>
        )}

        {activeTab === 'following' && (
          <div className="profile-tabs__users">
            {followingLoading ? (
              <div className="profile-tabs__loading">Loading...</div>
            ) : following.length === 0 ? (
              <div className="profile-tabs__empty">
                {t('profile.no_following')}
              </div>
            ) : (
              following.map((user) => (
                <UserCard key={user.id} user={user} />
              ))
            )}
          </div>
        )}
      </div>
    </div>
  );
}
