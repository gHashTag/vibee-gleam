import { useEffect, useState } from 'react';
import { useParams, useNavigate, Link } from 'react-router-dom';
import { useAtomValue, useSetAtom } from 'jotai';
import { ArrowLeft, UserX } from 'lucide-react';
import {
  viewedProfileAtom,
  profileLoadingAtom,
  profileErrorAtom,
  loadProfileAtom,
} from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import { ProfileHeader, ProfileTabs, ProfileEdit } from '@/components/Profile';
import '@/components/Profile/Profile.css';

export function ProfilePage() {
  const { t } = useLanguage();
  const { username } = useParams<{ username: string }>();
  const navigate = useNavigate();

  const profile = useAtomValue(viewedProfileAtom);
  const loading = useAtomValue(profileLoadingAtom);
  const error = useAtomValue(profileErrorAtom);
  const loadProfile = useSetAtom(loadProfileAtom);

  const [showEdit, setShowEdit] = useState(false);

  useEffect(() => {
    if (username) {
      loadProfile(username);
    }
  }, [username, loadProfile]);

  const handleBack = () => {
    navigate(-1);
  };

  if (loading) {
    return (
      <div className="profile-page">
        <div className="profile-page__container">
          <button className="profile-page__back" onClick={handleBack}>
            <ArrowLeft size={18} />
            <span>{t('common.back')}</span>
          </button>
          <div className="profile-page__loading">
            <div className="follow-button__loader" style={{ width: 32, height: 32 }} />
            <p>{t('profile.loading')}</p>
          </div>
        </div>
      </div>
    );
  }

  if (error || !profile) {
    return (
      <div className="profile-page">
        <div className="profile-page__container">
          <button className="profile-page__back" onClick={handleBack}>
            <ArrowLeft size={18} />
            <span>{t('common.back')}</span>
          </button>
          <div className="profile-page__error">
            <UserX size={64} />
            <h2>{t('profile.not_found')}</h2>
            <p>{error || t('profile.not_found_desc')}</p>
            <Link to="/" className="profile-page__error-home">
              {t('common.go_home')}
            </Link>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="profile-page">
      <div className="profile-page__container">
        <button className="profile-page__back" onClick={handleBack}>
          <ArrowLeft size={18} />
          <span>{t('common.back')}</span>
        </button>

        <ProfileHeader onEditClick={() => setShowEdit(true)} />
        <ProfileTabs />

        <ProfileEdit isOpen={showEdit} onClose={() => setShowEdit(false)} />
      </div>
    </div>
  );
}

export default ProfilePage;
