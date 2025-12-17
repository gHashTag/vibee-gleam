import { Link } from 'react-router-dom';
import { useLanguage } from '@/hooks/useLanguage';
import './Header.css';

export function Header() {
  const { lang, setLang, t } = useLanguage();

  return (
    <header className="landing-header">
      <div className="header-container">
        <Link to="/" className="header-logo">
          <span className="logo-icon">🐝</span>
          <span className="logo-text">VIBEE</span>
        </Link>

        <nav className="header-nav">
          <a href="#features" className="nav-link">{t('nav.features')}</a>
          <a href="#pricing" className="nav-link">{t('nav.pricing')}</a>
          <a href="#" className="nav-link">{t('nav.docs')}</a>
        </nav>

        <div className="header-actions">
          <div className="lang-switcher">
            <button
              className={`lang-btn ${lang === 'en' ? 'active' : ''}`}
              onClick={() => setLang('en')}
            >
              EN
            </button>
            <button
              className={`lang-btn ${lang === 'ru' ? 'active' : ''}`}
              onClick={() => setLang('ru')}
            >
              RU
            </button>
          </div>
          <Link to="/editor" className="btn-cta">
            {t('nav.getStarted')}
          </Link>
        </div>
      </div>
    </header>
  );
}
