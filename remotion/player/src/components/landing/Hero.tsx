import { Link } from 'react-router-dom';
import { useLanguage } from '@/hooks/useLanguage';
import './Hero.css';

export function Hero() {
  const { t } = useLanguage();

  return (
    <section className="hero">
      <div className="hero-bg">
        <div className="hero-gradient" />
        <div className="hero-grid" />
      </div>

      <div className="hero-content">
        <h1 className="hero-title">
          {t('hero.title')}
        </h1>
        <p className="hero-subtitle">
          {t('hero.subtitle')}
        </p>
        <div className="hero-cta">
          <Link to="/editor" className="btn-primary">
            {t('cta.try')}
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
              <path d="M5 12h14M12 5l7 7-7 7"/>
            </svg>
          </Link>
          <a href="#" className="btn-secondary">
            <svg width="20" height="20" viewBox="0 0 24 24" fill="currentColor">
              <polygon points="5 3 19 12 5 21 5 3"/>
            </svg>
            {t('cta.demo')}
          </a>
        </div>

        <div className="hero-stats">
          <div className="stat">
            <span className="stat-value">100+</span>
            <span className="stat-label">AI Tools</span>
          </div>
          <div className="stat-divider" />
          <div className="stat">
            <span className="stat-value">10+</span>
            <span className="stat-label">AI Models</span>
          </div>
          <div className="stat-divider" />
          <div className="stat">
            <span className="stat-value">24/7</span>
            <span className="stat-label">MCP Server</span>
          </div>
        </div>
      </div>
    </section>
  );
}
