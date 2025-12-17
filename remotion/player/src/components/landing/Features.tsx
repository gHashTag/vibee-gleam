import { useLanguage } from '@/hooks/useLanguage';
import './Features.css';

const features = [
  {
    emoji: '🎬',
    titleKey: 'features.ai.title',
    descKey: 'features.ai.desc',
    gradient: 'linear-gradient(135deg, #f59e0b 0%, #fbbf24 100%)',
  },
  {
    emoji: '🗣️',
    titleKey: 'features.lipsync.title',
    descKey: 'features.lipsync.desc',
    gradient: 'linear-gradient(135deg, #f59e0b 0%, #fcd34d 100%)',
  },
  {
    emoji: '📱',
    titleKey: 'features.telegram.title',
    descKey: 'features.telegram.desc',
    gradient: 'linear-gradient(135deg, #0088cc 0%, #29b6f6 100%)',
  },
  {
    emoji: '💰',
    titleKey: 'features.crypto.title',
    descKey: 'features.crypto.desc',
    gradient: 'linear-gradient(135deg, #f59e0b 0%, #fbbf24 100%)',
  },
];

export function Features() {
  const { t } = useLanguage();

  return (
    <section className="features" id="features">
      <div className="features-container">
        <div className="features-header">
          <h2 className="features-title">{t('features.title')}</h2>
        </div>

        <div className="features-grid">
          {features.map((feature, index) => (
            <div
              className="feature-card"
              key={index}
              style={{ '--delay': `${index * 0.1}s` } as React.CSSProperties}
            >
              <div
                className="feature-icon"
                style={{ background: feature.gradient }}
              >
                <span className="feature-emoji">{feature.emoji}</span>
              </div>
              <h3 className="feature-title">{t(feature.titleKey)}</h3>
              <p className="feature-desc">{t(feature.descKey)}</p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
