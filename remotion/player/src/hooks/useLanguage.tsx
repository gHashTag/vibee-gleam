import { createContext, useContext, useState, useEffect, type ReactNode } from 'react';

type Language = 'ru' | 'en';

interface Translations {
  [key: string]: string;
}

interface LanguageContextType {
  lang: Language;
  setLang: (lang: Language) => void;
  t: (key: string) => string;
}

const en: Translations = {
  // Header
  "nav.features": "Features",
  "nav.pricing": "Pricing",
  "nav.docs": "Docs",
  "nav.getStarted": "Get Started",

  // Hero
  "hero.title": "AI Video Generation Platform",
  "hero.subtitle": "Create stunning videos with AI avatars, lip-sync technology, and 100+ automation tools. Buzz with creativity! 🐝",
  "cta.try": "Try Free",
  "cta.demo": "Watch Demo",

  // Features
  "features.title": "Powerful Features",
  "features.ai.title": "AI Video Generator",
  "features.ai.desc": "Generate professional videos with Kling, Veo3, and Sora AI models",
  "features.lipsync.title": "Lip-Sync Avatars",
  "features.lipsync.desc": "Create talking avatars with Hedra & HeyGen integration",
  "features.telegram.title": "Telegram Integration",
  "features.telegram.desc": "Full MTProto API, bots, and message automation",
  "features.crypto.title": "Crypto Payments",
  "features.crypto.desc": "TON, USDT, P2P trading built-in",

  // Integrations
  "integrations.title": "Powered by Leading AI",
  "integrations.subtitle": "Integrate with the best AI services in one platform",

  // How it Works
  "howItWorks.title": "How It Works",
  "howItWorks.step1.title": "Upload",
  "howItWorks.step1.desc": "Upload your media assets - images, videos, or audio files",
  "howItWorks.step2.title": "Generate",
  "howItWorks.step2.desc": "Choose AI model and customize your video settings",
  "howItWorks.step3.title": "Export",
  "howItWorks.step3.desc": "Download your video or share directly to social media",

  // Pricing
  "pricing.title": "Simple Pricing",
  "pricing.subtitle": "Choose the plan that fits your needs",
  "pricing.free.name": "Free",
  "pricing.free.price": "$0",
  "pricing.free.period": "/month",
  "pricing.free.feature1": "10 renders/month",
  "pricing.free.feature2": "Basic AI models",
  "pricing.free.feature3": "720p export",
  "pricing.free.feature4": "Community support",
  "pricing.free.cta": "Start Free",
  "pricing.pro.name": "Pro",
  "pricing.pro.price": "$29",
  "pricing.pro.period": "/month",
  "pricing.pro.feature1": "100 renders/month",
  "pricing.pro.feature2": "All AI models",
  "pricing.pro.feature3": "4K export",
  "pricing.pro.feature4": "Priority support",
  "pricing.pro.feature5": "Remove watermark",
  "pricing.pro.cta": "Get Pro",
  "pricing.pro.popular": "Popular",
  "pricing.business.name": "Business",
  "pricing.business.price": "$99",
  "pricing.business.period": "/month",
  "pricing.business.feature1": "Unlimited renders",
  "pricing.business.feature2": "API access",
  "pricing.business.feature3": "Custom models",
  "pricing.business.feature4": "Dedicated support",
  "pricing.business.feature5": "White-label option",
  "pricing.business.cta": "Contact Sales",

  // Errors
  "errors.title": "Something went wrong",
  "errors.subtitle": "An unexpected error occurred",
  "errors.tryAgain": "Try Again",
  "errors.reload": "Reload Page",
  "errors.clearAndReload": "Clear Data & Reload",
  "errors.showDetails": "Show Error Details",
  "errors.support": "If the problem persists, contact support",

  // Footer
  "footer.product": "Product",
  "footer.features": "Features",
  "footer.pricing": "Pricing",
  "footer.api": "API",
  "footer.company": "Company",
  "footer.about": "About",
  "footer.blog": "Blog",
  "footer.careers": "Careers",
  "footer.support": "Support",
  "footer.docs": "Documentation",
  "footer.help": "Help Center",
  "footer.contact": "Contact",
  "footer.rights": "All rights reserved.",
};

const ru: Translations = {
  // Header
  "nav.features": "Возможности",
  "nav.pricing": "Цены",
  "nav.docs": "Документация",
  "nav.getStarted": "Начать",

  // Hero
  "hero.title": "Платформа AI Видео Генерации",
  "hero.subtitle": "Создавайте профессиональные видео с AI аватарами, lip-sync технологией и 100+ инструментами автоматизации. Жужжим креативно! 🐝",
  "cta.try": "Попробовать",
  "cta.demo": "Смотреть демо",

  // Features
  "features.title": "Возможности",
  "features.ai.title": "AI Видео Генератор",
  "features.ai.desc": "Генерация видео с Kling, Veo3 и Sora AI моделями",
  "features.lipsync.title": "Lip-Sync Аватары",
  "features.lipsync.desc": "Говорящие аватары с интеграцией Hedra и HeyGen",
  "features.telegram.title": "Telegram Интеграция",
  "features.telegram.desc": "Полный MTProto API, боты и автоматизация сообщений",
  "features.crypto.title": "Крипто Платежи",
  "features.crypto.desc": "Встроенная поддержка TON, USDT и P2P торговли",

  // Integrations
  "integrations.title": "На базе лучших AI",
  "integrations.subtitle": "Интеграция с лучшими AI сервисами на одной платформе",

  // How it Works
  "howItWorks.title": "Как это работает",
  "howItWorks.step1.title": "Загрузите",
  "howItWorks.step1.desc": "Загрузите медиа файлы - изображения, видео или аудио",
  "howItWorks.step2.title": "Генерируйте",
  "howItWorks.step2.desc": "Выберите AI модель и настройте параметры видео",
  "howItWorks.step3.title": "Экспортируйте",
  "howItWorks.step3.desc": "Скачайте видео или поделитесь в соцсетях",

  // Pricing
  "pricing.title": "Простые цены",
  "pricing.subtitle": "Выберите подходящий план",
  "pricing.free.name": "Бесплатно",
  "pricing.free.price": "$0",
  "pricing.free.period": "/месяц",
  "pricing.free.feature1": "10 рендеров/месяц",
  "pricing.free.feature2": "Базовые AI модели",
  "pricing.free.feature3": "Экспорт 720p",
  "pricing.free.feature4": "Поддержка сообщества",
  "pricing.free.cta": "Начать бесплатно",
  "pricing.pro.name": "Pro",
  "pricing.pro.price": "$29",
  "pricing.pro.period": "/месяц",
  "pricing.pro.feature1": "100 рендеров/месяц",
  "pricing.pro.feature2": "Все AI модели",
  "pricing.pro.feature3": "Экспорт 4K",
  "pricing.pro.feature4": "Приоритетная поддержка",
  "pricing.pro.feature5": "Без водяного знака",
  "pricing.pro.cta": "Выбрать Pro",
  "pricing.pro.popular": "Популярный",
  "pricing.business.name": "Бизнес",
  "pricing.business.price": "$99",
  "pricing.business.period": "/месяц",
  "pricing.business.feature1": "Безлимитные рендеры",
  "pricing.business.feature2": "API доступ",
  "pricing.business.feature3": "Кастомные модели",
  "pricing.business.feature4": "Выделенная поддержка",
  "pricing.business.feature5": "White-label опция",
  "pricing.business.cta": "Связаться",

  // Errors
  "errors.title": "Что-то пошло не так",
  "errors.subtitle": "Произошла непредвиденная ошибка",
  "errors.tryAgain": "Попробовать снова",
  "errors.reload": "Перезагрузить страницу",
  "errors.clearAndReload": "Очистить данные и перезагрузить",
  "errors.showDetails": "Показать детали ошибки",
  "errors.support": "Если проблема сохраняется, обратитесь в поддержку",

  // Footer
  "footer.product": "Продукт",
  "footer.features": "Возможности",
  "footer.pricing": "Цены",
  "footer.api": "API",
  "footer.company": "Компания",
  "footer.about": "О нас",
  "footer.blog": "Блог",
  "footer.careers": "Карьера",
  "footer.support": "Поддержка",
  "footer.docs": "Документация",
  "footer.help": "Центр помощи",
  "footer.contact": "Контакты",
  "footer.rights": "Все права защищены.",
};

const translations: Record<Language, Translations> = { en, ru };

const LanguageContext = createContext<LanguageContextType | undefined>(undefined);

export function LanguageProvider({ children }: { children: ReactNode }) {
  const [lang, setLangState] = useState<Language>(() => {
    if (typeof window !== 'undefined') {
      const saved = localStorage.getItem('vibee-lang') as Language;
      if (saved && (saved === 'ru' || saved === 'en')) {
        return saved;
      }
      // Auto-detect from browser
      const browserLang = navigator.language.toLowerCase();
      if (browserLang.startsWith('ru')) {
        return 'ru';
      }
    }
    return 'en';
  });

  useEffect(() => {
    localStorage.setItem('vibee-lang', lang);
  }, [lang]);

  const setLang = (newLang: Language) => {
    setLangState(newLang);
  };

  const t = (key: string): string => {
    return translations[lang][key] || key;
  };

  return (
    <LanguageContext.Provider value={{ lang, setLang, t }}>
      {children}
    </LanguageContext.Provider>
  );
}

export function useLanguage() {
  const context = useContext(LanguageContext);
  if (!context) {
    throw new Error('useLanguage must be used within a LanguageProvider');
  }
  return context;
}
