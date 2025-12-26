// ===============================
// Language Atoms - i18n with Jotai
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';

export type Language = 'ru' | 'en';

interface Translations {
  [key: string]: string;
}

// ===============================
// Translations
// ===============================

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

  // Editor Header
  "editor.export": "Export",
  "editor.exporting": "Rendering...",
  "editor.settings": "Settings",
  "editor.save": "Save Project",
  "editor.load": "Load Project",
  "editor.reset": "Reset to Defaults",
  "editor.undo": "Undo",
  "editor.redo": "Redo",

  // Settings Modal
  "settings.title": "Settings",
  "settings.export": "Export",
  "settings.codec": "Codec",
  "settings.quality": "Quality",
  "settings.shortcuts": "Keyboard Shortcuts",
  "settings.project": "Project",
  "settings.name": "Name",
  "settings.resolution": "Resolution",
  "settings.fps": "FPS",
  "settings.duration": "Duration",

  // Dialogs
  "dialog.reset.title": "Reset to Defaults?",
  "dialog.reset.text": "All changes will be lost. Timeline, assets, and settings will be restored to their original state.",
  "dialog.reset.warning": "This action cannot be undone.",
  "dialog.cancel": "Cancel",
  "dialog.reset": "Reset",
  "dialog.exportAnyway": "Export Anyway",

  // Blob Warning
  "blob.title": "Local Files Detected",
  "blob.text": "The following files are stored locally and will be skipped during export:",
  "blob.hint": "To include these files, delete and re-upload them. They will be stored in the cloud.",
  "blob.criticalTitle": "Cannot Export",
  "blob.criticalText": "The following required files are stored locally and cannot be accessed by the render server:",
  "blob.criticalHint": "Upload your lipsync video to the cloud first. Go to Assets panel, delete the local file and re-upload it.",
  "dialog.ok": "OK",

  // Login Modal
  "login.title": "Login to Export",
  "login.subtitle": "Sign in with Telegram to get 3 free video renders!",
  "login.button": "Login",
  "login.buttonFull": "Sign in with Telegram",

  // Quota
  "quota.unlimited": "Unlimited",
  "quota.left": "left",
  "quota.free": "free",

  // Properties Panel
  "props.properties": "Properties",
  "props.batchEdit": "Batch Edit",
  "props.items": "items",
  "props.adjustDuration": "Adjust Duration",
  "props.makeSameDuration": "Make Same Duration",
  "props.setsAllToShortest": "Sets all to shortest",
  "props.selectionInfo": "Selection Info",
  "props.content": "Content",
  "props.style": "Style",
  "props.fontSize": "Font Size",
  "props.color": "Color",
  "props.weight": "Weight",
  "props.align": "Align",
  "props.left": "Left",
  "props.center": "Center",
  "props.right": "Right",
  "props.position": "Position",
  "props.opacity": "Opacity",
  "props.timing": "Timing",
  "props.start": "Start",
  "props.duration": "Duration",
  "props.media": "Media",
  "props.lipsyncVideo": "Lipsync Video",
  "props.coverImage": "Cover Image",
  "props.backgroundMusic": "Background Music",
  "props.effects": "Effects",
  "props.musicVolume": "Music Volume",
  "props.coverDuration": "Cover Duration",
  "props.vignette": "Vignette",
  "props.colorCorrection": "Color Correction",
  "props.avatarCircle": "Avatar Circle",
  "props.size": "Size",
  "props.bottom": "Bottom",
  "props.backgrounds": "Backgrounds",
  "props.videos": "videos",
  "props.dragVideosHint": "Drag videos to Video track to change backgrounds",
  "props.enterText": "Enter text...",

  // Duration adjustment buttons
  "props.minus1s": "-1 second",
  "props.minus05s": "-0.5 second",
  "props.plus05s": "+0.5 second",
  "props.plus1s": "+1 second",
  "props.pathPlaceholder": "/path/to/file",

  // Section headers
  "section.text": "Text",
  "section.style": "Style",
  "section.position": "Position",
  "section.timing": "Timing",
  "section.media": "Media",
  "section.effects": "Effects",
  "section.avatar": "Avatar Circle",
  "section.backgrounds": "Backgrounds",
  "section.audio": "Audio",

  // Properties - Volume
  "props.volume": "Volume",

  // Font weights
  "font.light": "Light",
  "font.regular": "Regular",
  "font.medium": "Medium",
  "font.semibold": "SemiBold",
  "font.bold": "Bold",
  "font.extrabold": "ExtraBold",

  // TrackItem
  "track.clickToAdjustVolume": "Click to adjust volume",

  // Auth
  "auth.logout": "Logout",

  // Captions preview
  "captions.previewText": "Hello",

  // Layers Panel
  "layers.addText": "Add Text",
  "layers.hideTrack": "Hide track",
  "layers.showTrack": "Show track",
  "layers.lockTrack": "Lock track",
  "layers.unlockTrack": "Unlock track",
  "layers.selected": "selected",
  "layers.delete": "Delete",

  // Captions Panel
  "captions.title": "Captions",
  "captions.style": "Style",
  "captions.hide": "Hide captions",
  "captions.show": "Show captions",
  "captions.addAt": "Add at",
  "captions.add": "Add",
  "captions.uploadHint": "Upload .srt or .vtt file",
  "captions.import": "Import",
  "captions.transcribeHint": "Auto-transcribe Russian audio using Whisper",
  "captions.transcribing": "Transcribing...",
  "captions.transcribe": "Transcribe RU",
  "captions.empty": "No captions yet",
  "captions.emptyHint": "Add manually or import .srt/.vtt file",
  "captions.text": "Text",
  "captions.fontSize": "Font Size",
  "captions.fontWeight": "Font Weight",
  "captions.font": "Font",
  "captions.cyrillic": "Cyrillic",
  "captions.searchFonts": "Search fonts...",
  "captions.popular": "Popular",
  "captions.allFonts": "All Fonts",
  "captions.noFonts": "No fonts found",
  "captions.colors": "Colors",
  "captions.textColor": "Text Color",
  "captions.highlight": "Highlight",
  "captions.background": "Background",
  "captions.position": "Position",
  "captions.bottomPercent": "Bottom %",
  "captions.maxWidth": "Max Width %",
  "captions.effects": "Effects",
  "captions.textShadow": "Text Shadow",
  "captions.current": "Current",
  "captions.noVideoLoaded": "No video loaded. Please add a lip-sync video first.",
  "captions.noVideo": "No video loaded. Please add a lip-sync video first.",
  "captions.transcriptionFailed": "Transcription failed:",
  "captions.parseError": "Could not parse captions from file. Please check the format.",

  // Assets Panel
  "assets.dropOrClick": "Drop files or click to upload",
  "assets.uploadsToCloud": "Uploads to S3 cloud",
  "assets.uploading": "Uploading...",
  "assets.done": "Done",
  "assets.error": "Error",
  "assets.videos": "Videos",
  "assets.images": "Images",
  "assets.audio": "Audio",
  "assets.localWarning": "Local file - will be skipped during export!\nRe-upload to fix.",
  "assets.doubleClickHint": "Double-click or drag to timeline",
  "assets.localNoExport": "Local file - won't export",

  // Chat Panel
  "chat.offlineMessage": "I'm currently offline. The AI server will connect automatically when available. In the meantime, you can explore the template properties in the left panel.",
  "chat.applied": "Applied:",
  "chat.failedToApply": "Failed to apply action:",
  "chat.unknownError": "Unknown error",
  "chat.agent": "VIBEE Agent",
  "chat.connected": "Connected",
  "chat.offline": "Offline",
  "chat.clearChat": "Clear chat",
  "chat.chat": "Chat",
  "chat.logs": "Logs",
  "chat.capturedLogs": "captured logs",
  "chat.templateProps": "template properties",
  "chat.errors": "errors",
  "chat.placeholder": "Describe what you want to create...",
  "chat.messagePlaceholder": "Write a message or upload a file...",
  "chat.send": "Send",
  "chat.welcome": "Hi! I'm your VIBEE AI assistant. I can help you create and edit video templates. What would you like to build today?",
  "chat.cleared": "Chat cleared. How can I help?",

  // Templates Panel
  "templates.title": "Templates",

  // Timeline
  "timeline.skipToStart": "Skip to start",
  "timeline.pause": "Pause",
  "timeline.play": "Play",
  "timeline.skipToEnd": "Skip to end",
  "timeline.slower": "Slower",
  "timeline.faster": "Faster",
  "timeline.unmute": "Unmute audio",
  "timeline.mute": "Mute audio",
  "timeline.volume": "Volume",
  "timeline.snapToGrid": "Snap to grid",
  "timeline.on": "ON",
  "timeline.off": "OFF",
  "timeline.zoomOut": "Zoom out",
  "timeline.zoomIn": "Zoom in",
  "timeline.fitToView": "Fit to view",
  "timeline.in": "In",
  "timeline.out": "Out",
  "timeline.assetNotCompatible": "Asset type not compatible with track",

  // Volume Popup
  "volume.musicVolume": "Music Volume",
  "volume.avatarVolume": "Avatar Volume",
  "volume.videoVolume": "Video Volume",
  "volume.mute": "Mute",
  "volume.clickToAdjust": "Click to adjust volume",

  // Canvas
  "canvas.fullscreenNotSupported": "Fullscreen not supported or blocked",
  "canvas.exitFullscreen": "Exit fullscreen",
  "canvas.fullscreen": "Fullscreen",
  "canvas.transcribingAudio": "Transcribing audio...",
  "canvas.loadingCaptions": "Loading captions...",

  // Paywall
  "paywall.junior": "JUNIOR",
  "paywall.middle": "MIDDLE",
  "paywall.senior": "SENIOR",
  "paywall.rendersMonth": "renders/month",
  "paywall.hdQuality": "HD quality",
  "paywall.4kQuality": "4K quality",
  "paywall.emailSupport": "Email support",
  "paywall.prioritySupport": "Priority support",
  "paywall.premiumSupport": "Premium support",
  "paywall.customFonts": "Custom fonts",
  "paywall.apiAccess": "API access",
  "paywall.unlimitedRenders": "Unlimited renders",
  "paywall.card": "Card",
  "paywall.stars": "Stars",
  "paywall.ton": "TON",
  "paywall.freeUsedUp": "Free Renders Used Up!",
  "paywall.subscribeMessage": "You've used all your free renders. Subscribe to continue creating amazing videos.",
  "paywall.mostPopular": "Most Popular",
  "paywall.perMonth": "/month",
  "paywall.securePayments": "All payments are secure and processed via Telegram",

  // Auth
  "auth.login": "Login",
  "auth.signInTelegram": "Sign in with Telegram",

  // Context Menu
  "menu.copy": "Copy",
  "menu.paste": "Paste",
  "menu.duplicate": "Duplicate",
  "menu.color": "Color",
  "menu.delete": "Delete",
  "menu.noColor": "No color",
  "color.red": "Red",
  "color.orange": "Orange",
  "color.yellow": "Yellow",
  "color.green": "Green",
  "color.blue": "Blue",
  "color.purple": "Purple",
  "color.pink": "Pink",

  // Header Alerts
  "editor.invalidFormat": "Invalid project file format",
  "editor.importSuccess": "Project imported successfully!",
  "editor.importFailed": "Failed to import project. Invalid JSON format.",
  "editor.exportFailed": "Export failed",
  "editor.connectionLost": "Lost connection to render server",
  "editor.unknownError": "Unknown error",

  // Tab Tooltips
  "tabs.templates": "Templates",
  "tabs.assets": "Assets",
  "tabs.player": "Player",
  "tabs.layers": "Layers",
  "tabs.properties": "Properties",
  "tabs.captions": "Captions",

  // Player
  "player.pause": "Pause",
  "player.play": "Play",

  // Player Panel Settings
  "player.music": "Music",
  "player.musicVolume": "Volume",
  "player.effects": "Effects",
  "player.vignette": "Vignette",
  "player.colorCorrection": "Color",
  "player.avatar": "Avatar",
  "player.autoDetect": "Face Detection",
  "player.detect": "Detect",
  "player.detecting": "Detecting...",
  "player.circle": "Circle",
  "player.borderRadius": "Radius",
  "player.avatarSize": "Size",
  "player.positionX": "Position X",
  "player.positionY": "Position Y",
  "player.faceScale": "Face Scale",
  "player.captions": "Captions",
  "player.showCaptions": "Show",
  "player.playback": "Playback",
  "player.playbackSpeed": "Speed",
  "player.split": "Split",
  "player.fullscreen": "Fullscreen",
  "player.reset": "Reset to defaults",

  // WebSocket
  "ws.syncActive": "Real-time sync active",
  "ws.connecting": "Connecting to sync server...",

  // Codec Options
  "codec.h264": "H.264 (MP4) - Best compatibility",
  "codec.h265": "H.265 (HEVC) - Smaller size",
  "codec.vp9": "VP9 (WebM) - Web optimized",
  "codec.prores": "ProRes - Professional",

  // Quality Options
  "quality.high": "High (1080p)",
  "quality.medium": "Medium (720p)",
  "quality.low": "Low (480p)",

  // Keyboard Shortcuts
  "shortcuts.title": "Keyboard Shortcuts",
  "shortcuts.playback": "Playback",
  "shortcuts.editing": "Editing",
  "shortcuts.selection": "Selection",
  "shortcuts.navigation": "Navigation",
  "shortcuts.view": "View",
  "shortcut.playPause": "Play / Pause",
  "shortcut.rewind1s": "Rewind 1 second",
  "shortcut.pause": "Pause",
  "shortcut.forward1s": "Forward 1 second",
  "shortcut.prevFrame": "Previous frame",
  "shortcut.nextFrame": "Next frame",
  "shortcut.back1Frame": "Back 1 frame",
  "shortcut.forward1Frame": "Forward 1 frame",
  "shortcut.back10Frames": "Back 10 frames",
  "shortcut.forward10Frames": "Forward 10 frames",
  "shortcut.goToStart": "Go to start",
  "shortcut.goToEnd": "Go to end",
  "shortcut.jkl": "-1s / Pause / +1s",
  "shortcut.undo": "Undo",
  "shortcut.redo": "Redo",
  "shortcut.selectAll": "Select All",
  "shortcut.copy": "Copy",
  "shortcut.paste": "Paste",
  "shortcut.duplicate": "Duplicate",
  "shortcut.delete": "Delete",
  "shortcut.deleteWithGap": "Delete and close gap",
  "shortcut.splitAtPlayhead": "Split at playhead",
  "shortcut.clearSelection": "Clear Selection",
  "shortcut.addToSelection": "Add to selection",
  "shortcut.selectRange": "Select range",
  "shortcut.toSelectionStart": "To selection start",
  "shortcut.toSelectionEnd": "To selection end",
  "shortcut.setInPoint": "Set In point",
  "shortcut.setOutPoint": "Set Out point",
  "shortcut.resetInOut": "Reset In/Out points",
  "shortcut.toggleMarker": "Add/remove marker",
  "shortcut.nextMarker": "To next marker",
  "shortcut.prevMarker": "To previous marker",
  "shortcut.move1Frame": "Move 1 Frame",
  "shortcut.move10Frames": "Move 10 Frames",
  "shortcut.goToStartEnd": "Go to Start / End",
  "shortcut.zoomInOut": "Zoom In / Out",
  "shortcut.fitTimeline": "Fit to window",
  "shortcut.showShortcuts": "Show shortcuts",

  // Canvas
  "canvas.zoomOut": "Zoom out",
  "canvas.zoomIn": "Zoom in",

  // Loading
  "loading.editor": "Loading Editor...",

  // Layers - additional
  "layers.newText": "New Text",

  // Generate Panel
  "generate.title": "Generate",
  "generate.image": "Photo",
  "generate.video": "Video",
  "generate.audio": "Voice",
  "generate.lipsync": "Lipsync",
  "generate.model": "Model",
  "generate.prompt": "Prompt",
  "generate.promptPlaceholder": "Describe what you want to generate...",
  "generate.videoPromptPlaceholder": "Describe the video scene...",
  "generate.textPlaceholder": "Enter text to convert to speech...",
  "generate.aspectRatio": "Aspect Ratio",
  "generate.duration": "Duration",
  "generate.resolution": "Resolution",
  "generate.voice": "Voice",
  "generate.text": "Text",
  "generate.speed": "Speed",
  "generate.audioUrl": "Audio URL",
  "generate.imageUrl": "Image URL",
  "generate.generating": "Generating...",
  "generate.generateImage": "Generate Image",
  "generate.generateVideo": "Generate Video",
  "generate.generateAudio": "Generate Audio",
  "generate.generateLipsync": "Generate Lipsync",
  "generate.error": "Generation failed. Please try again.",
  "tabs.generate": "Generate",
  "generate.audioSource": "Voice Audio",
  "generate.uploadAudio": "Upload",
  "generate.recordAudio": "Record",
  "generate.saveRecording": "Save",
  "generate.imageSource": "Avatar Image",
  "generate.uploadImage": "Upload Photo",
  "generate.results": "Results",
  "generate.dragHint": "Drag to timeline",
  "generate.addToTimeline": "Add to timeline",
  "generate.remove": "Remove",
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

  // Editor Header
  "editor.export": "Экспорт",
  "editor.exporting": "Рендеринг...",
  "editor.settings": "Настройки",
  "editor.save": "Сохранить проект",
  "editor.load": "Загрузить проект",
  "editor.reset": "Сбросить настройки",
  "editor.undo": "Отменить",
  "editor.redo": "Повторить",

  // Settings Modal
  "settings.title": "Настройки",
  "settings.export": "Экспорт",
  "settings.codec": "Кодек",
  "settings.quality": "Качество",
  "settings.shortcuts": "Горячие клавиши",
  "settings.project": "Проект",
  "settings.name": "Название",
  "settings.resolution": "Разрешение",
  "settings.fps": "FPS",
  "settings.duration": "Длительность",

  // Dialogs
  "dialog.reset.title": "Сбросить настройки?",
  "dialog.reset.text": "Все изменения будут потеряны. Таймлайн, ассеты и настройки будут восстановлены к исходному состоянию.",
  "dialog.reset.warning": "Это действие нельзя отменить.",
  "dialog.cancel": "Отмена",
  "dialog.reset": "Сбросить",
  "dialog.exportAnyway": "Экспортировать",

  // Blob Warning
  "blob.title": "Обнаружены локальные файлы",
  "blob.text": "Следующие файлы хранятся локально и будут пропущены при экспорте:",
  "blob.hint": "Чтобы включить эти файлы, удалите их и загрузите заново. Они будут сохранены в облаке.",
  "blob.criticalTitle": "Невозможно экспортировать",
  "blob.criticalText": "Следующие обязательные файлы хранятся локально и недоступны серверу рендеринга:",
  "blob.criticalHint": "Сначала загрузите lipsync видео в облако. Откройте панель Assets, удалите локальный файл и загрузите его заново.",
  "dialog.ok": "OK",

  // Login Modal
  "login.title": "Войдите для экспорта",
  "login.subtitle": "Войдите через Telegram и получите 3 бесплатных рендера!",
  "login.button": "Войти",
  "login.buttonFull": "Войти через Telegram",

  // Quota
  "quota.unlimited": "Безлимит",
  "quota.left": "осталось",
  "quota.free": "бесплатно",

  // Properties Panel
  "props.properties": "Свойства",
  "props.batchEdit": "Групповое редактирование",
  "props.items": "элементов",
  "props.adjustDuration": "Изменить длительность",
  "props.makeSameDuration": "Сделать одинаковыми",
  "props.setsAllToShortest": "Установить минимальную",
  "props.selectionInfo": "Информация о выборке",
  "props.content": "Контент",
  "props.style": "Стиль",
  "props.fontSize": "Размер шрифта",
  "props.color": "Цвет",
  "props.weight": "Насыщенность",
  "props.align": "Выравнивание",
  "props.left": "Слева",
  "props.center": "По центру",
  "props.right": "Справа",
  "props.position": "Позиция",
  "props.opacity": "Прозрачность",
  "props.timing": "Тайминг",
  "props.start": "Начало",
  "props.duration": "Длительность",
  "props.media": "Медиа",
  "props.lipsyncVideo": "Lipsync видео",
  "props.coverImage": "Обложка",
  "props.backgroundMusic": "Фоновая музыка",
  "props.effects": "Эффекты",
  "props.musicVolume": "Громкость музыки",
  "props.coverDuration": "Длительность обложки",
  "props.vignette": "Виньетка",
  "props.colorCorrection": "Цветокоррекция",
  "props.avatarCircle": "Круг аватара",
  "props.size": "Размер",
  "props.bottom": "Снизу",
  "props.backgrounds": "Фоны",
  "props.videos": "видео",
  "props.dragVideosHint": "Перетащите видео на дорожку Video для смены фона",
  "props.enterText": "Введите текст...",

  // Duration adjustment buttons
  "props.minus1s": "-1 секунда",
  "props.minus05s": "-0.5 секунды",
  "props.plus05s": "+0.5 секунды",
  "props.plus1s": "+1 секунда",
  "props.pathPlaceholder": "/путь/к/файлу",

  // Section headers
  "section.text": "Текст",
  "section.style": "Стиль",
  "section.position": "Позиция",
  "section.timing": "Тайминг",
  "section.media": "Медиа",
  "section.effects": "Эффекты",
  "section.avatar": "Круг аватара",
  "section.backgrounds": "Фоны",
  "section.audio": "Аудио",

  // Properties - Volume
  "props.volume": "Громкость",

  // Font weights
  "font.light": "Тонкий",
  "font.regular": "Обычный",
  "font.medium": "Средний",
  "font.semibold": "Полужирный",
  "font.bold": "Жирный",
  "font.extrabold": "Сверхжирный",

  // TrackItem
  "track.clickToAdjustVolume": "Нажмите для настройки громкости",

  // Auth
  "auth.logout": "Выйти",

  // Captions preview
  "captions.previewText": "Привет",

  // Layers Panel
  "layers.addText": "Добавить текст",
  "layers.hideTrack": "Скрыть дорожку",
  "layers.showTrack": "Показать дорожку",
  "layers.lockTrack": "Заблокировать дорожку",
  "layers.unlockTrack": "Разблокировать дорожку",
  "layers.selected": "выбрано",
  "layers.delete": "Удалить",

  // Captions Panel
  "captions.title": "Субтитры",
  "captions.style": "Стиль",
  "captions.hide": "Скрыть субтитры",
  "captions.show": "Показать субтитры",
  "captions.addAt": "Добавить в",
  "captions.add": "Добавить",
  "captions.uploadHint": "Загрузить .srt или .vtt файл",
  "captions.import": "Импорт",
  "captions.transcribeHint": "Авто-транскрибация русской речи через Whisper",
  "captions.transcribing": "Транскрибация...",
  "captions.transcribe": "Транскрибировать",
  "captions.empty": "Нет субтитров",
  "captions.emptyHint": "Добавьте вручную или импортируйте .srt/.vtt файл",
  "captions.text": "Текст",
  "captions.fontSize": "Размер шрифта",
  "captions.fontWeight": "Насыщенность",
  "captions.font": "Шрифт",
  "captions.cyrillic": "Кириллица",
  "captions.searchFonts": "Поиск шрифтов...",
  "captions.popular": "Популярные",
  "captions.allFonts": "Все шрифты",
  "captions.noFonts": "Шрифты не найдены",
  "captions.colors": "Цвета",
  "captions.textColor": "Цвет текста",
  "captions.highlight": "Подсветка",
  "captions.background": "Фон",
  "captions.position": "Позиция",
  "captions.bottomPercent": "Отступ снизу %",
  "captions.maxWidth": "Макс. ширина %",
  "captions.effects": "Эффекты",
  "captions.textShadow": "Тень текста",
  "captions.current": "Текущий",
  "captions.noVideoLoaded": "Видео не загружено. Сначала добавьте lipsync видео.",
  "captions.noVideo": "Видео не загружено. Сначала добавьте lip-sync видео.",
  "captions.transcriptionFailed": "Ошибка транскрипции:",
  "captions.parseError": "Не удалось разобрать субтитры. Проверьте формат файла.",

  // Assets Panel
  "assets.dropOrClick": "Перетащите файлы или нажмите для загрузки",
  "assets.uploadsToCloud": "Загрузка в облако S3",
  "assets.uploading": "Загрузка...",
  "assets.done": "Готово",
  "assets.error": "Ошибка",
  "assets.videos": "Видео",
  "assets.images": "Изображения",
  "assets.audio": "Аудио",
  "assets.localWarning": "Локальный файл - будет пропущен при экспорте!\nПерезагрузите для исправления.",
  "assets.doubleClickHint": "Двойной клик или перетащите на таймлайн",
  "assets.localNoExport": "Локальный файл - не экспортируется",

  // Chat Panel
  "chat.offlineMessage": "Я сейчас офлайн. AI сервер подключится автоматически, когда будет доступен. Пока можете изучить настройки шаблона в левой панели.",
  "chat.applied": "Применено:",
  "chat.failedToApply": "Ошибка применения:",
  "chat.unknownError": "Неизвестная ошибка",
  "chat.agent": "VIBEE Агент",
  "chat.connected": "Подключен",
  "chat.offline": "Офлайн",
  "chat.clearChat": "Очистить чат",
  "chat.chat": "Чат",
  "chat.logs": "Логи",
  "chat.capturedLogs": "логов",
  "chat.templateProps": "свойств шаблона",
  "chat.errors": "ошибок",
  "chat.placeholder": "Опишите, что хотите создать...",
  "chat.messagePlaceholder": "Напишите сообщение или загрузите файл...",
  "chat.send": "Отправить",
  "chat.welcome": "Привет! Я VIBEE AI ассистент. Могу помочь создать и отредактировать видео шаблоны. Что хотите сделать?",
  "chat.cleared": "Чат очищен. Чем могу помочь?",

  // Templates Panel
  "templates.title": "Шаблоны",

  // Timeline
  "timeline.skipToStart": "В начало",
  "timeline.pause": "Пауза",
  "timeline.play": "Воспроизвести",
  "timeline.skipToEnd": "В конец",
  "timeline.slower": "Медленнее",
  "timeline.faster": "Быстрее",
  "timeline.unmute": "Включить звук",
  "timeline.mute": "Выключить звук",
  "timeline.volume": "Громкость",
  "timeline.snapToGrid": "Привязка к сетке",
  "timeline.on": "ВКЛ",
  "timeline.off": "ВЫКЛ",
  "timeline.zoomOut": "Уменьшить",
  "timeline.zoomIn": "Увеличить",
  "timeline.fitToView": "По размеру окна",
  "timeline.in": "Вход",
  "timeline.out": "Выход",
  "timeline.assetNotCompatible": "Тип файла несовместим с дорожкой",

  // Volume Popup
  "volume.musicVolume": "Громкость музыки",
  "volume.avatarVolume": "Громкость аватара",
  "volume.videoVolume": "Громкость видео",
  "volume.mute": "Без звука",
  "volume.clickToAdjust": "Нажмите для настройки громкости",

  // Canvas
  "canvas.fullscreenNotSupported": "Полноэкранный режим не поддерживается или заблокирован",
  "canvas.exitFullscreen": "Выйти из полноэкранного режима",
  "canvas.fullscreen": "Полноэкранный режим",
  "canvas.transcribingAudio": "Транскрибация аудио...",
  "canvas.loadingCaptions": "Загрузка субтитров...",

  // Paywall
  "paywall.junior": "JUNIOR",
  "paywall.middle": "MIDDLE",
  "paywall.senior": "SENIOR",
  "paywall.rendersMonth": "рендеров/месяц",
  "paywall.hdQuality": "HD качество",
  "paywall.4kQuality": "4K качество",
  "paywall.emailSupport": "Email поддержка",
  "paywall.prioritySupport": "Приоритетная поддержка",
  "paywall.premiumSupport": "Премиум поддержка",
  "paywall.customFonts": "Кастомные шрифты",
  "paywall.apiAccess": "API доступ",
  "paywall.unlimitedRenders": "Безлимитные рендеры",
  "paywall.card": "Карта",
  "paywall.stars": "Stars",
  "paywall.ton": "TON",
  "paywall.freeUsedUp": "Бесплатные рендеры закончились!",
  "paywall.subscribeMessage": "Вы использовали все бесплатные рендеры. Подпишитесь, чтобы продолжить создавать видео.",
  "paywall.mostPopular": "Популярный",
  "paywall.perMonth": "/месяц",
  "paywall.securePayments": "Все платежи безопасны и обрабатываются через Telegram",

  // Auth
  "auth.login": "Войти",
  "auth.signInTelegram": "Войти через Telegram",

  // Context Menu
  "menu.copy": "Копировать",
  "menu.paste": "Вставить",
  "menu.duplicate": "Дублировать",
  "menu.color": "Цвет",
  "menu.delete": "Удалить",
  "menu.noColor": "Без цвета",
  "color.red": "Красный",
  "color.orange": "Оранжевый",
  "color.yellow": "Жёлтый",
  "color.green": "Зелёный",
  "color.blue": "Синий",
  "color.purple": "Фиолетовый",
  "color.pink": "Розовый",

  // Header Alerts
  "editor.invalidFormat": "Неверный формат файла проекта",
  "editor.importSuccess": "Проект успешно импортирован!",
  "editor.importFailed": "Ошибка импорта. Неверный формат JSON.",
  "editor.exportFailed": "Ошибка экспорта",
  "editor.connectionLost": "Потеряно соединение с сервером рендеринга",
  "editor.unknownError": "Неизвестная ошибка",

  // Tab Tooltips
  "tabs.templates": "Шаблоны",
  "tabs.assets": "Ассеты",
  "tabs.player": "Плеер",
  "tabs.layers": "Слои",
  "tabs.properties": "Свойства",
  "tabs.captions": "Субтитры",

  // Player
  "player.pause": "Пауза",
  "player.play": "Воспроизвести",

  // Player Panel Settings
  "player.music": "Музыка",
  "player.musicVolume": "Громкость",
  "player.effects": "Эффекты",
  "player.vignette": "Виньетка",
  "player.colorCorrection": "Цвет",
  "player.avatar": "Аватар",
  "player.autoDetect": "Авто-лицо",
  "player.detect": "Найти",
  "player.detecting": "Поиск...",
  "player.circle": "Круг",
  "player.borderRadius": "Радиус",
  "player.avatarSize": "Размер",
  "player.positionX": "Позиция X",
  "player.positionY": "Позиция Y",
  "player.faceScale": "Масштаб лица",
  "player.captions": "Субтитры",
  "player.showCaptions": "Показать",
  "player.playback": "Воспроизведение",
  "player.playbackSpeed": "Скорость",
  "player.split": "Сплит",
  "player.fullscreen": "На весь экран",
  "player.reset": "Сбросить",

  // WebSocket
  "ws.syncActive": "Синхронизация активна",
  "ws.connecting": "Подключение к серверу синхронизации...",

  // Codec Options
  "codec.h264": "H.264 (MP4) - Лучшая совместимость",
  "codec.h265": "H.265 (HEVC) - Меньший размер",
  "codec.vp9": "VP9 (WebM) - Для веба",
  "codec.prores": "ProRes - Профессиональный",

  // Quality Options
  "quality.high": "Высокое (1080p)",
  "quality.medium": "Среднее (720p)",
  "quality.low": "Низкое (480p)",

  // Keyboard Shortcuts
  "shortcuts.title": "Горячие клавиши",
  "shortcuts.playback": "Воспроизведение",
  "shortcuts.editing": "Редактирование",
  "shortcuts.selection": "Выделение",
  "shortcuts.navigation": "Навигация",
  "shortcuts.view": "Вид",
  "shortcut.playPause": "Плей / Пауза",
  "shortcut.rewind1s": "Назад на 1 секунду",
  "shortcut.pause": "Пауза",
  "shortcut.forward1s": "Вперёд на 1 секунду",
  "shortcut.prevFrame": "Предыдущий кадр",
  "shortcut.nextFrame": "Следующий кадр",
  "shortcut.back1Frame": "Назад на 1 кадр",
  "shortcut.forward1Frame": "Вперёд на 1 кадр",
  "shortcut.back10Frames": "Назад на 10 кадров",
  "shortcut.forward10Frames": "Вперёд на 10 кадров",
  "shortcut.goToStart": "Перейти в начало",
  "shortcut.goToEnd": "Перейти в конец",
  "shortcut.jkl": "-1с / Пауза / +1с",
  "shortcut.undo": "Отменить",
  "shortcut.redo": "Повторить",
  "shortcut.selectAll": "Выделить всё",
  "shortcut.copy": "Копировать",
  "shortcut.paste": "Вставить",
  "shortcut.duplicate": "Дублировать",
  "shortcut.delete": "Удалить",
  "shortcut.deleteWithGap": "Удалить с закрытием gap",
  "shortcut.splitAtPlayhead": "Разрезать на playhead",
  "shortcut.clearSelection": "Снять выделение",
  "shortcut.addToSelection": "Добавить к выделению",
  "shortcut.selectRange": "Выделить диапазон",
  "shortcut.toSelectionStart": "К началу выделения",
  "shortcut.toSelectionEnd": "К концу выделения",
  "shortcut.setInPoint": "Установить In point",
  "shortcut.setOutPoint": "Установить Out point",
  "shortcut.resetInOut": "Сбросить In/Out points",
  "shortcut.toggleMarker": "Добавить/удалить маркер",
  "shortcut.nextMarker": "К следующему маркеру",
  "shortcut.prevMarker": "К предыдущему маркеру",
  "shortcut.move1Frame": "Сдвиг на 1 кадр",
  "shortcut.move10Frames": "Сдвиг на 10 кадров",
  "shortcut.goToStartEnd": "В начало / конец",
  "shortcut.zoomInOut": "Увеличить / Уменьшить",
  "shortcut.fitTimeline": "Вместить в окно",
  "shortcut.showShortcuts": "Показать горячие клавиши",

  // Canvas
  "canvas.zoomOut": "Уменьшить",
  "canvas.zoomIn": "Увеличить",

  // Loading
  "loading.editor": "Загрузка редактора...",

  // Layers - additional
  "layers.newText": "Новый текст",

  // Generate Panel
  "generate.title": "Генерация",
  "generate.image": "Фото",
  "generate.video": "Видео",
  "generate.audio": "Голос",
  "generate.lipsync": "Липсинк",
  "generate.model": "Модель",
  "generate.prompt": "Промпт",
  "generate.promptPlaceholder": "Опишите, что хотите сгенерировать...",
  "generate.videoPromptPlaceholder": "Опишите видео сцену...",
  "generate.textPlaceholder": "Введите текст для озвучки...",
  "generate.aspectRatio": "Соотношение",
  "generate.duration": "Длительность",
  "generate.resolution": "Разрешение",
  "generate.voice": "Голос",
  "generate.text": "Текст",
  "generate.speed": "Скорость",
  "generate.audioUrl": "URL аудио",
  "generate.imageUrl": "URL картинки",
  "generate.generating": "Генерация...",
  "generate.generateImage": "Сгенерировать фото",
  "generate.generateVideo": "Сгенерировать видео",
  "generate.generateAudio": "Сгенерировать аудио",
  "generate.generateLipsync": "Сгенерировать липсинк",
  "generate.error": "Ошибка генерации. Попробуйте еще раз.",
  "tabs.generate": "Генерация",
  "generate.audioSource": "Голосовое аудио",
  "generate.uploadAudio": "Загрузить",
  "generate.recordAudio": "Записать",
  "generate.saveRecording": "Сохранить",
  "generate.imageSource": "Фото аватара",
  "generate.uploadImage": "Загрузить фото",
  "generate.results": "Результаты",
  "generate.dragHint": "Перетащите на таймлайн",
  "generate.addToTimeline": "Добавить на таймлайн",
  "generate.remove": "Удалить",
};

const translations: Record<Language, Translations> = { en, ru };

// ===============================
// Detect browser language
// ===============================

function detectBrowserLanguage(): Language {
  if (typeof window === 'undefined') return 'en';
  const browserLang = navigator.language.toLowerCase();
  if (browserLang.startsWith('ru')) {
    return 'ru';
  }
  return 'en';
}

// ===============================
// Atoms
// ===============================

// Language atom with localStorage persistence
export const languageAtom = atomWithStorage<Language>(
  'vibee-lang',
  detectBrowserLanguage()
);

// Translation function atom (derived)
export const translateAtom = atom((get) => {
  const lang = get(languageAtom);
  return (key: string): string => {
    return translations[lang][key] || key;
  };
});

// Set language action
export const setLanguageAtom = atom(
  null,
  (get, set, lang: Language) => {
    set(languageAtom, lang);
  }
);
