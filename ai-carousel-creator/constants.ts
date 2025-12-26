
import { AspectRatio, DesignPalette, FrameType } from './types';

export const ASPECT_RATIOS: { name: string; value: AspectRatio, className: string }[] = [
  { name: 'SQ (1:1)', value: '1:1', className: 'aspect-square' },
  { name: 'PORTRAIT (4:5)', value: '4:5', className: 'aspect-[4/5]' },
  { name: 'STORY (9:16)', value: '9:16', className: 'aspect-[9/16]' },
];

export const FONTS = [
  { name: 'RUSSO ONE (VIRAL)', value: '"Russo One", sans-serif' },
  { name: 'INTER (UI)', value: '"Inter", sans-serif' },
  { name: 'OSWALD (TALL)', value: '"Oswald", sans-serif' },
  { name: 'JETBRAINS (CODE)', value: '"JetBrains Mono", monospace' },
  { name: 'MONTSERRAT', value: '"Montserrat", sans-serif' },
  { name: 'ANTON (IMPACT)', value: '"Anton", sans-serif' },
  { name: 'RUBIK', value: '"Rubik", sans-serif' },
  { name: 'PLAYFAIR', value: '"Playfair Display", serif' },
];

export const ART_STYLES = [
  { name: '📸 PHOTOREALISM', value: 'photorealistic, 8k, high detail, professional photography, shot on 35mm, cinematic lighting' },
  { name: '👾 CYBERPUNK', value: 'cyberpunk style, dark background, neon accents, futuristic, glitch aesthetic, high contrast' },
  { name: '🔪 ACID GRAPHICS', value: 'acid graphics, y2k aesthetic, chrome textures, distorted shapes, neon green and black' },
  { name: '💎 3D ABSTRACT', value: '3d abstract render, glassmorphism, iridescent materials, soft studio lighting, blender 3d' },
  { name: '🌑 DARK NOIR', value: 'black and white photography, noir style, dramatic lighting, film grain, moody' },
  { name: '🎨 OIL PAINTING', value: 'oil painting texture, heavy strokes, expressive, masterpiece' },
  { name: '📐 MINIMALIST', value: 'minimalist vector art, flat design, solid colors, simple shapes, clean, bauhaus' },
];

export const FRAMES: { id: FrameType; name: string; border: string; overlay?: string }[] = [
    { id: 'none', name: 'NONE', border: 'none' },
    { id: 'browser-light', name: 'BROWSER (L)', border: '20px solid #f0f0f0', overlay: 'browser-ui-light' },
    { id: 'browser-dark', name: 'BROWSER (D)', border: '20px solid #1a1a1a', overlay: 'browser-ui-dark' },
    { id: 'film', name: 'FILM STRIP', border: 'none', overlay: 'film-holes' },
    { id: 'polaroid', name: 'POLAROID', border: '15px solid white', overlay: 'polaroid-text' },
    { id: 'paper', name: 'PAPER', border: '12px solid #fff', overlay: 'paper-texture' },
    { id: 'photo-booth', name: 'PHOTOBOOTH', border: 'none', overlay: 'photo-booth' },
];

export const STICKERS = [
    { name: 'ARROW 01', content: 'M10 50 C 40 10, 100 10, 120 50 L 110 40 M 120 50 L 130 40' },
    { name: 'ARROW 02', content: 'M10 90 Q 50 10 90 90 L 80 70 M 90 90 L 70 85' },
    { name: 'CIRCLE', content: 'M50 10 A 40 40 0 1 0 50 90 A 40 40 0 1 0 50 10' },
    { name: 'OVAL', content: 'M10 50 A 40 20 0 1 0 90 50 A 40 20 0 1 0 10 50' },
    { name: 'UNDERLINE', content: 'M10 50 Q 75 80 140 50' },
    { name: 'SCRIBBLE', content: 'M10 50 Q 30 20 50 50 T 90 50 T 130 50' },
    { name: 'STAR', content: 'M50 10 L 60 40 L 90 40 L 65 60 L 75 90 L 50 70 L 25 90 L 35 60 L 10 40 L 40 40 Z' },
    { name: 'CROSS', content: 'M20 20 L 80 80 M 80 20 L 20 80' },
    { name: 'BOX', content: 'M10 10 H90 V90 H10 Z' },
];

export const DESIGN_PALETTES: DesignPalette[] = [
  {
    name: 'ACID MODE',
    textColor: '#ffffff',
    highlightColor: '#ccff00', 
    overlayColor: '#000000',
    overlayMode: 'gradient',
    overlayOpacity: 80,
    textBackgroundColor: '#000000',
    textBackgroundOpacity: 0,
    fontFamily: '"Russo One", sans-serif'
  },
  {
    name: 'HYPEBEAST',
    textColor: '#000000',
    highlightColor: '#ff0000', 
    overlayColor: '#ffffff', 
    overlayMode: 'solid',
    overlayOpacity: 100,
    textBackgroundColor: '#ffffff',
    textBackgroundOpacity: 0,
    fontFamily: '"Oswald", sans-serif'
  },
  {
    name: 'MATRIX',
    textColor: '#00ff41',
    highlightColor: '#ffffff', 
    overlayColor: '#000000',
    overlayMode: 'solid',
    overlayOpacity: 95,
    textBackgroundColor: '#0d0208',
    textBackgroundOpacity: 0,
    fontFamily: '"JetBrains Mono", monospace'
  },
  {
    name: 'VOGUE',
    textColor: '#ffffff',
    highlightColor: '#db2777',
    overlayColor: '#000000',
    overlayMode: 'gradient',
    overlayOpacity: 40,
    textBackgroundColor: '#000000', 
    textBackgroundOpacity: 0,
    fontFamily: '"Playfair Display", serif'
  }
];

export const INITIAL_STORY_PLACEHOLDER = {
  ru: `ВВЕДИТЕ ТЕМУ ИЛИ ТЕКСТ.`,
  en: `ENTER TOPIC OR TEXT.`
};

export const TRANSLATIONS = {
  // HEADERS & UI
  CONTROL_DECK: { en: 'CONTROL_DECK_V2', ru: 'ПАНЕЛЬ_УПРАВЛЕНИЯ_V2' },
  VIEWPORT: { en: 'VIEWPORT_01', ru: 'ОКНО_ПРОСМОТРА_01' },
  NO_SIGNAL: { en: 'NO_SIGNAL', ru: 'НЕТ_СИГНАЛА' },
  INPUT_DATA_MSG: { en: 'Input Data to Initialize System', ru: 'Введите данные для запуска системы' },
  
  // TABS
  TAB_INPUT: { en: '01_INPUT', ru: '01_ВВОД' },
  TAB_EDIT: { en: '02_EDIT', ru: '02_РЕДАКТОР' },
  TAB_STYLE: { en: '03_STYLE', ru: '03_СТИЛЬ' },
  TAB_HISTORY: { en: '04_LOGS', ru: '04_АРХИВ' },
  
  // BUTTONS
  EXPORT: { en: 'EXPORT', ru: 'ЭКСПОРТ' },
  DOWNLOAD_PACK: { en: 'DOWNLOAD_PACK', ru: 'СКАЧАТЬ_ПАК' },
  INITIALIZE: { en: 'INITIALIZE GENERATION', ru: 'ЗАПУСТИТЬ ГЕНЕРАЦИЮ' },
  RE_ROLL: { en: 'RE-ROLL', ru: 'ПЕРЕГЕНЕРИРОВАТЬ' },
  ADD_FRAME: { en: 'ADD FRAME', ru: 'ДОБАВИТЬ СЛАЙД' },
  AI_REWRITE: { en: 'AI_REWRITE', ru: 'AI_РЕРАЙТ' },
  SAVE_FRAME: { en: 'SAVE_FRAME', ru: 'СОХРАНИТЬ_СЛАЙД' },
  RESET_SYSTEM: { en: 'RESET SYSTEM', ru: 'СБРОСИТЬ СИСТЕМУ' },
  LOAD_RECORD: { en: 'LOAD', ru: 'ЗАГРУЗИТЬ' },
  DELETE_RECORD: { en: 'DEL', ru: 'УДАЛ.' },
  
  // ELEMENT ACTIONS
  ADD_HEADING: { en: 'HEADER', ru: 'ЗАГОЛОВОК' },
  ADD_TEXT: { en: 'TEXT', ru: 'ТЕКСТ' },
  ADD_IMAGE: { en: 'IMAGE', ru: 'КАРТИНКА' },
  ADD_STICKER: { en: 'STICKER', ru: 'СТИКЕР' },
  LAYERS_LABEL: { en: 'LAYERS', ru: 'СЛОИ' },
  
  // TOOLBAR
  TOOL_TEXT: { en: 'TEXT', ru: 'ТЕКСТ' },
  TOOL_MEDIA: { en: 'MEDIA', ru: 'МЕДИА' },
  TOOL_FRAMES: { en: 'FRAMES', ru: 'РАМКИ' },
  TOOL_STICKERS: { en: 'STICKER', ru: 'СТИКЕР' },
  TOOL_TOOLS: { en: 'TOOLS', ru: 'ИНСТРУМ.' },

  // SUB TOOLS
  ACTION_DUPLICATE: { en: 'DUPLICATE', ru: 'ДУБЛЬ' },
  ACTION_BRIDGE: { en: 'BRIDGE (SPAN)', ru: 'МОСТ (SPAN)' },
  ACTION_UNLINK: { en: 'UNLINK', ru: 'РАЗОРВАТЬ' },
  ACTION_DELETE: { en: 'DELETE', ru: 'УДАЛИТЬ' },
  ACTION_UP: { en: 'LAYER UP', ru: 'СЛОЙ ВВЕРХ' },
  ACTION_DOWN: { en: 'LAYER DOWN', ru: 'СЛОЙ ВНИЗ' },
  ACTION_LOCK: { en: 'LOCK', ru: 'ЗАМОК' },
  ACTION_EDIT: { en: 'EDIT', ru: 'РЕДАКТ.' },

  // LABELS
  INPUT_STREAM: { en: 'Input Stream', ru: 'Входной поток' },
  GLOBAL_PRESETS: { en: 'Global Presets', ru: 'Глобальные пресеты' },
  SAVED_LOGS: { en: 'SAVED LOGS', ru: 'СОХРАНЕННЫЕ' },
  
  // REF IMAGE LABELS
  REF_IMAGE_LABEL: { en: 'AI Reference', ru: 'Референс AI' },
  REF_TYPE_LABEL: { en: 'Reference Type', ru: 'Тип' },
  REF_TYPE_PRODUCT: { en: 'Product', ru: 'Продукт' },
  REF_TYPE_CHARACTER: { en: 'Char', ru: 'Персонаж' },
  REF_TYPE_STYLE: { en: 'Style', ru: 'Стиль' },
  REMOVE_REF: { en: 'X', ru: 'X' },
  UPLOAD_REF_PLACEHOLDER: { en: 'UPLOAD REF', ru: 'ЗАГРУЗИТЬ' },

  // LOADING STATES
  INIT_NEURAL: { en: 'INITIALIZING...', ru: 'ИНИЦИАЛИЗАЦИЯ...' },
  RENDERING: { en: 'RENDERING', ru: 'РЕНДЕРИНГ' },
  BATCH_PROCESS: { en: 'BATCHING', ru: 'ПАКЕТНАЯ ОБРАБОТКА' },
  OPTIMIZING: { en: 'OPTIMIZING...', ru: 'ОПТИМИЗАЦИЯ...' },
  COMPRESSING: { en: 'COMPRESSING...', ru: 'СЖАТИЕ...' },
  GENERATING_DOC: { en: 'GENERATING DOC...', ru: 'ГЕНЕРАЦИЯ DOC...' },
  
  // ERRORS
  SYSTEM_FAILURE: { en: 'FAILURE', ru: 'СБОЙ' },
  BATCH_FAIL: { en: 'BATCH FAIL', ru: 'СБОЙ ПАКЕТА' },
  OPT_ERROR: { en: 'OPT ERROR', ru: 'ОШИБКА' },
  EXPORT_FAIL: { en: 'EXPORT FAIL', ru: 'ОШИБКА' },
  DATA_REQUIRED: { en: 'NO DATA', ru: 'НЕТ ДАННЫХ' },

  // PLACEHOLDERS
  NEW_BLOCK: { en: 'NEW BLOCK', ru: 'НОВЫЙ БЛОК' },
  DEFAULT_HEADING: { en: 'HEADING', ru: 'ЗАГОЛОВОК' },
  DEFAULT_TEXT: { en: 'Text Block', ru: 'Текстовый блок' }
};
