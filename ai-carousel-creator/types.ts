
export type AspectRatio = '1:1' | '4:5' | '9:16';
export type OverlayMode = 'gradient' | 'solid' | 'none';
export type Language = 'en' | 'ru';
export type CornerPosition = 'top-left' | 'top-right' | 'bottom-left' | 'bottom-right';
export type VerticalPosition = 'top' | 'bottom';
export type ElementType = 'heading' | 'text' | 'image' | 'sticker'; 
export type ReferenceType = 'product' | 'character' | 'style';
export type FrameType = 'none' | 'browser-light' | 'browser-dark' | 'film' | 'polaroid' | 'paper' | 'photo-booth';

export interface GlobalSettings {
  // Typography
  fontFamily: string;
  textAlign: 'left' | 'center' | 'right';
  fontSize: number;
  lineHeight: number;
  textPadding: number;
  isUppercase: boolean;
  
  // Colors & Backgrounds
  textColor: string;
  highlightColor: string; 
  textBackgroundColor: string;
  textBackgroundOpacity: number;
  
  // Overlay / Image Effects
  overlayMode: OverlayMode;
  overlayColor: string;
  overlayOpacity: number;
  fadeHeight: number;
  
  // Misc
  cornerRadius: number;
  shadowIntensity: number;
  showSwipeArrow: boolean;
  swipeIndicatorPosition: VerticalPosition;
  artStyle: string;

  // Branding
  brandingText: string;
  brandingPosition: CornerPosition;
}

export interface SharedMeta {
  enabled: boolean;
  slideIds: string[];
}

export interface SlideElement {
  id: string;
  type: ElementType;
  content: string; // Text string or Image URL or SVG Path for stickers
  position: { x: number; y: number };
  scale: number;
  rotation: number;
  width?: number; // For images
  zIndex: number;
  
  // New visual props
  opacity?: number;
  filter?: string; // e.g. 'grayscale(100%)'
  lock?: boolean;
  shared?: SharedMeta;
  
  // Individual Overrides
  fontSize?: number;
  fontFamily?: string;
  textColor?: string;
  textAlign?: 'left' | 'center' | 'right';
  isUppercase?: boolean;
}

export interface Slide {
  id: string;
  imageUrl: string;
  imagePrompt: string;
  elements: SlideElement[];
  frame?: FrameType; // New: Slide frame style
}

export interface DesignPalette {
  name: string;
  textColor: string;
  highlightColor: string;
  overlayColor: string;
  overlayMode: OverlayMode;
  overlayOpacity: number;
  textBackgroundColor: string;
  textBackgroundOpacity: number;
  fontFamily: string;
}

export interface HistoryItem {
  id: string;
  timestamp: number;
  topic: string;
  slides: Slide[];
  settings: GlobalSettings;
  aspectRatio: AspectRatio;
}
