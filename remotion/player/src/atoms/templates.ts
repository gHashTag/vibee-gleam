// ===============================
// Templates Atoms - Video templates management
// ===============================

import { atom, type Getter, type Setter } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import type { Asset, Track, CaptionStyle } from '@/store/types';
import type { AvatarAnimation, AvatarBorderEffect } from '@/shared/types';
import { STORAGE_KEYS } from './storageKeys';
import {
  // Media
  lipSyncVideoAtom,
  coverImageAtom,
  backgroundMusicAtom,
  musicVolumeAtom,
  coverDurationAtom,
  // Effects
  vignetteStrengthAtom,
  colorCorrectionAtom,
  // Legacy avatar settings
  circleSizePercentAtom,
  circleBottomPercentAtom,
  circleLeftPercentAtom,
  faceOffsetXAtom,
  faceOffsetYAtom,
  faceScaleAtom,
  isCircleAvatarAtom,
  avatarBorderRadiusAtom,
  // Split/Fullscreen mode settings (CONSOLIDATED)
  type AvatarModeSettings,
  splitAvatarSettingsAtom,
  fullscreenAvatarSettingsAtom,
  // Animation
  avatarAnimationAtom,
  // Border effect
  avatarBorderEffectAtom,
  avatarBorderColorAtom,
  avatarBorderColor2Atom,
  avatarBorderWidthAtom,
  avatarBorderIntensityAtom,
  // Captions
  captionStyleAtom,
  showCaptionsAtom,
} from './derived/templateProps';

// ===============================
// Per-Template Settings Interface
// ===============================

export interface TemplateSettings {
  // Media
  lipSyncVideo?: string;
  coverImage?: string;
  backgroundMusic?: string;
  musicVolume?: number;
  coverDuration?: number;
  // Effects
  vignetteStrength?: number;
  colorCorrection?: number;
  // Legacy avatar settings
  circleSizePercent?: number;
  circleBottomPercent?: number;
  circleLeftPercent?: number;
  faceOffsetX?: number;
  faceOffsetY?: number;
  faceScale?: number;
  isCircleAvatar?: boolean;
  avatarBorderRadius?: number;
  // Avatar mode settings (CONSOLIDATED - replaces 12 individual settings)
  splitAvatarSettings?: AvatarModeSettings;
  fullscreenAvatarSettings?: AvatarModeSettings;
  // Animation
  avatarAnimation?: AvatarAnimation;
  // Border effect
  avatarBorderEffect?: AvatarBorderEffect;
  avatarBorderColor?: string;
  avatarBorderColor2?: string;
  avatarBorderWidth?: number;
  avatarBorderIntensity?: number;
  // Captions
  captionStyle?: CaptionStyle;
  showCaptions?: boolean;
}

// ===============================
// Helper Functions
// ===============================

/**
 * Capture current settings from all atoms
 */
export function captureCurrentSettings(get: Getter): TemplateSettings {
  return {
    // Media
    lipSyncVideo: get(lipSyncVideoAtom),
    coverImage: get(coverImageAtom),
    backgroundMusic: get(backgroundMusicAtom),
    musicVolume: get(musicVolumeAtom),
    coverDuration: get(coverDurationAtom),
    // Effects
    vignetteStrength: get(vignetteStrengthAtom),
    colorCorrection: get(colorCorrectionAtom),
    // Legacy avatar settings
    circleSizePercent: get(circleSizePercentAtom),
    circleBottomPercent: get(circleBottomPercentAtom),
    circleLeftPercent: get(circleLeftPercentAtom),
    faceOffsetX: get(faceOffsetXAtom),
    faceOffsetY: get(faceOffsetYAtom),
    faceScale: get(faceScaleAtom),
    isCircleAvatar: get(isCircleAvatarAtom),
    avatarBorderRadius: get(avatarBorderRadiusAtom),
    // Avatar mode settings (CONSOLIDATED - 12 → 2)
    splitAvatarSettings: { ...get(splitAvatarSettingsAtom) },
    fullscreenAvatarSettings: { ...get(fullscreenAvatarSettingsAtom) },
    // Animation
    avatarAnimation: get(avatarAnimationAtom),
    // Border effect
    avatarBorderEffect: get(avatarBorderEffectAtom),
    avatarBorderColor: get(avatarBorderColorAtom),
    avatarBorderColor2: get(avatarBorderColor2Atom),
    avatarBorderWidth: get(avatarBorderWidthAtom),
    avatarBorderIntensity: get(avatarBorderIntensityAtom),
    // Captions
    captionStyle: get(captionStyleAtom),
    showCaptions: get(showCaptionsAtom),
  };
}

/**
 * Apply settings to all atoms
 */
export function applySettings(set: Setter, settings: TemplateSettings): void {
  // Media
  if (settings.lipSyncVideo !== undefined) set(lipSyncVideoAtom, settings.lipSyncVideo);
  if (settings.coverImage !== undefined) set(coverImageAtom, settings.coverImage);
  if (settings.backgroundMusic !== undefined) set(backgroundMusicAtom, settings.backgroundMusic);
  if (settings.musicVolume !== undefined) set(musicVolumeAtom, settings.musicVolume);
  if (settings.coverDuration !== undefined) set(coverDurationAtom, settings.coverDuration);
  // Effects
  if (settings.vignetteStrength !== undefined) set(vignetteStrengthAtom, settings.vignetteStrength);
  if (settings.colorCorrection !== undefined) set(colorCorrectionAtom, settings.colorCorrection);
  // Legacy avatar settings
  if (settings.circleSizePercent !== undefined) set(circleSizePercentAtom, settings.circleSizePercent);
  if (settings.circleBottomPercent !== undefined) set(circleBottomPercentAtom, settings.circleBottomPercent);
  if (settings.circleLeftPercent !== undefined) set(circleLeftPercentAtom, settings.circleLeftPercent);
  if (settings.faceOffsetX !== undefined) set(faceOffsetXAtom, settings.faceOffsetX);
  if (settings.faceOffsetY !== undefined) set(faceOffsetYAtom, settings.faceOffsetY);
  if (settings.faceScale !== undefined) set(faceScaleAtom, settings.faceScale);
  if (settings.isCircleAvatar !== undefined) set(isCircleAvatarAtom, settings.isCircleAvatar);
  if (settings.avatarBorderRadius !== undefined) set(avatarBorderRadiusAtom, settings.avatarBorderRadius);
  // Avatar mode settings (CONSOLIDATED - 12 → 2)
  if (settings.splitAvatarSettings !== undefined) set(splitAvatarSettingsAtom, settings.splitAvatarSettings);
  if (settings.fullscreenAvatarSettings !== undefined) set(fullscreenAvatarSettingsAtom, settings.fullscreenAvatarSettings);
  // Animation
  if (settings.avatarAnimation !== undefined) set(avatarAnimationAtom, settings.avatarAnimation);
  // Border effect
  if (settings.avatarBorderEffect !== undefined) set(avatarBorderEffectAtom, settings.avatarBorderEffect);
  if (settings.avatarBorderColor !== undefined) set(avatarBorderColorAtom, settings.avatarBorderColor);
  if (settings.avatarBorderColor2 !== undefined) set(avatarBorderColor2Atom, settings.avatarBorderColor2);
  if (settings.avatarBorderWidth !== undefined) set(avatarBorderWidthAtom, settings.avatarBorderWidth);
  if (settings.avatarBorderIntensity !== undefined) set(avatarBorderIntensityAtom, settings.avatarBorderIntensity);
  // Captions
  if (settings.captionStyle !== undefined) set(captionStyleAtom, settings.captionStyle);
  if (settings.showCaptions !== undefined) set(showCaptionsAtom, settings.showCaptions);
}

export interface Template {
  id: string;
  name: string;
  description: string;
  thumbnail?: string;
  compositionId: string;
  // Default props for the template
  defaultProps: Record<string, unknown>;
  // Assets used in the template
  assets?: Asset[];
  // Tracks (timeline items)
  tracks?: Track[];
  // Created timestamp
  createdAt?: number;
  // Is user-created (can be deleted)
  isUserCreated?: boolean;
}

// Default built-in templates
const DEFAULT_TEMPLATES: Template[] = [
  {
    id: 'vibee-reel-1',
    name: 'Vibee Reel 1',
    description: 'Split-screen talking head with B-roll',
    compositionId: 'SplitTalkingHead',
    defaultProps: {
      splitRatio: 0.5,
      musicVolume: 0.06,
      showCaptions: true,
    },
    isUserCreated: false,
  },
];

// Available templates - persisted in localStorage
export const templatesAtom = atomWithStorage<Template[]>(
  STORAGE_KEYS.templates,
  DEFAULT_TEMPLATES
);

// Currently selected template ID
export const selectedTemplateIdAtom = atomWithStorage<string | null>(
  STORAGE_KEYS.selectedTemplate,
  'vibee-reel-1'
);

// Per-template settings storage
export const templateSettingsAtom = atomWithStorage<Record<string, TemplateSettings>>(
  STORAGE_KEYS.templateSettings,
  {}
);

// Derived: Get selected template
export const selectedTemplateAtom = atom((get) => {
  const templates = get(templatesAtom);
  const selectedId = get(selectedTemplateIdAtom);
  return templates.find((t) => t.id === selectedId) || templates[0];
});

// Action: Select template (with save/load settings)
export const selectTemplateAtom = atom(
  null,
  (get, set, templateId: string) => {
    const templates = get(templatesAtom);
    const template = templates.find((t) => t.id === templateId);
    if (!template) return;

    const currentId = get(selectedTemplateIdAtom);

    // 1. Save current settings to the old template
    if (currentId && currentId !== templateId) {
      const currentSettings = captureCurrentSettings(get);
      const allSettings = get(templateSettingsAtom);
      set(templateSettingsAtom, {
        ...allSettings,
        [currentId]: currentSettings,
      });
    }

    // 2. Load settings from the new template
    const allSettings = get(templateSettingsAtom);
    const savedSettings = allSettings[templateId];

    if (savedSettings) {
      // Use saved settings
      applySettings(set, savedSettings);
    } else if (template.defaultProps) {
      // Use default props for new template
      applySettings(set, template.defaultProps as TemplateSettings);
    }

    // 3. Set new template ID
    set(selectedTemplateIdAtom, templateId);
  }
);

// Action: Save current settings to selected template
export const saveCurrentSettingsAtom = atom(
  null,
  (get, set) => {
    const currentId = get(selectedTemplateIdAtom);
    if (!currentId) return;

    const currentSettings = captureCurrentSettings(get);
    const allSettings = get(templateSettingsAtom);
    set(templateSettingsAtom, {
      ...allSettings,
      [currentId]: currentSettings,
    });
  }
);

// Action: Add new template
export const addTemplateAtom = atom(
  null,
  (get, set, newTemplate: Omit<Template, 'id' | 'createdAt' | 'isUserCreated'>) => {
    const templates = get(templatesAtom);
    const id = `user-template-${Date.now()}`;
    const template: Template = {
      ...newTemplate,
      id,
      createdAt: Date.now(),
      isUserCreated: true,
    };
    set(templatesAtom, [...templates, template]);
    set(selectedTemplateIdAtom, id);
    return template;
  }
);

// Action: Remove user-created template
export const removeTemplateAtom = atom(
  null,
  (get, set, templateId: string) => {
    const templates = get(templatesAtom);
    const template = templates.find((t) => t.id === templateId);

    // Only allow removing user-created templates
    if (!template?.isUserCreated) {
      console.warn('[Templates] Cannot remove built-in template');
      return false;
    }

    const newTemplates = templates.filter((t) => t.id !== templateId);
    set(templatesAtom, newTemplates);

    // Select first template if current was removed
    const selectedId = get(selectedTemplateIdAtom);
    if (selectedId === templateId && newTemplates.length > 0) {
      set(selectedTemplateIdAtom, newTemplates[0].id);
    }

    return true;
  }
);
