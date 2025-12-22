// ===============================
// Templates Atoms - Video templates management
// ===============================

import { atom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';

export interface Template {
  id: string;
  name: string;
  description: string;
  thumbnail?: string;
  compositionId: string;
  // Default props for the template
  defaultProps: Record<string, unknown>;
}

// Available templates
export const templatesAtom = atom<Template[]>([
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
  },
]);

// Currently selected template ID
export const selectedTemplateIdAtom = atomWithStorage<string | null>(
  'vibee-selected-template',
  'vibee-reel-1'
);

// Derived: Get selected template
export const selectedTemplateAtom = atom((get) => {
  const templates = get(templatesAtom);
  const selectedId = get(selectedTemplateIdAtom);
  return templates.find((t) => t.id === selectedId) || templates[0];
});

// Action: Select template
export const selectTemplateAtom = atom(
  null,
  (get, set, templateId: string) => {
    const templates = get(templatesAtom);
    const template = templates.find((t) => t.id === templateId);
    if (template) {
      set(selectedTemplateIdAtom, templateId);
    }
  }
);
