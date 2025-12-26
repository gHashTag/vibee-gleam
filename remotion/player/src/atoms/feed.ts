// ===============================
// Feed Atoms - Social feed for public templates
// ===============================

import { atom } from 'jotai';
import type { TemplateSettings, Template } from './templates';
import { templatesAtom, selectedTemplateIdAtom } from './templates';
import { assetsAtom } from './assets';
import { tracksAtom } from './tracks';
import type { Asset, Track } from '@/store/types';
import { sidebarTabAtom } from './ui';

// Remix source tracking - when user uses a template from feed
export interface RemixSource {
  templateId: number;
  templateName: string;
  creatorName: string;
  creatorAvatar?: string;
}

// Current remix source (set when using a template from feed)
export const currentRemixSourceAtom = atom<RemixSource | null>(null);

// API Base URL
const API_BASE = import.meta.env.VITE_API_BASE_URL ||
  (window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1'
    ? 'http://localhost:8081'
    : 'https://vibee-mcp.fly.dev');

// ===============================
// Types
// ===============================

export interface FeedTemplate {
  id: number;
  telegramId: number;
  creatorName: string;
  creatorAvatar?: string;
  name: string;
  description?: string;
  thumbnailUrl?: string;
  videoUrl: string;
  templateSettings: TemplateSettings;
  assets: Asset[];
  tracks: Track[];
  likesCount: number;
  viewsCount: number;
  usesCount: number;
  isLiked: boolean;
  isFeatured: boolean;
  createdAt: string;
}

export interface PublishData {
  name: string;
  description?: string;
  thumbnailUrl?: string;
  videoUrl: string;
  templateSettings: TemplateSettings;
  assets: Asset[];
  tracks: Track[];
}

export type FeedSort = 'recent' | 'popular';

// ===============================
// Atoms
// ===============================

// Feed templates list
export const feedTemplatesAtom = atom<FeedTemplate[]>([]);

// Loading state
export const feedLoadingAtom = atom(false);

// Error state
export const feedErrorAtom = atom<string | null>(null);

// Current page for pagination
export const feedPageAtom = atom(0);

// Has more pages
export const feedHasMoreAtom = atom(true);

// Sort order
export const feedSortAtom = atom<FeedSort>('recent');

// ===============================
// API Functions
// ===============================

async function fetchFeed(page: number, limit: number, sort: FeedSort): Promise<FeedTemplate[]> {
  const response = await fetch(`${API_BASE}/api/feed?page=${page}&limit=${limit}&sort=${sort}`);
  if (!response.ok) {
    throw new Error(`Failed to fetch feed: ${response.statusText}`);
  }
  const data = await response.json();
  return data.templates || [];
}

async function likeTemplate(id: number, userId: number): Promise<{ liked: boolean; likesCount: number }> {
  const response = await fetch(`${API_BASE}/api/feed/${id}/like`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ user_id: userId }),
  });
  if (!response.ok) {
    throw new Error(`Failed to like template: ${response.statusText}`);
  }
  return response.json();
}

async function publishTemplate(data: PublishData, telegramId: number): Promise<FeedTemplate> {
  const response = await fetch(`${API_BASE}/api/feed/publish`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      telegram_id: telegramId,
      name: data.name,
      description: data.description,
      thumbnail_url: data.thumbnailUrl,
      video_url: data.videoUrl,
      template_settings: data.templateSettings,
      assets: data.assets,
      tracks: data.tracks,
    }),
  });
  if (!response.ok) {
    throw new Error(`Failed to publish template: ${response.statusText}`);
  }
  return response.json();
}

async function useTemplate(id: number): Promise<FeedTemplate> {
  const response = await fetch(`${API_BASE}/api/feed/${id}/use`, {
    method: 'POST',
  });
  if (!response.ok) {
    throw new Error(`Failed to use template: ${response.statusText}`);
  }
  return response.json();
}

// ===============================
// Action Atoms
// ===============================

// Load feed (initial or refresh)
export const loadFeedAtom = atom(
  null,
  async (get, set, refresh?: boolean) => {
    const sort = get(feedSortAtom);
    const currentTemplates = get(feedTemplatesAtom);

    // If refresh, start from page 0
    const page = refresh ? 0 : get(feedPageAtom);
    const limit = 20;

    set(feedLoadingAtom, true);
    set(feedErrorAtom, null);

    try {
      const templates = await fetchFeed(page, limit, sort);

      if (refresh || page === 0) {
        set(feedTemplatesAtom, templates);
      } else {
        set(feedTemplatesAtom, [...currentTemplates, ...templates]);
      }

      set(feedPageAtom, page + 1);
      set(feedHasMoreAtom, templates.length === limit);
    } catch (error) {
      set(feedErrorAtom, error instanceof Error ? error.message : 'Failed to load feed');
    } finally {
      set(feedLoadingAtom, false);
    }
  }
);

// Load more (next page)
export const loadMoreFeedAtom = atom(
  null,
  async (get, set) => {
    const hasMore = get(feedHasMoreAtom);
    const isLoading = get(feedLoadingAtom);

    if (!hasMore || isLoading) return;

    set(loadFeedAtom);
  }
);

// Change sort order
export const changeFeedSortAtom = atom(
  null,
  async (get, set, sort: FeedSort) => {
    set(feedSortAtom, sort);
    set(feedPageAtom, 0);
    set(loadFeedAtom, true); // refresh
  }
);

// Like/unlike template
export const likeTemplateAtom = atom(
  null,
  async (get, set, templateId: number) => {
    // TODO: Get actual user ID from Telegram WebApp
    const userId = 144022504; // Default for testing

    try {
      const result = await likeTemplate(templateId, userId);

      // Update template in list
      const templates = get(feedTemplatesAtom);
      set(feedTemplatesAtom, templates.map(t =>
        t.id === templateId
          ? { ...t, isLiked: result.liked, likesCount: result.likesCount }
          : t
      ));
    } catch (error) {
      console.error('[Feed] Failed to like:', error);
    }
  }
);

// Use template - load into editor with remix tracking
export const useTemplateAtom = atom(
  null,
  async (get, set, templateId: number) => {
    try {
      const template = await useTemplate(templateId);

      // Add as a user template
      const templates = get(templatesAtom);
      const newId = `feed-template-${template.id}-${Date.now()}`;

      // Create new template (always create fresh to allow multiple remixes)
      const newTemplate: Template = {
        id: newId,
        name: `Remix: ${template.name}`,
        description: template.description || '',
        thumbnail: template.thumbnailUrl,
        compositionId: 'SplitTalkingHead',
        defaultProps: template.templateSettings as Record<string, unknown>,
        assets: template.assets,
        tracks: template.tracks,
        createdAt: Date.now(),
        isUserCreated: true,
      };
      set(templatesAtom, [...templates, newTemplate]);

      // Load assets and tracks from template
      if (template.assets?.length) {
        set(assetsAtom, template.assets);
      }
      if (template.tracks?.length) {
        set(tracksAtom, template.tracks);
      }

      // Select the template
      set(selectedTemplateIdAtom, newId);

      // Set remix source for attribution
      set(currentRemixSourceAtom, {
        templateId: template.id,
        templateName: template.name,
        creatorName: template.creatorName || 'Anonymous',
        creatorAvatar: template.creatorAvatar,
      });

      // Switch to lipsync tab so user can replace their cameo/voice
      set(sidebarTabAtom, 'lipsync');

      console.log('[Feed] Template loaded for remix:', template.name);
      console.log('[Feed] Switch to lipsync tab to record your cameo!');
    } catch (error) {
      console.error('[Feed] Failed to use template:', error);
      set(feedErrorAtom, error instanceof Error ? error.message : 'Failed to use template');
    }
  }
);

// Publish current template to feed
export const publishToFeedAtom = atom(
  null,
  async (get, set, data: Omit<PublishData, 'templateSettings' | 'assets' | 'tracks'> & { templateSettings?: TemplateSettings }) => {
    // TODO: Get actual user ID from Telegram WebApp
    const telegramId = 144022504;

    try {
      const fullData: PublishData = {
        ...data,
        templateSettings: data.templateSettings || {},
        assets: get(assetsAtom),
        tracks: get(tracksAtom),
      };

      const template = await publishTemplate(fullData, telegramId);

      // Add to feed list
      const templates = get(feedTemplatesAtom);
      set(feedTemplatesAtom, [template, ...templates]);

      console.log('[Feed] Published:', template.name);
      return template;
    } catch (error) {
      console.error('[Feed] Failed to publish:', error);
      set(feedErrorAtom, error instanceof Error ? error.message : 'Failed to publish');
      throw error;
    }
  }
);
