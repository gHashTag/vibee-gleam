/**
 * S3 Asset Configuration
 *
 * Configuration for asset storage and management in S3.
 * Supports both test (placeholder) and production assets.
 *
 * Bucket Structure:
 * s3://vibee-assets/
 * ├── test/                      # Test assets (reused for all test renders)
 * │   ├── lipsync/
 * │   │   └── test-avatar.mp4    # 30sec test avatar
 * │   ├── broll/
 * │   │   └── test-broll-01..04.mp4
 * │   ├── music/
 * │   │   └── test-music.mp3
 * │   └── manifest.json
 * │
 * ├── production/                # Production assets
 * │   ├── lipsync/{user_id}/
 * │   ├── broll/{category}/
 * │   └── music/{mood}/
 * │
 * └── renders/                   # Rendered videos
 *     └── batch-{id}/
 *         ├── manifest.json
 *         ├── videos/
 *         └── thumbnails/
 */

// ============================================================
// Configuration
// ============================================================

export interface S3Config {
  /** S3 bucket name */
  bucket: string;
  /** AWS region */
  region: string;
  /** Base URL for public access */
  baseUrl: string;
  /** CloudFront distribution (optional) */
  cloudFrontUrl?: string;
}

export const DEFAULT_S3_CONFIG: S3Config = {
  bucket: process.env.VIBEE_S3_BUCKET || 'vibee-assets',
  region: process.env.AWS_REGION || 'us-east-1',
  baseUrl: process.env.VIBEE_S3_BASE_URL || 'https://vibee-assets.s3.amazonaws.com',
  cloudFrontUrl: process.env.VIBEE_CLOUDFRONT_URL,
};

// ============================================================
// Asset Categories
// ============================================================

export type AssetCategory =
  | 'lipsync'
  | 'broll'
  | 'music'
  | 'cover'
  | 'overlay';

export type AssetEnvironment = 'test' | 'production';

export interface AssetMetadata {
  id: string;
  category: AssetCategory;
  environment: AssetEnvironment;
  filename: string;
  mimeType: string;
  size: number;
  duration?: number;
  width?: number;
  height?: number;
  tags: string[];
  uploadedAt: string;
  url: string;
}

// ============================================================
// Test Assets
// ============================================================

/**
 * Test assets for development and testing
 * These are placeholder assets used when testMode=true
 */
export const TEST_ASSETS = {
  lipsync: {
    default: '/test/lipsync/test-avatar.mp4',
    duration: 30,
    description: '30-second test avatar for all variation testing',
  },
  broll: [
    { url: '/test/broll/test-broll-01.mp4', tags: ['abstract', 'tech'] },
    { url: '/test/broll/test-broll-02.mp4', tags: ['nature', 'calm'] },
    { url: '/test/broll/test-broll-03.mp4', tags: ['business', 'office'] },
    { url: '/test/broll/test-broll-04.mp4', tags: ['lifestyle', 'modern'] },
  ],
  music: [
    { url: '/test/music/test-music.mp3', mood: 'ambient', duration: 60 },
  ],
  covers: [
    { url: '/test/covers/test-cover.jpg', style: 'default' },
  ],
};

// ============================================================
// Asset Path Helpers
// ============================================================

/**
 * Get S3 key for an asset
 */
export function getAssetKey(
  category: AssetCategory,
  filename: string,
  environment: AssetEnvironment = 'production',
  subPath?: string
): string {
  const parts = [environment, category];
  if (subPath) {
    parts.push(subPath);
  }
  parts.push(filename);
  return parts.join('/');
}

/**
 * Get full URL for an asset
 */
export function getAssetUrl(
  key: string,
  config: S3Config = DEFAULT_S3_CONFIG
): string {
  if (config.cloudFrontUrl) {
    return `${config.cloudFrontUrl}/${key}`;
  }
  return `${config.baseUrl}/${key}`;
}

/**
 * Get render output path
 */
export function getRenderOutputKey(
  batchId: string,
  variantId: string,
  type: 'video' | 'thumbnail' = 'video'
): string {
  const folder = type === 'video' ? 'videos' : 'thumbnails';
  const extension = type === 'video' ? 'mp4' : 'jpg';
  return `renders/${batchId}/${folder}/${variantId}.${extension}`;
}

// ============================================================
// Asset Manifest
// ============================================================

export interface AssetManifest {
  version: string;
  updatedAt: string;
  environment: AssetEnvironment;
  assets: {
    lipsync: AssetMetadata[];
    broll: AssetMetadata[];
    music: AssetMetadata[];
    covers: AssetMetadata[];
  };
}

/**
 * Create empty manifest
 */
export function createEmptyManifest(
  environment: AssetEnvironment
): AssetManifest {
  return {
    version: '1.0.0',
    updatedAt: new Date().toISOString(),
    environment,
    assets: {
      lipsync: [],
      broll: [],
      music: [],
      covers: [],
    },
  };
}

/**
 * Create test manifest with placeholder assets
 */
export function createTestManifest(): AssetManifest {
  const now = new Date().toISOString();

  return {
    version: '1.0.0',
    updatedAt: now,
    environment: 'test',
    assets: {
      lipsync: [
        {
          id: 'test-avatar',
          category: 'lipsync',
          environment: 'test',
          filename: 'test-avatar.mp4',
          mimeType: 'video/mp4',
          size: 10_000_000, // ~10MB
          duration: 30,
          width: 1080,
          height: 1920,
          tags: ['test', 'placeholder'],
          uploadedAt: now,
          url: TEST_ASSETS.lipsync.default,
        },
      ],
      broll: TEST_ASSETS.broll.map((b, i) => ({
        id: `test-broll-${String(i + 1).padStart(2, '0')}`,
        category: 'broll' as const,
        environment: 'test' as const,
        filename: `test-broll-${String(i + 1).padStart(2, '0')}.mp4`,
        mimeType: 'video/mp4',
        size: 5_000_000, // ~5MB
        duration: 10,
        width: 1920,
        height: 1080,
        tags: b.tags,
        uploadedAt: now,
        url: b.url,
      })),
      music: TEST_ASSETS.music.map((m, i) => ({
        id: `test-music-${i + 1}`,
        category: 'music' as const,
        environment: 'test' as const,
        filename: 'test-music.mp3',
        mimeType: 'audio/mpeg',
        size: 2_000_000, // ~2MB
        duration: m.duration,
        tags: [m.mood, 'test'],
        uploadedAt: now,
        url: m.url,
      })),
      covers: TEST_ASSETS.covers.map((c, i) => ({
        id: `test-cover-${i + 1}`,
        category: 'cover' as const,
        environment: 'test' as const,
        filename: 'test-cover.jpg',
        mimeType: 'image/jpeg',
        size: 500_000, // ~500KB
        width: 1080,
        height: 1920,
        tags: [c.style, 'test'],
        uploadedAt: now,
        url: c.url,
      })),
    },
  };
}

// ============================================================
// Asset Resolution
// ============================================================

export interface AssetResolverOptions {
  environment: AssetEnvironment;
  manifest?: AssetManifest;
  config?: S3Config;
}

/**
 * Resolve placeholder patterns to actual asset URLs
 */
export function resolveAssetPlaceholders(
  props: Record<string, unknown>,
  options: AssetResolverOptions
): Record<string, unknown> {
  const { environment, manifest, config = DEFAULT_S3_CONFIG } = options;
  const resolved = { ...props };

  // Use test manifest if no manifest provided and in test mode
  const assetManifest =
    manifest ||
    (environment === 'test' ? createTestManifest() : createEmptyManifest(environment));

  // Resolve lipSyncVideo
  if (typeof resolved.lipSyncVideo === 'string') {
    resolved.lipSyncVideo = resolveSingleAsset(
      resolved.lipSyncVideo,
      'lipsync',
      assetManifest,
      config
    );
  }

  // Resolve backgroundVideos
  if (Array.isArray(resolved.backgroundVideos)) {
    resolved.backgroundVideos = resolved.backgroundVideos.map((url) =>
      typeof url === 'string'
        ? resolveSingleAsset(url, 'broll', assetManifest, config)
        : url
    );
  }

  // Resolve backgroundMusic
  if (typeof resolved.backgroundMusic === 'string') {
    resolved.backgroundMusic = resolveSingleAsset(
      resolved.backgroundMusic,
      'music',
      assetManifest,
      config
    );
  }

  return resolved;
}

function resolveSingleAsset(
  url: string,
  category: AssetCategory,
  manifest: AssetManifest,
  config: S3Config
): string {
  // Not a placeholder
  if (!url.includes('{{')) {
    return url;
  }

  // Get assets for category
  const assets = manifest.assets[category as keyof typeof manifest.assets] || [];

  if (assets.length === 0) {
    console.warn(`No ${category} assets available in manifest`);
    return url;
  }

  // Parse placeholder pattern: {{assets.category.index}} or {{assets.category.tag}}
  const match = url.match(/\{\{assets\.(\w+)(?:\.(\w+))?\}\}/);
  if (!match) {
    return assets[0]?.url || url;
  }

  const [, , selector] = match;

  // If selector is a number, use as index
  if (selector && /^\d+$/.test(selector)) {
    const index = parseInt(selector, 10);
    return assets[index % assets.length]?.url || assets[0]?.url || url;
  }

  // If selector is a tag, find matching asset
  if (selector) {
    const matching = assets.find((a) =>
      a.tags.some((t) => t.toLowerCase() === selector.toLowerCase())
    );
    if (matching) {
      return matching.url;
    }
  }

  // Default to first asset
  return assets[0]?.url || url;
}

// ============================================================
// Exports
// ============================================================

export default {
  config: DEFAULT_S3_CONFIG,
  testAssets: TEST_ASSETS,
  getAssetKey,
  getAssetUrl,
  getRenderOutputKey,
  createTestManifest,
  resolveAssetPlaceholders,
};
