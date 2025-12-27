import { useCallback, useState, useRef, useEffect, useMemo } from 'react';
import { useAtomValue, useSetAtom } from 'jotai';
import { assetsAtom, addAssetAtom, removeAssetAtom, addItemAtom } from '@/atoms';
import { templatesAtom, selectedTemplateIdAtom, selectTemplateAtom, removeTemplateAtom } from '@/atoms/templates';
import { currentFrameAtom } from '@/atoms/playback';
import { useLanguage } from '@/hooks/useLanguage';
import { useLongPress } from '@/hooks/useLongPress';
import { Music, Trash2, Upload, Loader2, Play, Maximize, Minimize2, Film, Plus, LayoutTemplate, Check, ChevronDown, ChevronUp } from 'lucide-react';
import type { Asset, AssetType } from '@/store/types';
import { broadcastAssetAdded, broadcastAssetRemoved } from '@/lib/websocket';
import { toAbsoluteUrl } from '@/lib/mediaUrl';
import { QuickActionsMenu, useAssetQuickActions } from '@/components/common/QuickActionsMenu';
import { SearchBar, type AssetFilter } from '@/components/common/SearchBar';
import { RecentFavorites, useRecentFavorites, type RecentAsset } from '@/components/Assets/RecentFavorites';
import { SkeletonAssetGrid } from '@/components/common/Skeleton';
import './AssetsPanel.css';

// Render server URL (for S3 uploads)
const RENDER_SERVER_URL = import.meta.env.VITE_RENDER_SERVER_URL || 'http://localhost:3333';

interface UploadProgress {
  fileName: string;
  progress: number;
  status: 'uploading' | 'done' | 'error';
  error?: string;
}

// Generate thumbnail from video (preserves aspect ratio)
function generateVideoThumbnail(videoUrl: string): Promise<string> {
  return new Promise((resolve) => {
    const video = document.createElement('video');
    video.crossOrigin = 'anonymous';
    video.muted = true;
    video.preload = 'metadata';

    video.onloadeddata = () => {
      video.currentTime = 0.1; // Seek to 0.1s for thumbnail
    };

    video.onseeked = () => {
      const canvas = document.createElement('canvas');
      // Preserve original aspect ratio, max 300px on longest side for sharp thumbnails
      const maxSize = 300;
      const vw = video.videoWidth;
      const vh = video.videoHeight;

      if (vw > vh) {
        // Landscape
        canvas.width = maxSize;
        canvas.height = Math.round((vh / vw) * maxSize);
      } else {
        // Portrait or square
        canvas.height = maxSize;
        canvas.width = Math.round((vw / vh) * maxSize);
      }

      const ctx = canvas.getContext('2d');
      if (ctx) {
        ctx.drawImage(video, 0, 0, canvas.width, canvas.height);
        resolve(canvas.toDataURL('image/jpeg', 0.85));
      } else {
        resolve('');
      }
    };

    video.onerror = () => resolve('');
    video.src = videoUrl;
  });
}

// Get video duration
function getVideoDuration(videoUrl: string): Promise<number> {
  return new Promise((resolve) => {
    const video = document.createElement('video');
    video.preload = 'metadata';
    video.onloadedmetadata = () => resolve(video.duration);
    video.onerror = () => resolve(0);
    video.src = videoUrl;
  });
}

// Format duration as mm:ss
function formatDuration(seconds: number): string {
  const mins = Math.floor(seconds / 60);
  const secs = Math.floor(seconds % 60);
  return `${mins}:${secs.toString().padStart(2, '0')}`;
}

// Format file size
function formatFileSize(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
  return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
}

export function AssetsPanel() {
  const { t } = useLanguage();
  const assets = useAtomValue(assetsAtom);
  const addAsset = useSetAtom(addAssetAtom);
  const removeAsset = useSetAtom(removeAssetAtom);
  const addItem = useSetAtom(addItemAtom);
  const currentFrame = useAtomValue(currentFrameAtom);
  const [uploads, setUploads] = useState<UploadProgress[]>([]);
  const [thumbnails, setThumbnails] = useState<Record<string, string>>({});
  const [durations, setDurations] = useState<Record<string, number>>({});
  const [viewMode, setViewMode] = useState<'fill' | 'fit'>('fill');
  const [searchQuery, setSearchQuery] = useState('');
  const [assetFilter, setAssetFilter] = useState<AssetFilter>('all');
  const [thumbnailsLoading, setThumbnailsLoading] = useState<Set<string>>(new Set());
  const [templatesExpanded, setTemplatesExpanded] = useState(true);

  // Templates
  const templates = useAtomValue(templatesAtom);
  const selectedTemplateId = useAtomValue(selectedTemplateIdAtom);
  const selectTemplate = useSetAtom(selectTemplateAtom);
  const removeTemplate = useSetAtom(removeTemplateAtom);

  // Recent/Favorites hook
  const {
    recentAssets,
    favoriteAssets,
    addRecent,
    removeRecent,
    clearRecent,
    toggleFavorite,
  } = useRecentFavorites();

  // Generate thumbnails for video assets
  useEffect(() => {
    assets.forEach(async (asset) => {
      if (asset.type === 'video' && !thumbnails[asset.id] && !thumbnailsLoading.has(asset.id)) {
        // Mark as loading
        setThumbnailsLoading(prev => new Set(prev).add(asset.id));

        const thumb = await generateVideoThumbnail(asset.url);
        if (thumb) {
          setThumbnails(prev => ({ ...prev, [asset.id]: thumb }));
        }
        const duration = await getVideoDuration(asset.url);
        if (duration) {
          setDurations(prev => ({ ...prev, [asset.id]: duration }));
        }

        // Mark as done loading
        setThumbnailsLoading(prev => {
          const next = new Set(prev);
          next.delete(asset.id);
          return next;
        });
      }
    });
  }, [assets]);

  const uploadToS3 = async (file: File): Promise<string | null> => {
    try {
      const response = await fetch(`${RENDER_SERVER_URL}/upload`, {
        method: 'POST',
        headers: {
          'Content-Type': file.type,
          'X-Filename': file.name,
        },
        body: file,
      });

      const result = await response.json();
      if (result.success && result.url) {
        return result.url;
      }
      throw new Error(result.error || 'Upload failed');
    } catch (error) {
      console.error('S3 upload error:', error);
      return null;
    }
  };

  const handleFileUpload = useCallback(
    async (files: FileList | null) => {
      if (!files) return;

      for (const file of Array.from(files)) {
        let type: AssetType;

        if (file.type.startsWith('video/')) {
          type = 'video';
        } else if (file.type.startsWith('image/')) {
          type = 'image';
        } else if (file.type.startsWith('audio/')) {
          type = 'audio';
        } else {
          continue;
        }

        setUploads((prev) => [
          ...prev,
          { fileName: file.name, progress: 0, status: 'uploading' },
        ]);

        let url = await uploadToS3(file);

        if (!url) {
          url = URL.createObjectURL(file);
          console.warn(`S3 upload failed for ${file.name}, using local blob`);
        }

        setUploads((prev) =>
          prev.map((u) =>
            u.fileName === file.name
              ? { ...u, progress: 100, status: 'done' }
              : u
          )
        );

        const newAsset: Asset = {
          id: `asset-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`,
          type,
          name: file.name,
          url,
          fileSize: file.size,
        };

        addAsset(newAsset);
        broadcastAssetAdded(newAsset);

        setTimeout(() => {
          setUploads((prev) => prev.filter((u) => u.fileName !== file.name));
        }, 2000);
      }
    },
    []
  );

  const handleRemoveAsset = useCallback((id: string) => {
    const asset = assets.find((a) => a.id === id);
    if (asset?.url.startsWith('blob:')) {
      URL.revokeObjectURL(asset.url);
    }
    removeAsset(id);
    broadcastAssetRemoved(id);
    // Clean up thumbnail
    setThumbnails(prev => {
      const { [id]: _, ...rest } = prev;
      return rest;
    });
  }, [assets, removeAsset]);

  const handleDrop = useCallback(
    (e: React.DragEvent) => {
      e.preventDefault();
      handleFileUpload(e.dataTransfer.files);
    },
    [handleFileUpload]
  );

  const handleDragStart = (e: React.DragEvent, asset: Asset) => {
    e.dataTransfer.setData('application/json', JSON.stringify(asset));
    e.dataTransfer.effectAllowed = 'copy';
  };

  const handleAddToTimeline = (asset: Asset) => {
    // Determine target track based on asset type
    let trackId: string;
    switch (asset.type) {
      case 'audio':
        trackId = 'track-audio';
        break;
      case 'image':
        trackId = 'track-image';
        break;
      default:
        trackId = 'track-video';
    }

    addItem({
      trackId,
      itemData: {
        type: asset.type as any,
        assetId: asset.id,
        startFrame: currentFrame, // Add at playhead position
        durationInFrames: asset.duration || 150,
        x: 0,
        y: 0,
        width: 1080,
        height: asset.type === 'video' ? 1920 : 1080,
        rotation: 0,
        opacity: 1,
        ...(asset.type === 'video' && { volume: 1, playbackRate: 1 }),
        ...(asset.type === 'audio' && { volume: 1 }),
      },
    });

    // Add to recent
    addRecent({
      id: asset.id,
      type: asset.type as 'video' | 'image' | 'audio',
      name: asset.name,
      thumbnail: thumbnails[asset.id],
      url: asset.url,
    });

    // Haptic feedback
    if ('vibrate' in navigator) {
      navigator.vibrate(50);
    }
  };

  // Add asset to the beginning of the timeline (frame 0)
  const handleAddToStart = (asset: Asset) => {
    let trackId: string;
    switch (asset.type) {
      case 'audio':
        trackId = 'track-audio';
        break;
      case 'image':
        trackId = 'track-image';
        break;
      default:
        trackId = 'track-video';
    }

    addItem({
      trackId,
      itemData: {
        type: asset.type as any,
        assetId: asset.id,
        startFrame: 0, // Add at the beginning
        durationInFrames: asset.duration || 150,
        x: 0,
        y: 0,
        width: 1080,
        height: asset.type === 'video' ? 1920 : 1080,
        rotation: 0,
        opacity: 1,
        ...(asset.type === 'video' && { volume: 1, playbackRate: 1 }),
        ...(asset.type === 'audio' && { volume: 1 }),
      },
    });

    if ('vibrate' in navigator) {
      navigator.vibrate(50);
    }
  };

  // Filter assets by search query and type
  const filteredAssets = useMemo(() => {
    return assets.filter((asset) => {
      // Filter by type
      if (assetFilter !== 'all' && asset.type !== assetFilter) {
        return false;
      }
      // Filter by search query
      if (searchQuery) {
        return asset.name.toLowerCase().includes(searchQuery.toLowerCase());
      }
      return true;
    });
  }, [assets, assetFilter, searchQuery]);

  const videoAssets = filteredAssets.filter((a) => a.type === 'video');
  const imageAssets = filteredAssets.filter((a) => a.type === 'image');
  const audioAssets = filteredAssets.filter((a) => a.type === 'audio');

  // Count videos still loading thumbnails
  const videosLoadingCount = useMemo(() => {
    return videoAssets.filter(a => thumbnailsLoading.has(a.id)).length;
  }, [videoAssets, thumbnailsLoading]);

  // Convert asset to RecentAsset format for favorites
  const assetToRecent = (asset: Asset): RecentAsset => ({
    id: asset.id,
    type: asset.type as 'video' | 'image' | 'audio',
    name: asset.name,
    thumbnail: thumbnails[asset.id],
    url: asset.url,
    usedAt: Date.now(),
  });

  // Handle selecting asset from recent/favorites
  const handleSelectRecentAsset = (asset: RecentAsset | any) => {
    const fullAsset = assets.find(a => a.id === asset.id);
    if (fullAsset) {
      handleAddToTimeline(fullAsset);
    }
  };

  // Handle template delete
  const handleDeleteTemplate = (e: React.MouseEvent, templateId: string) => {
    e.stopPropagation();
    if (confirm(t('templates.confirmDelete'))) {
      removeTemplate(templateId);
    }
  };

  return (
    <div className="assets-panel">
      <div className="panel-header">
        <Film size={14} />
        <span>{t('tabs.assets')}</span>
      </div>

      {/* Upload Zone */}
      <div
        className="upload-zone"
        onDrop={handleDrop}
        onDragOver={(e) => e.preventDefault()}
      >
        <input
          type="file"
          id="file-upload"
          multiple
          accept="video/*,image/*,audio/*"
          onChange={(e) => handleFileUpload(e.target.files)}
          style={{ display: 'none' }}
        />
        <label htmlFor="file-upload" className="upload-label">
          <Upload size={20} />
          <span>{t('assets.dropOrClick')}</span>
          <span className="upload-hint">{t('assets.uploadsToCloud')}</span>
        </label>
      </div>

      {/* Search Bar */}
      <div className="assets-search">
        <SearchBar
          value={searchQuery}
          onChange={setSearchQuery}
          filter={assetFilter}
          onFilterChange={setAssetFilter}
          placeholder={t('search.placeholder')}
        />
      </div>

      {/* Recent & Favorites */}
      <RecentFavorites
        recentAssets={recentAssets}
        favoriteAssets={favoriteAssets}
        onSelectAsset={handleSelectRecentAsset}
        onRemoveRecent={removeRecent}
        onToggleFavorite={toggleFavorite}
        onClearRecent={clearRecent}
      />

      {/* Templates Section */}
      {templates.length > 0 && (
        <div className="templates-section">
          <button
            className="templates-header"
            onClick={() => setTemplatesExpanded(!templatesExpanded)}
          >
            <LayoutTemplate size={14} />
            <span>{t('templates.title')}</span>
            <span className="templates-count">{templates.length}</span>
            {templatesExpanded ? <ChevronUp size={14} /> : <ChevronDown size={14} />}
          </button>
          {templatesExpanded && (
            <div className="templates-list">
              {templates.map((template) => {
                const isSelected = template.id === selectedTemplateId;
                return (
                  <div
                    key={template.id}
                    className={`template-item ${isSelected ? 'selected' : ''}`}
                    onClick={() => selectTemplate(template.id)}
                    title={template.description}
                  >
                    <div className="template-icon">
                      <LayoutTemplate size={14} />
                    </div>
                    <span className="template-name">{template.name}</span>
                    {isSelected && (
                      <div className="template-check">
                        <Check size={12} />
                      </div>
                    )}
                    {template.isUserCreated && (
                      <button
                        className="template-delete"
                        onClick={(e) => handleDeleteTemplate(e, template.id)}
                        title={t('templates.delete')}
                      >
                        <Trash2 size={10} />
                      </button>
                    )}
                  </div>
                );
              })}
            </div>
          )}
        </div>
      )}

      {/* View Mode Toggle */}
      <div className="view-mode-toggle">
        <button
          className={`view-mode-btn ${viewMode === 'fill' ? 'active' : ''}`}
          onClick={() => setViewMode('fill')}
          title="Fill (crop to fit)"
        >
          <Maximize size={14} />
        </button>
        <button
          className={`view-mode-btn ${viewMode === 'fit' ? 'active' : ''}`}
          onClick={() => setViewMode('fit')}
          title="Fit (show full)"
        >
          <Minimize2 size={14} />
        </button>
      </div>

      {/* Upload Progress */}
      {uploads.length > 0 && (
        <div className="upload-progress-list">
          {uploads.map((upload) => (
            <div key={upload.fileName} className={`upload-progress-item ${upload.status}`}>
              <Loader2 size={14} className={upload.status === 'uploading' ? 'spin' : ''} />
              <span className="upload-filename">{upload.fileName}</span>
              <span className="upload-status">
                {upload.status === 'uploading' ? t('assets.uploading') :
                 upload.status === 'done' ? t('assets.done') : t('assets.error')}
              </span>
            </div>
          ))}
        </div>
      )}

      {/* Videos - Grid View */}
      {videoAssets.length > 0 && (
        <div className="asset-group">
          <h3 className="asset-group-title">
            {t('assets.videos')}
            <span className="asset-count">{videoAssets.length}</span>
          </h3>
          <div className={`asset-grid ${viewMode}-mode`}>
            {videoAssets.map((asset) => (
              <VideoAssetCard
                key={asset.id}
                asset={asset}
                thumbnail={thumbnails[asset.id]}
                duration={durations[asset.id]}
                viewMode={viewMode}
                isLoading={thumbnailsLoading.has(asset.id)}
                onDragStart={handleDragStart}
                onAddToTimeline={handleAddToTimeline}
                onAddToStart={handleAddToStart}
                onRemove={handleRemoveAsset}
                t={t}
              />
            ))}
          </div>
        </div>
      )}

      {/* Skeleton placeholders for initial load */}
      {uploads.length > 0 && videoAssets.length === 0 && imageAssets.length === 0 && (
        <SkeletonAssetGrid count={uploads.length} />
      )}

      {/* Images - Grid View */}
      {imageAssets.length > 0 && (
        <div className="asset-group">
          <h3 className="asset-group-title">
            {t('assets.images')}
            <span className="asset-count">{imageAssets.length}</span>
          </h3>
          <div className={`asset-grid ${viewMode}-mode`}>
            {imageAssets.map((asset) => (
              <ImageAssetCard
                key={asset.id}
                asset={asset}
                viewMode={viewMode}
                onDragStart={handleDragStart}
                onAddToTimeline={handleAddToTimeline}
                onAddToStart={handleAddToStart}
                onRemove={handleRemoveAsset}
                t={t}
              />
            ))}
          </div>
        </div>
      )}

      {/* Audio - List View */}
      {audioAssets.length > 0 && (
        <div className="asset-group">
          <h3 className="asset-group-title">
            {t('assets.audio')}
            <span className="asset-count">{audioAssets.length}</span>
          </h3>
          <div className="asset-list">
            {audioAssets.map((asset) => (
              <AudioAssetItem
                key={asset.id}
                asset={asset}
                onDragStart={handleDragStart}
                onAddToTimeline={handleAddToTimeline}
                onRemove={handleRemoveAsset}
                t={t}
              />
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

// Video Card with thumbnail and hover preview
interface VideoAssetCardProps {
  asset: Asset;
  thumbnail?: string;
  duration?: number;
  viewMode: 'fill' | 'fit';
  isLoading?: boolean;
  onDragStart: (e: React.DragEvent, asset: Asset) => void;
  onAddToTimeline: (asset: Asset) => void;
  onAddToStart: (asset: Asset) => void;
  onRemove: (id: string) => void;
  t: (key: string) => string;
}

function VideoAssetCard({ asset, thumbnail, duration, viewMode, isLoading, onDragStart, onAddToTimeline, onAddToStart, onRemove, t }: VideoAssetCardProps) {
  const [isHovering, setIsHovering] = useState(false);
  const [menuOpen, setMenuOpen] = useState(false);
  const [menuPosition, setMenuPosition] = useState({ x: 0, y: 0 });
  const videoRef = useRef<HTMLVideoElement>(null);
  const isBlobUrl = asset.url.startsWith('blob:');
  const videoUrl = toAbsoluteUrl(asset.url);

  // Quick actions for long-press menu
  const quickActions = useAssetQuickActions(asset, {
    onAddToTimeline: () => onAddToTimeline(asset),
    onAddToStart: () => onAddToStart(asset),
    onPreview: () => {
      // Open video in a popup preview
      const win = window.open(videoUrl, '_blank', 'width=400,height=600');
      win?.focus();
    },
    onDelete: () => onRemove(asset.id),
  });

  // Long press handler
  const longPressHandlers = useLongPress({
    threshold: 400,
    onLongPress: (e) => {
      // Get position for menu
      let x = 0, y = 0;
      if ('touches' in e && e.touches.length > 0) {
        x = e.touches[0].clientX;
        y = e.touches[0].clientY;
      } else if ('clientX' in e) {
        x = e.clientX;
        y = e.clientY;
      }
      setMenuPosition({ x, y });
      setMenuOpen(true);
    },
    onClick: () => {
      // Single tap on mobile = add to timeline
      onAddToTimeline(asset);
    },
  });

  useEffect(() => {
    if (videoRef.current) {
      if (isHovering) {
        videoRef.current.play().catch(() => {});
      } else {
        videoRef.current.pause();
        videoRef.current.currentTime = 0;
      }
    }
  }, [isHovering]);

  // Combine handlers
  const handleMouseEnter = () => setIsHovering(true);
  const handleMouseLeave = (e: React.MouseEvent) => {
    setIsHovering(false);
    longPressHandlers.onMouseLeave(e);
  };

  return (
    <>
      <div
        className={`asset-card ${isBlobUrl ? 'blob-warning' : ''} ${longPressHandlers.isPressed ? 'pressing' : ''}`}
        draggable
        onDragStart={(e) => onDragStart(e, asset)}
        onDoubleClick={() => onAddToTimeline(asset)}
        onMouseEnter={handleMouseEnter}
        onMouseLeave={handleMouseLeave}
        onMouseDown={longPressHandlers.onMouseDown}
        onMouseUp={longPressHandlers.onMouseUp}
        onTouchStart={longPressHandlers.onTouchStart}
        onTouchEnd={longPressHandlers.onTouchEnd}
        onTouchMove={longPressHandlers.onTouchMove}
        title={asset.name}
      >
        <div className="asset-card-thumbnail">
          {isHovering ? (
            <video
              ref={videoRef}
              src={videoUrl}
              muted
              loop
              playsInline
              className="asset-card-video"
            />
          ) : thumbnail ? (
            <img src={thumbnail} alt={asset.name} className="asset-card-img" />
          ) : isLoading ? (
            <div className="asset-card-placeholder asset-card-loading">
              <Loader2 size={16} className="spin" />
            </div>
          ) : (
            <div className="asset-card-placeholder">
              <Play size={16} />
            </div>
          )}
          {duration && (
            <span className="asset-card-duration">{formatDuration(duration)}</span>
          )}
          {isBlobUrl && <span className="asset-card-warning">⚠️</span>}
        </div>
        <div className="asset-card-info">
          <span className="asset-card-name">{asset.name}</span>
          {asset.fileSize && (
            <span className="asset-card-size">{formatFileSize(asset.fileSize)}</span>
          )}
        </div>
        <button
          className="asset-card-add"
          onClick={(e) => {
            e.stopPropagation();
            onAddToTimeline(asset);
          }}
          title={t('generate.addToTimeline')}
        >
          <Plus size={12} />
        </button>
        <button
          className="asset-card-remove"
          onClick={(e) => {
            e.stopPropagation();
            onRemove(asset.id);
          }}
        >
          <Trash2 size={10} />
        </button>
      </div>

      <QuickActionsMenu
        isOpen={menuOpen}
        onClose={() => setMenuOpen(false)}
        position={menuPosition}
        actions={quickActions}
        title={asset.name}
      />
    </>
  );
}

// Image Card
interface ImageAssetCardProps {
  asset: Asset;
  viewMode: 'fill' | 'fit';
  onDragStart: (e: React.DragEvent, asset: Asset) => void;
  onAddToTimeline: (asset: Asset) => void;
  onAddToStart: (asset: Asset) => void;
  onRemove: (id: string) => void;
  t: (key: string) => string;
}

function ImageAssetCard({ asset, viewMode, onDragStart, onAddToTimeline, onAddToStart, onRemove, t }: ImageAssetCardProps) {
  const [menuOpen, setMenuOpen] = useState(false);
  const [menuPosition, setMenuPosition] = useState({ x: 0, y: 0 });
  const isBlobUrl = asset.url.startsWith('blob:');
  const imageUrl = toAbsoluteUrl(asset.url);

  // Quick actions for long-press menu
  const quickActions = useAssetQuickActions(asset, {
    onAddToTimeline: () => onAddToTimeline(asset),
    onAddToStart: () => onAddToStart(asset),
    onPreview: () => {
      // Open image in a popup preview
      const win = window.open(imageUrl, '_blank', 'width=600,height=800');
      win?.focus();
    },
    onDelete: () => onRemove(asset.id),
  });

  // Long press handler
  const longPressHandlers = useLongPress({
    threshold: 400,
    onLongPress: (e) => {
      let x = 0, y = 0;
      if ('touches' in e && e.touches.length > 0) {
        x = e.touches[0].clientX;
        y = e.touches[0].clientY;
      } else if ('clientX' in e) {
        x = e.clientX;
        y = e.clientY;
      }
      setMenuPosition({ x, y });
      setMenuOpen(true);
    },
    onClick: () => {
      onAddToTimeline(asset);
    },
  });

  return (
    <>
      <div
        className={`asset-card ${isBlobUrl ? 'blob-warning' : ''} ${longPressHandlers.isPressed ? 'pressing' : ''}`}
        draggable
        onDragStart={(e) => onDragStart(e, asset)}
        onDoubleClick={() => onAddToTimeline(asset)}
        title={asset.name}
        {...longPressHandlers}
      >
        <div className="asset-card-thumbnail">
          <img src={imageUrl} alt={asset.name} className="asset-card-img" />
          {isBlobUrl && <span className="asset-card-warning">⚠️</span>}
        </div>
        <div className="asset-card-info">
          <span className="asset-card-name">{asset.name}</span>
          {asset.fileSize && (
            <span className="asset-card-size">{formatFileSize(asset.fileSize)}</span>
          )}
        </div>
        <button
          className="asset-card-add"
          onClick={(e) => {
            e.stopPropagation();
            onAddToTimeline(asset);
          }}
          title={t('generate.addToTimeline')}
        >
          <Plus size={12} />
        </button>
        <button
          className="asset-card-remove"
          onClick={(e) => {
            e.stopPropagation();
            onRemove(asset.id);
          }}
        >
          <Trash2 size={10} />
        </button>
      </div>

      <QuickActionsMenu
        isOpen={menuOpen}
        onClose={() => setMenuOpen(false)}
        position={menuPosition}
        actions={quickActions}
        title={asset.name}
      />
    </>
  );
}

// Audio Item (list view)
interface AudioAssetItemProps {
  asset: Asset;
  onDragStart: (e: React.DragEvent, asset: Asset) => void;
  onAddToTimeline: (asset: Asset) => void;
  onRemove: (id: string) => void;
  t: (key: string) => string;
}

function AudioAssetItem({ asset, onDragStart, onAddToTimeline, onRemove, t }: AudioAssetItemProps) {
  const isBlobUrl = asset.url.startsWith('blob:');

  return (
    <div
      className={`asset-item ${isBlobUrl ? 'blob-warning' : ''}`}
      draggable
      onDragStart={(e) => onDragStart(e, asset)}
      onDoubleClick={() => onAddToTimeline(asset)}
      title={asset.name}
    >
      <div className="asset-icon">
        <Music size={16} />
      </div>
      <span className="asset-name">{asset.name}</span>
      {asset.fileSize && (
        <span className="asset-size">{formatFileSize(asset.fileSize)}</span>
      )}
      {isBlobUrl && <span className="blob-badge">⚠️</span>}
      <button
        className="asset-add"
        onClick={(e) => {
          e.stopPropagation();
          onAddToTimeline(asset);
        }}
        title={t('generate.addToTimeline')}
      >
        <Plus size={14} />
      </button>
      <button
        className="asset-remove"
        onClick={(e) => {
          e.stopPropagation();
          onRemove(asset.id);
        }}
      >
        <Trash2 size={12} />
      </button>
    </div>
  );
}
