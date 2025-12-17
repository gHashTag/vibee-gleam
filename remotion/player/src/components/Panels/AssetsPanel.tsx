import { useCallback, useState } from 'react';
import { useEditorStore } from '@/store/editorStore';
import { Film, Image, Music, Plus, Trash2, Upload, Loader2 } from 'lucide-react';
import type { Asset, AssetType } from '@/store/types';
import { broadcastAssetAdded, broadcastAssetRemoved } from '@/lib/websocket';
import './AssetsPanel.css';

// Render server URL (for S3 uploads)
const RENDER_SERVER_URL = import.meta.env.VITE_RENDER_SERVER_URL || 'http://localhost:3333';

const ASSET_ICONS: Record<AssetType, typeof Film> = {
  video: Film,
  image: Image,
  audio: Music,
};

interface UploadProgress {
  fileName: string;
  progress: number;
  status: 'uploading' | 'done' | 'error';
  error?: string;
}

export function AssetsPanel() {
  const assets = useEditorStore((s) => s.assets);
  const addAsset = useEditorStore((s) => s.addAsset);
  const removeAsset = useEditorStore((s) => s.removeAsset);
  const addItem = useEditorStore((s) => s.addItem);
  const [uploads, setUploads] = useState<UploadProgress[]>([]);

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
          continue; // Skip unsupported files
        }

        // Add to upload progress
        setUploads((prev) => [
          ...prev,
          { fileName: file.name, progress: 0, status: 'uploading' },
        ]);

        // Try S3 upload first, fallback to local blob
        let url = await uploadToS3(file);

        if (!url) {
          // Fallback to local blob URL (for local dev without S3)
          url = URL.createObjectURL(file);
          console.warn(`S3 upload failed for ${file.name}, using local blob`);
        }

        // Update upload status
        setUploads((prev) =>
          prev.map((u) =>
            u.fileName === file.name
              ? { ...u, progress: 100, status: 'done' }
              : u
          )
        );

        // Add asset to store and get the generated ID
        const newAsset: Asset = {
          id: `asset-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`,
          type,
          name: file.name,
          url,
          fileSize: file.size,
        };

        // Add to local store
        useEditorStore.setState((state) => ({
          assets: [...state.assets, newAsset],
        }));

        // Broadcast to other clients via WebSocket
        broadcastAssetAdded(newAsset);

        // Remove from upload list after delay
        setTimeout(() => {
          setUploads((prev) => prev.filter((u) => u.fileName !== file.name));
        }, 2000);
      }
    },
    []
  );

  // Wrapper for removeAsset that also broadcasts and cleans up blob URLs
  const handleRemoveAsset = useCallback((id: string) => {
    // Find asset before removing to check for blob URL
    const asset = assets.find((a) => a.id === id);
    if (asset?.url.startsWith('blob:')) {
      // Revoke blob URL to free memory
      URL.revokeObjectURL(asset.url);
      console.log('[Assets] Revoked blob URL:', asset.url);
    }
    removeAsset(id);
    broadcastAssetRemoved(id);
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
    // Determine which track based on asset type
    const trackId =
      asset.type === 'video'
        ? 'track-video'
        : asset.type === 'audio'
        ? 'track-audio'
        : 'track-video';

    addItem(trackId, {
      type: asset.type as any,
      assetId: asset.id,
      startFrame: 0,
      durationInFrames: asset.duration || 150,
      x: 0,
      y: 0,
      width: 1080,
      height: 1920,
      rotation: 0,
      opacity: 1,
      ...(asset.type === 'video' && { volume: 1, playbackRate: 1 }),
      ...(asset.type === 'audio' && { volume: 1 }),
    });
  };

  // Group assets by type
  const videoAssets = assets.filter((a) => a.type === 'video');
  const imageAssets = assets.filter((a) => a.type === 'image');
  const audioAssets = assets.filter((a) => a.type === 'audio');

  return (
    <div className="assets-panel">
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
          <span>Drop files or click to upload</span>
          <span className="upload-hint">Uploads to S3 cloud</span>
        </label>
      </div>

      {/* Upload Progress */}
      {uploads.length > 0 && (
        <div className="upload-progress-list">
          {uploads.map((upload) => (
            <div key={upload.fileName} className={`upload-progress-item ${upload.status}`}>
              <Loader2 size={14} className={upload.status === 'uploading' ? 'spin' : ''} />
              <span className="upload-filename">{upload.fileName}</span>
              <span className="upload-status">
                {upload.status === 'uploading' ? 'Uploading...' :
                 upload.status === 'done' ? 'Done' : 'Error'}
              </span>
            </div>
          ))}
        </div>
      )}

      {/* Videos */}
      {videoAssets.length > 0 && (
        <AssetGroup
          title="Videos"
          count={videoAssets.length}
          assets={videoAssets}
          onDragStart={handleDragStart}
          onAddToTimeline={handleAddToTimeline}
          onRemove={handleRemoveAsset}
        />
      )}

      {/* Images */}
      {imageAssets.length > 0 && (
        <AssetGroup
          title="Images"
          count={imageAssets.length}
          assets={imageAssets}
          onDragStart={handleDragStart}
          onAddToTimeline={handleAddToTimeline}
          onRemove={handleRemoveAsset}
        />
      )}

      {/* Audio */}
      {audioAssets.length > 0 && (
        <AssetGroup
          title="Audio"
          count={audioAssets.length}
          assets={audioAssets}
          onDragStart={handleDragStart}
          onAddToTimeline={handleAddToTimeline}
          onRemove={handleRemoveAsset}
        />
      )}
    </div>
  );
}

interface AssetGroupProps {
  title: string;
  count: number;
  assets: Asset[];
  onDragStart: (e: React.DragEvent, asset: Asset) => void;
  onAddToTimeline: (asset: Asset) => void;
  onRemove: (id: string) => void;
}

function AssetGroup({
  title,
  count,
  assets,
  onDragStart,
  onAddToTimeline,
  onRemove,
}: AssetGroupProps) {
  return (
    <div className="asset-group">
      <h3 className="asset-group-title">
        {title}
        <span className="asset-count">{count}</span>
      </h3>
      <div className="asset-list">
        {assets.map((asset) => {
          const Icon = ASSET_ICONS[asset.type];
          const isBlobUrl = asset.url.startsWith('blob:');
          return (
            <div
              key={asset.id}
              className={`asset-item ${isBlobUrl ? 'blob-warning' : ''}`}
              draggable
              onDragStart={(e) => onDragStart(e, asset)}
              onDoubleClick={() => onAddToTimeline(asset)}
              title={isBlobUrl
                ? `${asset.name}\n⚠️ Local file - will be skipped during export!\nRe-upload to fix.`
                : `${asset.name}\nDouble-click or drag to timeline`}
            >
              <div className="asset-icon">
                <Icon size={16} />
              </div>
              <span className="asset-name">{asset.name}</span>
              {isBlobUrl && (
                <span className="blob-badge" title="Local file - won't export">
                  ⚠️
                </span>
              )}
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
        })}
      </div>
    </div>
  );
}
