import { useState } from 'react';
import { useAtomValue, useSetAtom } from 'jotai';
import { generatedResultsAtom, removeGeneratedResultAtom, type GeneratedResult } from '@/atoms/generateResults';
import { addAssetAtom } from '@/atoms/assets';
import { useEditorStore } from '@/store/editorStore';
import { useLanguage } from '@/hooks/useLanguage';
import { toAbsoluteUrl } from '@/lib/mediaUrl';
import { Trash2, Plus, GripVertical, Image, Video, Music, Share2 } from 'lucide-react';
import { PublishModal } from '@/components/Modals/PublishModal';
import type { Asset } from '@/store/types';
import './ResultsGallery.css';

interface ResultsGalleryProps {
  tab: 'image' | 'video' | 'audio' | 'lipsync';
}

export function ResultsGallery({ tab }: ResultsGalleryProps) {
  const { t } = useLanguage();
  const generatedResults = useAtomValue(generatedResultsAtom);
  const removeResult = useSetAtom(removeGeneratedResultAtom);
  const addItem = useEditorStore((s) => s.addItem);

  // Publish modal state
  const [publishModalOpen, setPublishModalOpen] = useState(false);
  const [publishVideoUrl, setPublishVideoUrl] = useState<string | undefined>();

  // Get results for current tab
  const results = generatedResults[tab];

  // Handle share to feed
  const handleShare = (result: GeneratedResult) => {
    setPublishVideoUrl(toAbsoluteUrl(result.url));
    setPublishModalOpen(true);
  };

  // Handle drag start for drag & drop to timeline
  const handleDragStart = (e: React.DragEvent, result: GeneratedResult) => {
    const asset: Asset = {
      id: result.id,
      type: result.type,
      name: result.name,
      url: result.url,
    };
    e.dataTransfer.setData('application/json', JSON.stringify(asset));
    e.dataTransfer.effectAllowed = 'copy';
  };

  // Add result to timeline
  const handleAddToTimeline = (result: GeneratedResult) => {
    const trackId = result.type === 'audio' ? 'track-audio' : 'track-video';

    addItem(trackId, {
      type: result.type as 'video' | 'image' | 'audio',
      assetId: result.id,
      startFrame: 0,
      durationInFrames: result.type === 'audio' ? 150 : 90,
      x: 0,
      y: 0,
      width: 1080,
      height: result.type === 'video' ? 1920 : 1080,
      rotation: 0,
      opacity: 1,
      ...(result.type === 'video' && { volume: 1, playbackRate: 1 }),
      ...(result.type === 'audio' && { volume: 1 }),
    });
  };

  // Remove result
  const handleRemoveResult = (resultId: string) => {
    removeResult({ tab, resultId });
  };

  // Get icon for empty state
  const getEmptyIcon = () => {
    switch (tab) {
      case 'image': return <Image size={48} />;
      case 'video': return <Video size={48} />;
      case 'audio': return <Music size={48} />;
      case 'lipsync': return <Video size={48} />;
    }
  };

  // Get empty message
  const getEmptyMessage = () => {
    switch (tab) {
      case 'image': return t('results.noImages') || 'No images yet. Generate some photos!';
      case 'video': return t('results.noVideos') || 'No videos yet. Generate some videos!';
      case 'audio': return t('results.noAudio') || 'No audio yet. Generate some voice!';
      case 'lipsync': return t('results.noLipsync') || 'No lipsync videos yet. Generate some!';
    }
  };

  return (
    <div className="results-gallery">
      {results.length === 0 ? (
        <div className="results-empty">
          <div className="results-empty-icon">{getEmptyIcon()}</div>
          <span className="results-empty-text">{getEmptyMessage()}</span>
        </div>
      ) : (
        <div className="results-grid">
          {results.map((result) => (
            <div
              key={result.id}
              className="result-card"
              draggable
              onDragStart={(e) => handleDragStart(e, result)}
            >
              <div className="result-drag-handle">
                <GripVertical size={16} />
              </div>

              <div className="result-preview">
                {result.type === 'image' && (
                  <img
                    src={toAbsoluteUrl(result.url)}
                    alt={result.name}
                    className="result-image"
                  />
                )}
                {result.type === 'video' && (
                  <video
                    src={toAbsoluteUrl(result.url)}
                    className="result-video"
                    controls
                    muted
                  />
                )}
                {result.type === 'audio' && (
                  <div className="result-audio-container">
                    <Music size={32} className="result-audio-icon" />
                    <audio
                      src={toAbsoluteUrl(result.url)}
                      controls
                      className="result-audio"
                    />
                  </div>
                )}
              </div>

              <div className="result-info">
                <span className="result-name">{result.name}</span>
                <div className="result-actions">
                  {result.type === 'video' && (
                    <button
                      className="result-action-btn result-share-btn"
                      onClick={() => handleShare(result)}
                      title={t('publish.share')}
                    >
                      <Share2 size={16} />
                    </button>
                  )}
                  <button
                    className="result-action-btn result-add-btn"
                    onClick={() => handleAddToTimeline(result)}
                    title={t('generate.addToTimeline')}
                  >
                    <Plus size={16} />
                  </button>
                  <button
                    className="result-action-btn result-remove-btn"
                    onClick={() => handleRemoveResult(result.id)}
                    title={t('generate.remove')}
                  >
                    <Trash2 size={16} />
                  </button>
                </div>
              </div>
            </div>
          ))}
        </div>
      )}

      <PublishModal
        isOpen={publishModalOpen}
        onClose={() => setPublishModalOpen(false)}
        videoUrl={publishVideoUrl}
      />
    </div>
  );
}
