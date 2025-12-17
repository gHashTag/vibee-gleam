import { useState, useRef } from 'react';
import { useEditorStore } from '@/store/editorStore';
import type { CaptionItem, CaptionStyle } from '@/store/types';
import {
  Plus,
  Trash2,
  Type,
  Palette,
  Eye,
  EyeOff,
  Upload,
  FileText,
} from 'lucide-react';
import './CaptionsPanel.css';

// Parse SRT timestamp to milliseconds (format: 00:00:00,000)
function parseSrtTime(time: string): number {
  const [hours, minutes, rest] = time.split(':');
  const [seconds, ms] = rest.replace(',', '.').split('.');
  return (
    parseInt(hours) * 3600000 +
    parseInt(minutes) * 60000 +
    parseInt(seconds) * 1000 +
    parseInt(ms || '0')
  );
}

// Parse VTT timestamp to milliseconds (format: 00:00:00.000 or 00:00.000)
function parseVttTime(time: string): number {
  const parts = time.split(':');
  if (parts.length === 3) {
    const [hours, minutes, seconds] = parts;
    const [secs, ms] = seconds.split('.');
    return (
      parseInt(hours) * 3600000 +
      parseInt(minutes) * 60000 +
      parseInt(secs) * 1000 +
      parseInt(ms || '0')
    );
  } else {
    const [minutes, seconds] = parts;
    const [secs, ms] = seconds.split('.');
    return parseInt(minutes) * 60000 + parseInt(secs) * 1000 + parseInt(ms || '0');
  }
}

// Parse SRT file content
function parseSrt(content: string): CaptionItem[] {
  const captions: CaptionItem[] = [];
  const blocks = content.trim().split(/\n\s*\n/);

  for (const block of blocks) {
    const lines = block.trim().split('\n');
    if (lines.length < 3) continue;

    // Line 0: sequence number (skip)
    // Line 1: timestamp
    // Lines 2+: text
    const timeLine = lines[1];
    const timeMatch = timeLine.match(/(\d{2}:\d{2}:\d{2}[,\.]\d{3})\s*-->\s*(\d{2}:\d{2}:\d{2}[,\.]\d{3})/);
    if (!timeMatch) continue;

    const startMs = parseSrtTime(timeMatch[1]);
    const endMs = parseSrtTime(timeMatch[2]);
    const text = lines.slice(2).join(' ').replace(/<[^>]*>/g, '').trim();

    if (text) {
      captions.push({
        text,
        startMs,
        endMs,
        timestampMs: startMs,
        confidence: null,
      });
    }
  }

  return captions;
}

// Parse VTT file content
function parseVtt(content: string): CaptionItem[] {
  const captions: CaptionItem[] = [];
  // Remove WEBVTT header and metadata
  const lines = content.replace(/^WEBVTT.*$/m, '').trim().split(/\n\s*\n/);

  for (const block of lines) {
    const blockLines = block.trim().split('\n');

    // Find the timestamp line
    let timeLineIndex = 0;
    for (let i = 0; i < blockLines.length; i++) {
      if (blockLines[i].includes('-->')) {
        timeLineIndex = i;
        break;
      }
    }

    const timeLine = blockLines[timeLineIndex];
    if (!timeLine || !timeLine.includes('-->')) continue;

    const timeMatch = timeLine.match(/(\d{1,2}:?\d{2}:\d{2}[\.]\d{3})\s*-->\s*(\d{1,2}:?\d{2}:\d{2}[\.]\d{3})/);
    if (!timeMatch) continue;

    const startMs = parseVttTime(timeMatch[1]);
    const endMs = parseVttTime(timeMatch[2]);
    const text = blockLines.slice(timeLineIndex + 1).join(' ').replace(/<[^>]*>/g, '').trim();

    if (text) {
      captions.push({
        text,
        startMs,
        endMs,
        timestampMs: startMs,
        confidence: null,
      });
    }
  }

  return captions;
}

export function CaptionsPanel() {
  const templateProps = useEditorStore((s) => s.templateProps);
  const updateTemplateProp = useEditorStore((s) => s.updateTemplateProp);
  const currentFrame = useEditorStore((s) => s.currentFrame);
  const project = useEditorStore((s) => s.project);

  const [activeTab, setActiveTab] = useState<'captions' | 'style'>('captions');
  const fileInputRef = useRef<HTMLInputElement>(null);

  const handleFileUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = (event) => {
      const content = event.target?.result as string;
      if (!content) return;

      let parsedCaptions: CaptionItem[] = [];

      if (file.name.endsWith('.srt')) {
        parsedCaptions = parseSrt(content);
      } else if (file.name.endsWith('.vtt')) {
        parsedCaptions = parseVtt(content);
      }

      if (parsedCaptions.length > 0) {
        updateTemplateProp('captions', parsedCaptions);
        console.log(`[Captions] Loaded ${parsedCaptions.length} captions from ${file.name}`);
      } else {
        alert('Could not parse captions from file. Please check the format.');
      }
    };

    reader.readAsText(file);
    // Reset input so the same file can be selected again
    e.target.value = '';
  };

  const captions = templateProps.captions || [];
  const captionStyle = templateProps.captionStyle || {};
  const showCaptions = templateProps.showCaptions ?? true;

  const currentTimeMs = (currentFrame / project.fps) * 1000;

  // Find current caption
  const currentCaption = captions.find(
    (c) => currentTimeMs >= c.startMs && currentTimeMs < c.endMs
  );

  const handleToggleCaptions = () => {
    updateTemplateProp('showCaptions', !showCaptions);
  };

  const handleAddCaption = () => {
    const newCaption: CaptionItem = {
      text: 'New caption',
      startMs: currentTimeMs,
      endMs: currentTimeMs + 2000, // 2 seconds
      timestampMs: currentTimeMs,
      confidence: null,
    };
    updateTemplateProp('captions', [...captions, newCaption]);
  };

  const handleUpdateCaption = (index: number, updates: Partial<CaptionItem>) => {
    const newCaptions = [...captions];
    newCaptions[index] = { ...newCaptions[index], ...updates };
    updateTemplateProp('captions', newCaptions);
  };

  const handleDeleteCaption = (index: number) => {
    const newCaptions = captions.filter((_, i) => i !== index);
    updateTemplateProp('captions', newCaptions);
  };

  const handleStyleChange = (key: keyof CaptionStyle, value: any) => {
    updateTemplateProp('captionStyle', { ...captionStyle, [key]: value });
  };

  const formatTime = (ms: number) => {
    const seconds = Math.floor(ms / 1000);
    const minutes = Math.floor(seconds / 60);
    const secs = seconds % 60;
    const millis = Math.floor((ms % 1000) / 10);
    return `${minutes}:${secs.toString().padStart(2, '0')}.${millis.toString().padStart(2, '0')}`;
  };

  const parseTime = (timeStr: string): number => {
    const parts = timeStr.split(':');
    if (parts.length === 2) {
      const [minSec, millis] = parts[1].split('.');
      const minutes = parseInt(parts[0]) || 0;
      const seconds = parseInt(minSec) || 0;
      const ms = parseInt(millis) * 10 || 0;
      return minutes * 60000 + seconds * 1000 + ms;
    }
    return 0;
  };

  return (
    <div className="captions-panel">
      {/* Header */}
      <div className="captions-header">
        <div className="captions-tabs">
          <button
            className={`caption-tab ${activeTab === 'captions' ? 'active' : ''}`}
            onClick={() => setActiveTab('captions')}
          >
            <Type size={14} />
            Captions
          </button>
          <button
            className={`caption-tab ${activeTab === 'style' ? 'active' : ''}`}
            onClick={() => setActiveTab('style')}
          >
            <Palette size={14} />
            Style
          </button>
        </div>
        <button
          className={`caption-toggle ${showCaptions ? 'active' : ''}`}
          onClick={handleToggleCaptions}
          title={showCaptions ? 'Hide captions' : 'Show captions'}
        >
          {showCaptions ? <Eye size={16} /> : <EyeOff size={16} />}
        </button>
      </div>

      {/* Captions Tab */}
      {activeTab === 'captions' && (
        <div className="captions-content">
          {/* Action Buttons */}
          <div className="caption-actions">
            <button className="add-caption-btn" onClick={handleAddCaption}>
              <Plus size={16} />
              Add at {formatTime(currentTimeMs)}
            </button>
            <input
              ref={fileInputRef}
              type="file"
              accept=".srt,.vtt"
              onChange={handleFileUpload}
              style={{ display: 'none' }}
            />
            <button
              className="upload-caption-btn"
              onClick={() => fileInputRef.current?.click()}
              title="Upload .srt or .vtt file"
            >
              <Upload size={16} />
              Import
            </button>
          </div>

          {/* Captions List */}
          <div className="captions-list">
            {captions.length === 0 ? (
              <div className="captions-empty">
                <FileText size={32} />
                <p>No captions yet</p>
                <span>Add manually or import .srt/.vtt file</span>
              </div>
            ) : (
              captions
                .slice()
                .sort((a, b) => a.startMs - b.startMs)
                .map((caption, index) => {
                  const isCurrent =
                    currentTimeMs >= caption.startMs && currentTimeMs < caption.endMs;
                  return (
                    <div
                      key={index}
                      className={`caption-item ${isCurrent ? 'current' : ''}`}
                    >
                      <div className="caption-timing">
                        <input
                          type="text"
                          value={formatTime(caption.startMs)}
                          onChange={(e) => {
                            const ms = parseTime(e.target.value);
                            handleUpdateCaption(index, { startMs: ms, timestampMs: ms });
                          }}
                          className="time-input"
                        />
                        <span className="time-separator">→</span>
                        <input
                          type="text"
                          value={formatTime(caption.endMs)}
                          onChange={(e) => {
                            handleUpdateCaption(index, { endMs: parseTime(e.target.value) });
                          }}
                          className="time-input"
                        />
                      </div>
                      <textarea
                        value={caption.text}
                        onChange={(e) => handleUpdateCaption(index, { text: e.target.value })}
                        className="caption-text"
                        rows={2}
                      />
                      <button
                        className="caption-delete"
                        onClick={() => handleDeleteCaption(index)}
                      >
                        <Trash2 size={14} />
                      </button>
                    </div>
                  );
                })
            )}
          </div>
        </div>
      )}

      {/* Style Tab */}
      {activeTab === 'style' && (
        <div className="captions-style">
          <div className="style-section">
            <h4 className="style-section-title">Text</h4>
            <div className="style-row">
              <label>Font Size</label>
              <input
                type="number"
                value={captionStyle.fontSize || 48}
                onChange={(e) => handleStyleChange('fontSize', Number(e.target.value))}
                min={24}
                max={120}
              />
            </div>
            <div className="style-row">
              <label>Font Weight</label>
              <select
                value={captionStyle.fontWeight || 700}
                onChange={(e) => handleStyleChange('fontWeight', Number(e.target.value))}
              >
                <option value={400}>Regular</option>
                <option value={500}>Medium</option>
                <option value={600}>SemiBold</option>
                <option value={700}>Bold</option>
                <option value={800}>ExtraBold</option>
              </select>
            </div>
          </div>

          <div className="style-section">
            <h4 className="style-section-title">Colors</h4>
            <div className="style-row">
              <label>Text Color</label>
              <input
                type="color"
                value={captionStyle.textColor || '#ffffff'}
                onChange={(e) => handleStyleChange('textColor', e.target.value)}
              />
            </div>
            <div className="style-row">
              <label>Highlight</label>
              <input
                type="color"
                value={captionStyle.highlightColor || '#f59e0b'}
                onChange={(e) => handleStyleChange('highlightColor', e.target.value)}
              />
            </div>
            <div className="style-row">
              <label>Background</label>
              <input
                type="color"
                value={captionStyle.backgroundColor || '#000000'}
                onChange={(e) => handleStyleChange('backgroundColor', e.target.value + '99')}
              />
            </div>
          </div>

          <div className="style-section">
            <h4 className="style-section-title">Position</h4>
            <div className="style-row">
              <label>Bottom %</label>
              <input
                type="number"
                value={captionStyle.bottomPercent || 20}
                onChange={(e) => handleStyleChange('bottomPercent', Number(e.target.value))}
                min={5}
                max={50}
              />
            </div>
            <div className="style-row">
              <label>Max Width %</label>
              <input
                type="number"
                value={captionStyle.maxWidthPercent || 85}
                onChange={(e) => handleStyleChange('maxWidthPercent', Number(e.target.value))}
                min={50}
                max={100}
              />
            </div>
          </div>

          <div className="style-section">
            <h4 className="style-section-title">Effects</h4>
            <div className="style-row checkbox">
              <label>
                <input
                  type="checkbox"
                  checked={captionStyle.showShadow ?? true}
                  onChange={(e) => handleStyleChange('showShadow', e.target.checked)}
                />
                Text Shadow
              </label>
            </div>
          </div>
        </div>
      )}

      {/* Current Caption Preview */}
      {currentCaption && showCaptions && (
        <div className="caption-preview">
          <span className="preview-label">Current:</span>
          <span className="preview-text">{currentCaption.text}</span>
        </div>
      )}
    </div>
  );
}
