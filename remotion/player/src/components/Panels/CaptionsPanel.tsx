import { useState, useRef, useMemo } from 'react';
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
  Mic,
  Loader2,
  Search,
  ChevronDown,
} from 'lucide-react';
import './CaptionsPanel.css';

// Import centralized font definitions
import {
  POPULAR_FONTS,
  UNIQUE_FONTS,
  type CyrillicFont,
} from '@/shared/fonts';

const RENDER_SERVER_URL = import.meta.env.VITE_RENDER_SERVER_URL || 'https://vibee-remotion.fly.dev';

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
  const [isTranscribing, setIsTranscribing] = useState(false);
  const [fontSearch, setFontSearch] = useState('');
  const [showFontDropdown, setShowFontDropdown] = useState(false);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const fontDropdownRef = useRef<HTMLDivElement>(null);


  // Handle Whisper transcription
  const handleTranscribe = async () => {
    const lipSyncVideo = templateProps.lipSyncVideo;
    if (!lipSyncVideo) {
      alert('No video loaded. Please add a lip-sync video first.');
      return;
    }

    setIsTranscribing(true);
    try {
      console.log(`[Captions] Starting transcription for: ${lipSyncVideo}`);

      const response = await fetch(`${RENDER_SERVER_URL}/transcribe`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          videoUrl: lipSyncVideo,
          language: 'ru',
          fps: project.fps,
        }),
      });

      const result = await response.json();

      if (result.success && result.captions) {
        updateTemplateProp('captions', result.captions);
        console.log(`[Captions] Loaded ${result.captions.length} captions from transcription`);
      } else {
        throw new Error(result.error || 'Transcription failed');
      }
    } catch (error) {
      console.error('[Captions] Transcription error:', error);
      alert(`Transcription failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    } finally {
      setIsTranscribing(false);
    }
  };

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

  // Filter fonts based on search
  const filteredFonts = useMemo(() => {
    if (!fontSearch) return UNIQUE_FONTS;
    const query = fontSearch.toLowerCase();
    return UNIQUE_FONTS.filter(
      (f: CyrillicFont) => f.name.toLowerCase().includes(query) || f.id.toLowerCase().includes(query)
    );
  }, [fontSearch]);

  // Get current font name
  const currentFontId = captionStyle.fontId || 'Montserrat';
  const currentFont = UNIQUE_FONTS.find((f: CyrillicFont) => f.id === currentFontId) || POPULAR_FONTS[0];

  // Handle font selection
  const handleFontSelect = (fontId: string) => {
    handleStyleChange('fontId', fontId);
    setShowFontDropdown(false);
    setFontSearch('');
  };

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
            <button
              className="transcribe-btn"
              onClick={handleTranscribe}
              disabled={isTranscribing}
              title="Auto-transcribe Russian audio using Whisper"
            >
              {isTranscribing ? (
                <Loader2 size={16} className="spinning" />
              ) : (
                <Mic size={16} />
              )}
              {isTranscribing ? 'Transcribing...' : 'Transcribe RU'}
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
            <div className="style-row font-selector-row">
              <label>Font ({UNIQUE_FONTS.length} Cyrillic)</label>
              <div className="font-selector" ref={fontDropdownRef}>
                <button
                  className="font-selector-button"
                  onClick={() => setShowFontDropdown(!showFontDropdown)}
                >
                  <span className="font-name">{currentFont.name}</span>
                  <ChevronDown size={14} />
                </button>
                {showFontDropdown && (
                  <div className="font-dropdown">
                    <div className="font-search-wrapper">
                      <Search size={14} />
                      <input
                        type="text"
                        placeholder="Search fonts..."
                        value={fontSearch}
                        onChange={(e) => setFontSearch(e.target.value)}
                        autoFocus
                      />
                    </div>
                    <div className="font-dropdown-list">
                      {!fontSearch && (
                        <>
                          <div className="font-category">Popular</div>
                          {POPULAR_FONTS.map((font: CyrillicFont) => (
                            <button
                              key={font.id}
                              className={`font-option ${currentFontId === font.id ? 'selected' : ''}`}
                              onClick={() => handleFontSelect(font.id)}
                            >
                              <span className="font-option-name">{font.name}</span>
                              <span className="font-option-preview">Привет</span>
                            </button>
                          ))}
                          <div className="font-category">All Fonts</div>
                        </>
                      )}
                      {filteredFonts
                        .filter((f: CyrillicFont) => fontSearch || !POPULAR_FONTS.find((p: CyrillicFont) => p.id === f.id))
                        .map((font: CyrillicFont) => (
                          <button
                            key={font.id}
                            className={`font-option ${currentFontId === font.id ? 'selected' : ''}`}
                            onClick={() => handleFontSelect(font.id)}
                          >
                            <span className="font-option-name">{font.name}</span>
                            <span className="font-option-category">{font.category}</span>
                          </button>
                        ))}
                      {filteredFonts.length === 0 && (
                        <div className="font-no-results">No fonts found</div>
                      )}
                    </div>
                  </div>
                )}
              </div>
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
