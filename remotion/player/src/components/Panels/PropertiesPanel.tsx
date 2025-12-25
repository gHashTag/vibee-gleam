import { useAtomValue, useSetAtom } from 'jotai';
import { templatePropsAtom, updateTemplatePropAtom, selectedItemIdsAtom, updateItemAtom, projectAtom, getSelectedItemsAtom } from '@/atoms';
import { useLanguage } from '@/hooks/useLanguage';
import type { LipSyncMainProps, TrackItem, TextItemProps } from '@/store/types';
import './PropertiesPanel.css';

export function PropertiesPanel() {
  const { t } = useLanguage();
  const templateProps = useAtomValue(templatePropsAtom);
  const updateTemplateProp = useSetAtom(updateTemplatePropAtom);
  const selectedItemIds = useAtomValue(selectedItemIdsAtom);
  const selectedItems = useAtomValue(getSelectedItemsAtom);
  const updateItem = useSetAtom(updateItemAtom);
  const project = useAtomValue(projectAtom);
  const selectedTextItem = selectedItems.find((item): item is TrackItem & TextItemProps => item.type === 'text');

  // Batch operations for multiple selected items
  const adjustDuration = (deltaFrames: number) => {
    selectedItems.forEach((item) => {
      const newDuration = Math.max(1, item.durationInFrames + deltaFrames);
      updateItem({ itemId: item.id, updates: { durationInFrames: newDuration } });
    });
  };

  const setUniformDuration = () => {
    if (selectedItems.length === 0) return;
    // Use shortest item as baseline
    const minDuration = Math.min(...selectedItems.map((i) => i.durationInFrames));
    selectedItems.forEach((item) => {
      updateItem({ itemId: item.id, updates: { durationInFrames: minDuration } });
    });
  };

  const handleChange = <K extends keyof LipSyncMainProps>(
    key: K,
    value: LipSyncMainProps[K]
  ) => {
    console.log(`[PropertiesPanel] handleChange called: ${String(key)} =`, value);
    updateTemplateProp({ key: key as any, value });
    console.log(`[PropertiesPanel] handleChange done: ${String(key)}`);
  };

  const handleTextChange = (key: string, value: any) => {
    if (selectedTextItem) {
      updateItem({ itemId: selectedTextItem.id, updates: { [key]: value } });
    }
  };

  // Show batch operations for multiple selected items
  if (selectedItems.length > 1) {
    const fps = project.fps;
    return (
      <div className="properties-panel">
        <div className="properties-section">
          <h3 className="properties-section-title">📦 {t('props.batchEdit')} ({selectedItems.length} {t('props.items')})</h3>

          <div className="batch-duration">
            <label className="property-label">{t('props.adjustDuration')}</label>
            <div className="batch-buttons">
              <button onClick={() => adjustDuration(-fps)} title={t('props.minus1s')}>-1s</button>
              <button onClick={() => adjustDuration(Math.round(-fps / 2))} title={t('props.minus05s')}>-0.5s</button>
              <button onClick={() => adjustDuration(Math.round(fps / 2))} title={t('props.plus05s')}>+0.5s</button>
              <button onClick={() => adjustDuration(fps)} title={t('props.plus1s')}>+1s</button>
            </div>
          </div>

          <div className="batch-uniform">
            <button className="batch-uniform-btn" onClick={setUniformDuration}>
              {t('props.makeSameDuration')}
            </button>
            <span className="batch-hint">
              {t('props.setsAllToShortest')}: {Math.min(...selectedItems.map((i) => i.durationInFrames))}f
            </span>
          </div>
        </div>

        <div className="properties-section">
          <h3 className="properties-section-title">📊 {t('props.selectionInfo')}</h3>
          <div className="selection-info">
            {['video', 'audio', 'avatar', 'text', 'image'].map((type) => {
              const count = selectedItems.filter((i) => i.type === type).length;
              if (count === 0) return null;
              return (
                <div key={type} className="selection-info-item">
                  <span className="selection-type">{type}</span>
                  <span className="selection-count">{count}</span>
                </div>
              );
            })}
          </div>
        </div>
      </div>
    );
  }

  // Show text properties if a text item is selected
  if (selectedTextItem) {
    return (
      <div className="properties-panel">
        <div className="properties-section">
          <h3 className="properties-section-title">✏️ {t('section.text')}</h3>
          <PropertyTextArea
            label={t('props.content')}
            value={(selectedTextItem as any).text || ''}
            onChange={(v) => handleTextChange('text', v)}
            placeholder={t('props.enterText')}
          />
        </div>

        <div className="properties-section">
          <h3 className="properties-section-title">🎨 {t('section.style')}</h3>
          <PropertyInput
            label={t('props.fontSize')}
            value={(selectedTextItem as any).fontSize || 48}
            onChange={(v) => handleTextChange('fontSize', v)}
            min={12}
            max={200}
            step={1}
            suffix="px"
          />
          <PropertyColor
            label={t('props.color')}
            value={(selectedTextItem as any).color || '#ffffff'}
            onChange={(v) => handleTextChange('color', v)}
          />
          <PropertySelect
            label={t('props.weight')}
            value={String((selectedTextItem as any).fontWeight || 400)}
            onChange={(v) => handleTextChange('fontWeight', Number(v))}
            options={[
              { value: '300', label: t('font.light') },
              { value: '400', label: t('font.regular') },
              { value: '500', label: t('font.medium') },
              { value: '600', label: t('font.semibold') },
              { value: '700', label: t('font.bold') },
              { value: '800', label: t('font.extrabold') },
            ]}
          />
          <PropertySelect
            label={t('props.align')}
            value={(selectedTextItem as any).textAlign || 'center'}
            onChange={(v) => handleTextChange('textAlign', v)}
            options={[
              { value: 'left', label: t('props.left') },
              { value: 'center', label: t('props.center') },
              { value: 'right', label: t('props.right') },
            ]}
          />
        </div>

        <div className="properties-section">
          <h3 className="properties-section-title">📍 {t('section.position')}</h3>
          <div className="properties-grid">
            <PropertyInput
              label="X"
              value={selectedTextItem.x}
              onChange={(v) => handleTextChange('x', v)}
              min={0}
              max={1080}
              step={1}
              suffix="px"
            />
            <PropertyInput
              label="Y"
              value={selectedTextItem.y}
              onChange={(v) => handleTextChange('y', v)}
              min={0}
              max={1920}
              step={1}
              suffix="px"
            />
          </div>
          <PropertySlider
            label={t('props.opacity')}
            value={selectedTextItem.opacity}
            onChange={(v) => handleTextChange('opacity', v)}
            min={0}
            max={1}
            step={0.05}
          />
        </div>

        <div className="properties-section">
          <h3 className="properties-section-title">⏱️ {t('section.timing')}</h3>
          <div className="properties-grid">
            <PropertyInput
              label={t('props.start')}
              value={selectedTextItem.startFrame}
              onChange={(v) => handleTextChange('startFrame', v)}
              min={0}
              step={1}
              suffix="f"
            />
            <PropertyInput
              label={t('props.duration')}
              value={selectedTextItem.durationInFrames}
              onChange={(v) => handleTextChange('durationInFrames', v)}
              min={1}
              step={1}
              suffix="f"
            />
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="properties-panel">
      {/* Media Section */}
      <div className="properties-section">
        <h3 className="properties-section-title">📁 {t('section.media')}</h3>
        <PropertyFileInput
          label={t('props.lipsyncVideo')}
          value={templateProps.lipSyncVideo}
          onChange={(v) => handleChange('lipSyncVideo', v)}
          placeholder={t('props.pathPlaceholder')}
        />
        <PropertyFileInput
          label={t('props.coverImage')}
          value={templateProps.coverImage}
          onChange={(v) => handleChange('coverImage', v)}
          placeholder={t('props.pathPlaceholder')}
        />
        <PropertyFileInput
          label={t('props.backgroundMusic')}
          value={templateProps.backgroundMusic}
          onChange={(v) => handleChange('backgroundMusic', v)}
          placeholder={t('props.pathPlaceholder')}
        />
      </div>

      {/* Effects Section */}
      <div className="properties-section">
        <h3 className="properties-section-title">🎚️ {t('section.effects')}</h3>
        <PropertySlider
          label={t('props.musicVolume')}
          value={templateProps.musicVolume}
          onChange={(v) => handleChange('musicVolume', v)}
          min={0}
          max={1}
          step={0.05}
        />
        <PropertyInput
          label={t('props.coverDuration')}
          value={templateProps.coverDuration}
          onChange={(v) => handleChange('coverDuration', v)}
          min={0}
          max={5}
          step={0.1}
          suffix="s"
        />
        <PropertySlider
          label={t('props.vignette')}
          value={templateProps.vignetteStrength}
          onChange={(v) => handleChange('vignetteStrength', v)}
          min={0}
          max={1}
          step={0.05}
        />
        <PropertySlider
          label={t('props.colorCorrection')}
          value={templateProps.colorCorrection}
          onChange={(v) => handleChange('colorCorrection', v)}
          min={0.8}
          max={1.5}
          step={0.05}
        />
      </div>

      {/* Avatar Circle Section */}
      <div className="properties-section">
        <h3 className="properties-section-title">🎯 {t('section.avatar')}</h3>
        <div className="properties-grid">
          <PropertyInput
            label={t('props.size')}
            value={templateProps.circleSizePercent}
            onChange={(v) => handleChange('circleSizePercent', v)}
            min={10}
            max={50}
            step={0.1}
            suffix="%"
          />
          <PropertyInput
            label={t('props.bottom')}
            value={templateProps.circleBottomPercent}
            onChange={(v) => handleChange('circleBottomPercent', v)}
            min={0}
            max={50}
            step={1}
            suffix="%"
          />
          <PropertyInput
            label={t('props.left')}
            value={templateProps.circleLeftPx}
            onChange={(v) => handleChange('circleLeftPx', v)}
            min={0}
            max={200}
            step={1}
            suffix="px"
          />
        </div>
      </div>

      {/* Backgrounds Info */}
      <div className="properties-section">
        <h3 className="properties-section-title">🎬 {t('section.backgrounds')}</h3>
        <div className="backgrounds-info">
          <p className="info-text">
            {templateProps.backgroundVideos.length} {t('props.videos')}
          </p>
          <p className="info-hint">
            {t('props.dragVideosHint')}
          </p>
        </div>
      </div>
    </div>
  );
}

// ===============================
// Sub-components
// ===============================

interface PropertyInputProps {
  label: string;
  value: number;
  onChange: (value: number) => void;
  min?: number;
  max?: number;
  step?: number;
  suffix?: string;
}

function PropertyInput({
  label,
  value,
  onChange,
  min,
  max,
  step,
  suffix,
}: PropertyInputProps) {
  return (
    <div className="property-input">
      <label className="property-label">{label}</label>
      <div className="property-input-wrapper">
        <input
          type="number"
          value={value}
          onChange={(e) => onChange(Number(e.target.value))}
          min={min}
          max={max}
          step={step}
        />
        {suffix && <span className="property-suffix">{suffix}</span>}
      </div>
    </div>
  );
}

interface PropertySliderProps {
  label: string;
  value: number;
  onChange: (value: number) => void;
  min: number;
  max: number;
  step: number;
}

function PropertySlider({
  label,
  value,
  onChange,
  min,
  max,
  step,
}: PropertySliderProps) {
  return (
    <div className="property-slider">
      <div className="property-slider-header">
        <label className="property-label">{label}</label>
        <span className="property-value">{value.toFixed(2)}</span>
      </div>
      <input
        type="range"
        value={value}
        onChange={(e) => onChange(Number(e.target.value))}
        min={min}
        max={max}
        step={step}
      />
    </div>
  );
}

interface PropertyFileInputProps {
  label: string;
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
}

function PropertyFileInput({ label, value, onChange, placeholder }: PropertyFileInputProps) {
  const fileName = value.split('/').pop() || 'None';

  return (
    <div className="property-file">
      <label className="property-label">{label}</label>
      <div className="property-file-wrapper">
        <span className="property-file-name" title={value}>
          {fileName}
        </span>
        <input
          type="text"
          value={value}
          onChange={(e) => onChange(e.target.value)}
          placeholder={placeholder}
          className="property-file-input"
        />
      </div>
    </div>
  );
}

interface PropertyTextAreaProps {
  label: string;
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
}

function PropertyTextArea({ label, value, onChange, placeholder }: PropertyTextAreaProps) {
  return (
    <div className="property-textarea">
      <label className="property-label">{label}</label>
      <textarea
        value={value}
        onChange={(e) => onChange(e.target.value)}
        rows={3}
        placeholder={placeholder}
      />
    </div>
  );
}

interface PropertyColorProps {
  label: string;
  value: string;
  onChange: (value: string) => void;
}

function PropertyColor({ label, value, onChange }: PropertyColorProps) {
  return (
    <div className="property-color">
      <label className="property-label">{label}</label>
      <div className="property-color-wrapper">
        <input
          type="color"
          value={value}
          onChange={(e) => onChange(e.target.value)}
        />
        <input
          type="text"
          value={value}
          onChange={(e) => onChange(e.target.value)}
          placeholder="#ffffff"
          className="property-color-hex"
        />
      </div>
    </div>
  );
}

interface PropertySelectProps {
  label: string;
  value: string;
  onChange: (value: string) => void;
  options: { value: string; label: string }[];
}

function PropertySelect({ label, value, onChange, options }: PropertySelectProps) {
  return (
    <div className="property-select">
      <label className="property-label">{label}</label>
      <select value={value} onChange={(e) => onChange(e.target.value)}>
        {options.map((opt) => (
          <option key={opt.value} value={opt.value}>
            {opt.label}
          </option>
        ))}
      </select>
    </div>
  );
}
