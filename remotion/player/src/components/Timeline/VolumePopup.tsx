import { createPortal } from 'react-dom';
import { useAtomValue, useSetAtom } from 'jotai';
import { musicVolumeAtom, volumePopupItemIdAtom } from '@/atoms';

/**
 * VolumePopup - Separate component for music volume control
 *
 * Why separate? TrackItem is wrapped in memo with custom comparison.
 * When musicVolumeAtom changes, memo blocks re-render.
 * This component subscribes directly to Jotai atoms, bypassing memo.
 */
export function VolumePopup() {
  const musicVolume = useAtomValue(musicVolumeAtom);
  const setMusicVolume = useSetAtom(musicVolumeAtom);
  const volumePopupItemId = useAtomValue(volumePopupItemIdAtom);
  const setVolumePopupItemId = useSetAtom(volumePopupItemIdAtom);

  if (!volumePopupItemId) return null;

  const handleVolumeChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setMusicVolume(parseFloat(e.target.value));
  };

  const closePopup = () => setVolumePopupItemId(null);

  return createPortal(
    <div className="volume-popup-overlay" onClick={closePopup}>
      <div className="volume-popup" onClick={(e) => e.stopPropagation()}>
        <div className="volume-popup-header">Music Volume</div>
        <div className="volume-popup-content">
          <input
            type="range"
            min="0"
            max="1"
            step="0.01"
            value={musicVolume}
            onChange={handleVolumeChange}
            className="volume-popup-slider"
          />
          <span className="volume-popup-value">{Math.round(musicVolume * 100)}%</span>
        </div>
        <div className="volume-popup-presets">
          <button type="button" onClick={() => { setMusicVolume(0); closePopup(); }}>Mute</button>
          <button type="button" onClick={() => { setMusicVolume(0.06); closePopup(); }}>6%</button>
          <button type="button" onClick={() => { setMusicVolume(0.15); closePopup(); }}>15%</button>
        </div>
      </div>
    </div>,
    document.body
  );
}
