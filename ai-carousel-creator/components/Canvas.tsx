
import * as React from 'react';
import type { Slide, GlobalSettings, Language, AspectRatio, SlideElement, FrameType } from '../types';
import { SwipeRightIcon } from './icons/SwipeRightIcon';
import { DownloadIcon } from './icons/DownloadIcon';
import { TRANSLATIONS } from '../constants';

interface CanvasProps {
  slide: Slide | undefined;
  activeElementId: string | null;
  settings: GlobalSettings;
  isLastSlide: boolean;
  aspectRatioClassName: string;
  aspectRatio: AspectRatio;
  onUpdateElement: (slideId: string, elementId: string, updatedProps: Partial<SlideElement>) => void;
  onSelectElement: (elementId: string | null) => void;
  onDownloadSingleSlide?: (slideId: string) => void;
  isExportMode?: boolean;
  language: Language;
}

const BASE_WIDTH = 540; 
const SNAP_STEP = 2.5; // Snap every 2.5% for layout precision

export const Canvas: React.FC<CanvasProps> = ({ 
  slide, 
  activeElementId,
  settings, 
  isLastSlide, 
  aspectRatio = '4:5', // Default fallback
  onUpdateElement, 
  onSelectElement,
  onDownloadSingleSlide,
  isExportMode = false,
  language
}) => {
  const canvasRef = React.useRef<HTMLDivElement>(null);
  const textareaRef = React.useRef<HTMLTextAreaElement>(null);
  const [isEditing, setIsEditing] = React.useState(false);
  const [isDragging, setIsDragging] = React.useState(false);
  const [scale, setScale] = React.useState(1);
  const [editText, setEditText] = React.useState('');

  const activeElement = React.useMemo(() => 
    slide?.elements.find(e => e.id === activeElementId), 
  [slide, activeElementId]);

  const ratioValues = React.useMemo(() => {
      const parts = aspectRatio.split(':').map(Number);
      return { w: parts[0] || 4, h: parts[1] || 5 };
  }, [aspectRatio]);

  const baseHeight = React.useMemo(() => {
    return (BASE_WIDTH * ratioValues.h) / ratioValues.w;
  }, [ratioValues]);

  // Robust scale calculation
  React.useLayoutEffect(() => {
    if (!canvasRef.current || isExportMode) return;
    
    const updateScale = () => {
        if (canvasRef.current) {
            const currentWidth = canvasRef.current.offsetWidth;
            if (currentWidth > 10) { 
                setScale(currentWidth / BASE_WIDTH);
            }
        }
    };
    
    updateScale();
    const timeout = setTimeout(updateScale, 100);
    const observer = new ResizeObserver(updateScale);
    observer.observe(canvasRef.current);
    
    return () => {
        observer.disconnect();
        clearTimeout(timeout);
    };
  }, [aspectRatio, slide, isExportMode]);

  React.useEffect(() => {
    if (activeElement && (activeElement.type === 'text' || activeElement.type === 'heading')) {
        setEditText(activeElement.content || '');
    }
    setIsEditing(false); 
  }, [activeElementId, slide?.id]);

  React.useEffect(() => {
    if (isEditing && textareaRef.current) {
        textareaRef.current.focus();
        textareaRef.current.style.height = 'auto';
        textareaRef.current.style.height = `${textareaRef.current.scrollHeight}px`;
    }
  }, [isEditing]);

  const handleElementDoubleClick = (e: React.MouseEvent, element: SlideElement) => {
    e.stopPropagation();
    if (element.lock) return;
    if (element.type === 'image' || element.type === 'sticker') return;
    onSelectElement(element.id);
    setIsEditing(true);
  };

  const handleTextChange = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    setEditText(e.target.value);
    e.target.style.height = 'auto';
    e.target.style.height = `${e.target.scrollHeight}px`;
  };

  const handleTextBlur = () => {
    if (slide && activeElementId) {
        onUpdateElement(slide.id, activeElementId, { content: editText });
    }
    setIsEditing(false);
  };

  // Helper for snapping
  const snapValue = (val: number) => Math.round(val / SNAP_STEP) * SNAP_STEP;

  // --- MOUSE DRAG ---
  const mouseDragRef = React.useRef<{ elementId: string; startX: number; startY: number; initialX: number; initialY: number; } | null>(null);
  
  const handleMouseMove = (e: MouseEvent) => {
    if (!mouseDragRef.current || !canvasRef.current || !slide) return;
    e.preventDefault();

    const { startX, startY, initialX, initialY, elementId } = mouseDragRef.current;
    const canvasRect = canvasRef.current.getBoundingClientRect();
    if (canvasRect.width === 0 || canvasRect.height === 0) return;

    const deltaX = e.clientX - startX;
    const deltaY = e.clientY - startY;
    const deltaPercentX = (deltaX / canvasRect.width) * 100;
    const deltaPercentY = (deltaY / canvasRect.height) * 100;

    const rawX = initialX + deltaPercentX;
    const rawY = initialY + deltaPercentY;

    // Apply Snapping
    const snappedX = snapValue(rawX);
    const snappedY = snapValue(rawY);

    onUpdateElement(slide.id, elementId, { position: { x: snappedX, y: snappedY } });
  };
  
  const handleMouseUp = () => {
    mouseDragRef.current = null;
    setIsDragging(false);
    window.removeEventListener('mousemove', handleMouseMove);
    window.removeEventListener('mouseup', handleMouseUp);
  };

  const handleMouseDown = (e: React.MouseEvent, element: SlideElement) => {
    if (isEditing || !slide) return;
    if (e.button !== 0) return;
    if (element.lock) { onSelectElement(element.id); return; }
    e.stopPropagation(); 
    onSelectElement(element.id);

    setIsDragging(true);
    mouseDragRef.current = {
      elementId: element.id,
      startX: e.clientX,
      startY: e.clientY,
      initialX: element.position.x,
      initialY: element.position.y,
    };
    window.addEventListener('mousemove', handleMouseMove);
    window.addEventListener('mouseup', handleMouseUp);
  };

  // --- TOUCH GESTURES ---
  const touchRef = React.useRef<{ mode: 'drag' | 'gesture'; elementId: string; startDist?: number; startAngle?: number; startScale?: number; startRotation?: number; startX?: number; startY?: number; initialX?: number; initialY?: number; } | null>(null);

  const getDistance = (t1: React.Touch, t2: React.Touch) => Math.sqrt(Math.pow(t1.clientX - t2.clientX, 2) + Math.pow(t1.clientY - t2.clientY, 2));
  const getAngle = (t1: React.Touch, t2: React.Touch) => (Math.atan2(t1.clientY - t2.clientY, t1.clientX - t2.clientX) * 180) / Math.PI;

  const handleTouchMove = (e: TouchEvent) => {
    if (!touchRef.current || !canvasRef.current || !slide) return;
    if (e.cancelable) e.preventDefault();

    const { mode, elementId } = touchRef.current;
    
    if (mode === 'drag' && e.touches.length === 1) {
        const touch = e.touches[0];
        const { startX, startY, initialX, initialY } = touchRef.current;
        const canvasRect = canvasRef.current.getBoundingClientRect();
        
        const deltaX = touch.clientX - (startX || 0);
        const deltaY = touch.clientY - (startY || 0);
        const deltaPercentX = (deltaX / canvasRect.width) * 100;
        const deltaPercentY = (deltaY / canvasRect.height) * 100;

        const rawX = (initialX || 0) + deltaPercentX;
        const rawY = (initialY || 0) + deltaPercentY;

        // Apply Snapping
        const snappedX = snapValue(rawX);
        const snappedY = snapValue(rawY);

        onUpdateElement(slide.id, elementId, { position: { x: snappedX, y: snappedY } });
    } else if (mode === 'gesture' && e.touches.length === 2) {
        const dist = getDistance(e.touches[0], e.touches[1]);
        const angle = getAngle(e.touches[0], e.touches[1]);
        const { startDist, startAngle, startScale, startRotation } = touchRef.current;
        
        let newScale = (startScale || 1);
        let newRotation = (startRotation || 0);

        if (startDist && dist) {
             const scaleFactor = dist / startDist;
             newScale = Math.max(0.1, Math.min(10, (startScale || 1) * scaleFactor));
        }
        if (startAngle !== undefined) {
             const deltaAngle = angle - startAngle;
             newRotation = (startRotation || 0) + deltaAngle;
        }
        onUpdateElement(slide.id, elementId, { scale: newScale, rotation: newRotation });
    }
  };

  const handleTouchEnd = () => {
    touchRef.current = null;
    setIsDragging(false);
    window.removeEventListener('touchmove', handleTouchMove);
    window.removeEventListener('touchend', handleTouchEnd);
  };

  const handleTouchStart = (e: React.TouchEvent, element: SlideElement) => {
    if (isEditing || !slide) return;
    if (element.lock) { onSelectElement(element.id); return; }
    e.stopPropagation();
    onSelectElement(element.id);

    if (e.touches.length === 1) {
        setIsDragging(true);
        touchRef.current = { mode: 'drag', elementId: element.id, startX: e.touches[0].clientX, startY: e.touches[0].clientY, initialX: element.position.x, initialY: element.position.y };
    } else if (e.touches.length === 2) {
        setIsDragging(true);
        touchRef.current = { mode: 'gesture', elementId: element.id, startDist: getDistance(e.touches[0], e.touches[1]), startAngle: getAngle(e.touches[0], e.touches[1]), startScale: element.scale || 1, startRotation: element.rotation || 0 };
    }
    window.addEventListener('touchmove', handleTouchMove, { passive: false });
    window.addEventListener('touchend', handleTouchEnd);
  };

  React.useEffect(() => {
    return () => {
      window.removeEventListener('mousemove', handleMouseMove);
      window.removeEventListener('mouseup', handleMouseUp);
      window.removeEventListener('touchmove', handleTouchMove);
      window.removeEventListener('touchend', handleTouchEnd);
    };
  }, []);
  
  // -- RENDERING FRAME & OVERLAYS --
  const renderFrame = (type?: FrameType) => {
      if (!type || type === 'none') return null;
      const frameStyle: React.CSSProperties = { position: 'absolute', inset: 0, pointerEvents: 'none', zIndex: 40 };

      if (type.startsWith('browser')) {
          const isDark = type.includes('dark');
          return (
              <div style={frameStyle} className="flex flex-col">
                  <div className={`h-[5%] w-full ${isDark ? 'bg-[#1a1a1a]' : 'bg-[#e0e0e0]'} flex items-center px-2 space-x-1 border-b ${isDark ? 'border-white/10' : 'border-black/10'}`}>
                      <div className="w-1.5 h-1.5 rounded-full bg-red-500"></div>
                      <div className="w-1.5 h-1.5 rounded-full bg-yellow-500"></div>
                      <div className="w-1.5 h-1.5 rounded-full bg-green-500"></div>
                  </div>
                  <div className={`flex-1 border-l-4 border-r-4 border-b-4 ${isDark ? 'border-[#1a1a1a]' : 'border-[#e0e0e0]'}`}></div>
              </div>
          );
      }
      if (type === 'polaroid') return <div style={{...frameStyle, border: '5% solid white', borderBottom: '15% solid white', boxShadow: 'inset 0 0 20px rgba(0,0,0,0.1)' }}></div>;
      if (type === 'film') {
          return (
              <div style={frameStyle} className="flex justify-between px-[2%]">
                  <div className="w-[5%] h-full flex flex-col justify-between py-1 bg-black/80"></div>
                   <div className="w-[5%] h-full flex flex-col justify-between py-1 bg-black/80"></div>
              </div>
          );
      }
      if (type === 'paper') return <div style={{...frameStyle, border: '3% solid #fff', boxShadow: 'inset 0 0 40px rgba(0,0,0,0.1)' }}></div>;
      if (type === 'photo-booth') return <div style={{...frameStyle, border: '4% solid #111' }}></div>;
      return null;
  };

  // -- NO SIGNAL STATE --
  if (!slide && !isExportMode) {
    return (
      <div className="relative w-full h-full flex items-center justify-center bg-black/50 border border-white/10 aspect-[4/5]">
         <div className="text-xl font-black text-[#ccff00] animate-pulse tracking-widest">{TRANSLATIONS.NO_SIGNAL[language]}</div>
      </div>
    );
  }

  // If slide is missing in export mode, return empty
  if (!slide) return null;

  const textShadowOpacity = settings.shadowIntensity / 100;
  const textShadow = settings.shadowIntensity > 0 
    ? `0px ${2 + settings.shadowIntensity / 10}px ${4 + settings.shadowIntensity / 5}px rgba(0,0,0,${textShadowOpacity})`
    : 'none';

  const hexToRgb = (hex: string) => {
      const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
      return result ? `${parseInt(result[1], 16)}, ${parseInt(result[2], 16)}, ${parseInt(result[3], 16)}` : '0,0,0';
  };
  const overlayColorRgb = hexToRgb(settings.overlayColor);
  let overlayBackground = 'transparent';
  const op = settings.overlayOpacity / 100;

  if (settings.overlayMode === 'solid') {
       overlayBackground = `rgba(${overlayColorRgb}, ${op})`;
  } else if (settings.overlayMode === 'gradient') {
      overlayBackground = `linear-gradient(to top, rgba(${overlayColorRgb},${op}) 0%, rgba(${overlayColorRgb},${op * 0.95}) 20%, rgba(${overlayColorRgb},${op * 0.6}) ${settings.fadeHeight}%, rgba(${overlayColorRgb},0) 100%)`;
  }

  const renderStyledText = (rawText: string) => {
      const parts = rawText.split(/(<b>.*?<\/b>)/g);
      return parts.map((part, index) => {
          if (part.startsWith('<b>') && part.endsWith('</b>')) {
              return <span key={index} style={{ color: settings.highlightColor }}>{part.replace(/<\/?b>/g, '')}</span>;
          }
          return <span key={index}>{part}</span>;
      });
  };

  const isDataUrl = slide.imageUrl.startsWith('data:');

  // Main Container Style
  return (
    <div 
        className={isExportMode ? 'w-full h-full overflow-hidden' : 'w-full h-full flex items-center justify-center relative'}
        style={!isExportMode ? { padding: '20px' } : undefined}
    >
        {/* CANVAS PRESERVE RATIO CONTAINER */}
        <div 
          ref={canvasRef}
          className={`group relative flex-shrink-0 bg-black overflow-hidden ${!isExportMode ? 'shadow-2xl ring-1 ring-white/10' : ''}`}
          onMouseDown={() => onSelectElement(null)} 
          onTouchStart={() => onSelectElement(null)}
          style={{
              width: '100%',
              height: '100%',
              maxWidth: isExportMode ? 'none' : '100%',
              maxHeight: isExportMode ? 'none' : '100%',
              aspectRatio: `${ratioValues.w}/${ratioValues.h}`,
              borderRadius: isExportMode ? 0 : `${settings.cornerRadius}px`,
              marginLeft: 'auto',
              marginRight: 'auto'
          }}
        >
          {/* VISUAL GRID OVERLAY (Shows only when dragging) */}
          {isDragging && !isExportMode && (
              <div 
                className="absolute inset-0 z-30 pointer-events-none" 
                style={{
                    backgroundImage: `linear-gradient(to right, rgba(255,255,255,0.1) 1px, transparent 1px), linear-gradient(to bottom, rgba(255,255,255,0.1) 1px, transparent 1px)`,
                    backgroundSize: `${SNAP_STEP * 4}% ${SNAP_STEP * 4}%` // Show major grid lines
                }}
              ></div>
          )}

          {/* Spacer SVG for aspect ratio preservation in some flex contexts */}
          {!isExportMode && (
              <svg 
                viewBox={`0 0 ${ratioValues.w} ${ratioValues.h}`} 
                style={{ width: '100%', height: '100%', pointerEvents: 'none', opacity: 0, position: 'relative', zIndex: -1 }} 
              />
          )}

          {/* CONTENT LAYER */}
          <div className="absolute inset-0 w-full h-full overflow-hidden">
             {/* BACKGROUND */}
             <div 
                className="absolute inset-0 w-full h-full pointer-events-none"
                style={{ transform: `scale(${isExportMode ? 1 : 1})` }}
             >
                <img 
                    src={slide.imageUrl} 
                    alt="background" 
                    className="w-full h-full object-cover" 
                    crossOrigin={isDataUrl ? undefined : "anonymous"}
                />
                <div className="absolute inset-0" style={{ background: overlayBackground }} />
             </div>

             {/* ELEMENTS */}
             <div className="absolute inset-0 w-full h-full">
                 <div 
                    style={{
                        position: 'absolute',
                        top: '50%', left: '50%',
                        width: `${BASE_WIDTH}px`,
                        height: `${baseHeight}px`,
                        transform: `translate(-50%, -50%) scale(${scale})`,
                        transformOrigin: 'center center',
                    }}
                 >
                    {slide.elements.map((el) => {
                        const isActive = el.id === activeElementId;
                        const isTextType = el.type === 'text' || el.type === 'heading';
                        const baseFontSize = el.fontSize || (el.type === 'heading' ? settings.fontSize * 1.5 : settings.fontSize);

                        return (
                            <div
                                key={el.id}
                                onMouseDown={(e) => handleMouseDown(e, el)}
                                onTouchStart={(e) => handleTouchStart(e, el)}
                                onDoubleClick={(e) => handleElementDoubleClick(e, el)}
                                className={`absolute select-none group/element transition-opacity duration-200`}
                                style={{
                                    left: `${el.position.x}%`,
                                    top: `${el.position.y}%`,
                                    transform: `translate(-50%, -50%) scale(${el.scale}) rotate(${el.rotation}deg)`,
                                    zIndex: el.zIndex + 20,
                                    width: isTextType ? `${100 - settings.textPadding * 2}%` : 'auto',
                                    maxWidth: isTextType ? undefined : '100%',
                                    cursor: isEditing ? 'text' : (el.lock ? 'default' : 'grab'),
                                    opacity: el.opacity ?? 1,
                                    filter: el.filter || 'none'
                                }}
                            >
                                 {/* UX: Improved Selection Box */}
                                 {!isExportMode && isActive && (
                                     <div className="absolute -inset-2 border-2 border-[#ccff00] rounded-sm pointer-events-none z-50 shadow-[0_0_15px_rgba(204,255,0,0.3)]">
                                         {/* Corner Handles (Visual Only for now to imply scalability) */}
                                         <div className="absolute -top-1.5 -left-1.5 w-3 h-3 bg-white border border-[#ccff00] rounded-full"></div>
                                         <div className="absolute -top-1.5 -right-1.5 w-3 h-3 bg-white border border-[#ccff00] rounded-full"></div>
                                         <div className="absolute -bottom-1.5 -left-1.5 w-3 h-3 bg-white border border-[#ccff00] rounded-full"></div>
                                         <div className="absolute -bottom-1.5 -right-1.5 w-3 h-3 bg-white border border-[#ccff00] rounded-full"></div>
                                         
                                         {/* Top Label */}
                                         <div className="absolute -top-8 left-1/2 -translate-x-1/2 bg-[#ccff00] text-black text-[9px] font-bold px-2 py-0.5 rounded shadow-sm whitespace-nowrap opacity-0 group-hover/element:opacity-100 transition-opacity">
                                             {el.type.toUpperCase()}
                                         </div>
                                     </div>
                                 )}

                                 {isEditing && isActive && isTextType ? (
                                    <textarea
                                        ref={textareaRef}
                                        value={editText}
                                        onChange={handleTextChange}
                                        onBlur={handleTextBlur}
                                        className="w-full p-4 bg-black/60 backdrop-blur-sm border-2 border-[#ccff00] focus:outline-none resize-none overflow-hidden font-mono text-white rounded-lg shadow-xl"
                                        style={{ 
                                            fontSize: `${baseFontSize}px`,
                                            fontFamily: el.fontFamily || settings.fontFamily,
                                            lineHeight: settings.lineHeight,
                                            textAlign: el.textAlign || settings.textAlign,
                                            color: el.textColor || settings.textColor,
                                            textTransform: (el.isUppercase ?? settings.isUppercase) ? 'uppercase' : 'none',
                                            minHeight: '100px'
                                        }}
                                        autoFocus
                                    />
                                 ) : (
                                     isTextType ? (
                                        <div style={{
                                            fontSize: `${baseFontSize}px`,
                                            fontFamily: el.fontFamily || settings.fontFamily,
                                            lineHeight: settings.lineHeight,
                                            textAlign: el.textAlign || settings.textAlign,
                                            color: el.textColor || settings.textColor,
                                            textShadow,
                                            whiteSpace: 'pre-wrap', wordWrap: 'break-word',
                                            textTransform: (el.isUppercase ?? settings.isUppercase) ? 'uppercase' : 'none',
                                            padding: '16px',
                                            backgroundColor: settings.textBackgroundOpacity > 0 ? `rgba(${hexToRgb(settings.textBackgroundColor)}, ${settings.textBackgroundOpacity / 100})` : 'transparent',
                                        }}>
                                            {renderStyledText(el.content)}
                                        </div>
                                     ) : el.type === 'sticker' ? (
                                         <svg viewBox="0 0 100 100" width="150" height="150" style={{ overflow: 'visible' }}>
                                             <path d={el.content} stroke={settings.highlightColor} strokeWidth="4" fill="none" strokeLinecap="round" strokeLinejoin="round" vectorEffect="non-scaling-stroke"/>
                                         </svg>
                                     ) : (
                                        <img src={el.content} alt="Layer" className="pointer-events-none object-contain" style={{ width: el.width ? `${el.width}px` : '300px' }} />
                                     )
                                 )}
                            </div>
                        );
                    })}
                 </div>
             </div>

             {/* FRAMES & UI OVERLAYS */}
             {renderFrame(slide.frame)}

             {settings.brandingText && (
                <div className={`absolute z-50 text-[10px] font-black tracking-widest uppercase text-white/50 pointer-events-none ${settings.brandingPosition === 'top-left' ? 'top-6 left-6' : settings.brandingPosition === 'top-right' ? 'top-6 right-6' : settings.brandingPosition === 'bottom-left' ? 'bottom-6 left-6' : 'bottom-6 right-6'}`}>
                    {settings.brandingText}
                </div>
             )}
             {settings.showSwipeArrow && !isLastSlide && (
                <div className={`absolute ${settings.swipeIndicatorPosition === 'top' ? 'top-6' : 'bottom-6'} right-6 z-50 pointer-events-none flex items-center gap-2`}>
                <SwipeRightIcon className="w-6 h-6 animate-pulse" style={{ color: settings.textColor }} />
                </div>
             )}

             {/* SINGLE DOWNLOAD BUTTON OVERLAY */}
             {!isExportMode && onDownloadSingleSlide && slide && (
                <button 
                    onClick={(e) => { e.stopPropagation(); onDownloadSingleSlide(slide.id); }}
                    className="absolute top-2 right-2 z-50 p-2 bg-black/50 hover:bg-[#ccff00] text-white hover:text-black rounded-full opacity-0 group-hover:opacity-100 transition-all backdrop-blur-sm shadow-lg"
                    title="Download Slide as PNG"
                >
                    <DownloadIcon className="w-4 h-4" />
                </button>
             )}

          </div>
        </div>
    </div>
  );
};
