
import * as React from 'react';
import type { Slide, GlobalSettings, AspectRatio, Language, HistoryItem, SlideElement, ElementType, ReferenceType, FrameType } from '../types';
import { FONTS, ASPECT_RATIOS, ART_STYLES, INITIAL_STORY_PLACEHOLDER, DESIGN_PALETTES, TRANSLATIONS, FRAMES, STICKERS } from '../constants';
import { generateImage } from '../services/geminiService';
import { Loader } from './common/Loader';
import { TrashIcon } from './icons/TrashIcon';
import { UploadIcon } from './icons/UploadIcon';
import { AlignLeftIcon } from './icons/AlignLeftIcon';
import { AlignCenterIcon } from './icons/AlignCenterIcon';
import { AlignRightIcon } from './icons/AlignRightIcon';
import { PlusIcon } from './icons/PlusIcon';
import { RefreshIcon } from './icons/RefreshIcon';
import { DownloadIcon } from './icons/DownloadIcon';
import { CloseIcon } from './icons/CloseIcon';

interface ControlPanelProps {
  slides: Slide[];
  activeSlide: Slide | undefined;
  activeElement: SlideElement | undefined;
  settings: GlobalSettings;
  isLoading: boolean;
  loadingMessage: string;
  isDownloading: boolean;
  error: string | null;
  aspectRatio: AspectRatio;
  onAspectRatioChange: (ratio: AspectRatio) => void;
  onCreateCarousel: (story: string) => void;
  onAddSlide: () => void;
  onDeleteSlide: (id: string) => void;
  onSelectSlide: (id: string | null) => void;
  onUpdateSlide: (id: string, updatedProps: Partial<Slide>) => void;
  onUpdateElement: (slideId: string, elementId: string, updatedProps: Partial<SlideElement>) => void;
  onAddElement: (slideId: string, type: ElementType, content?: string) => void;
  onDeleteElement: (slideId: string, elementId: string) => void;
  onSettingsChange: (settings: GlobalSettings) => void;
  onDownloadCarousel: () => void;
  onDownloadPDF: () => void;
  onDownloadSingleSlide: (id: string) => void;
  onRestart: () => void;
  setError: (error: string | null) => void;
  onGenerateAllImages: () => void;
  onGenerateSingleImage: (slideId: string) => void;
  onMakeViral: (slideId: string) => void;
  language: Language;
  onLanguageChange: (lang: Language) => void;
  history: HistoryItem[];
  onLoadHistory: (item: HistoryItem) => void;
  onDeleteHistory: (id: string) => void;
  referenceImage: string | null;
  referenceType: ReferenceType;
  onReferenceImageChange: (image: string | null) => void;
  onReferenceTypeChange: (type: ReferenceType) => void;
}

type MainTool = 'project' | 'text' | 'media' | 'frames' | 'style' | 'tools';

const ToolbarButton = ({ tool, icon, label, isActive, onToggle }: { tool: MainTool, icon: React.ReactNode, label: string, isActive: boolean, onToggle: (t: MainTool) => void }) => (
    <button 
      onClick={() => onToggle(isActive ? 'project' : tool)}
      className={`group flex flex-col items-center justify-center gap-1.5 w-full h-full transition-all duration-200 relative ${isActive ? 'text-[#ccff00]' : 'text-white/40 hover:text-white'}`}
    >
        <div className={`transform transition-transform ${isActive ? 'scale-110' : 'group-hover:scale-105'}`}>
            {icon}
        </div>
        <span className="text-[9px] font-bold uppercase tracking-wider scale-75 lg:scale-100">{label}</span>
        {isActive && <div className="absolute top-0 left-1/2 -translate-x-1/2 w-8 h-[2px] bg-[#ccff00] shadow-[0_0_10px_#ccff00] rounded-full" />}
    </button>
);

const SubToolbar = ({ title, onClose, children }: { title: string, onClose: () => void, children?: React.ReactNode }) => (
    <div className="fixed lg:absolute bottom-[88px] lg:bottom-0 left-0 right-0 w-full bg-[#111]/95 backdrop-blur-xl lg:backdrop-blur-md border-t border-white/10 lg:border-none flex flex-col h-[50vh] lg:h-[450px] animate-slideUp z-50 shadow-[0_-10px_40px_rgba(0,0,0,0.8)] rounded-t-2xl lg:rounded-none">
        
        {/* Mobile Drag Handle */}
        <div className="lg:hidden w-full flex justify-center pt-3 pb-1" onClick={onClose}>
            <div className="w-12 h-1.5 bg-white/20 rounded-full" />
        </div>

        <div className="flex justify-between items-center px-5 py-3 border-b border-white/5 flex-none">
            <span className="text-xs font-black text-[#ccff00] uppercase tracking-widest flex items-center gap-2">
                <span className="w-2 h-2 bg-[#ccff00] rounded-full shadow-[0_0_5px_#ccff00]"></span>
                {title}
            </span>
            <button onClick={onClose} className="p-2 hover:bg-white/10 rounded-full transition-colors"><CloseIcon className="w-5 h-5 text-white/50 hover:text-white"/></button>
        </div>
        <div className="flex-1 overflow-y-auto p-5 custom-scrollbar bg-gradient-to-b from-[#111] to-[#050505]">
            {children}
        </div>
    </div>
);

const SliderControl = ({ label, value, min, max, step, onChange, displayValue }: { label: string, value: number, min: number, max: number, step: number, onChange: (val: number) => void, displayValue?: string }) => (
    <div className="mb-5 group select-none">
        <div className="flex justify-between mb-2 items-end">
            <span className="text-[10px] font-mono text-white/50 uppercase tracking-wide group-hover:text-white/80 transition-colors">{label}</span>
            <span className="text-[11px] font-mono text-[#ccff00] bg-[#ccff00]/10 px-2 py-0.5 rounded border border-[#ccff00]/20 min-w-[30px] text-center">
                {displayValue || value}
            </span>
        </div>
        <div className="relative h-8 flex items-center touch-none">
            <input 
                type="range" min={min} max={max} step={step} value={value} 
                onChange={(e) => onChange(parseFloat(e.target.value))}
                className="w-full relative z-10 opacity-0 cursor-pointer h-full"
            />
            <div className="absolute left-0 top-1/2 -translate-y-1/2 w-full h-1 bg-white/20 rounded-full overflow-hidden pointer-events-none">
                <div 
                    className="h-full bg-[#ccff00] shadow-[0_0_10px_rgba(204,255,0,0.5)]" 
                    style={{ width: `${((value - min) / (max - min)) * 100}%` }}
                ></div>
            </div>
            <div 
                className="absolute top-1/2 -translate-y-1/2 w-5 h-5 bg-black border-2 border-[#ccff00] rounded-full shadow-lg pointer-events-none transition-transform group-hover:scale-125 group-active:scale-110"
                style={{ left: `calc(${((value - min) / (max - min)) * 100}% - 10px)` }}
            ></div>
        </div>
    </div>
);

export const ControlPanel: React.FC<ControlPanelProps> = ({
  slides, activeSlide, activeElement, settings,
  isLoading, loadingMessage, isDownloading, error,
  aspectRatio, onAspectRatioChange, onCreateCarousel,
  onAddSlide, onDeleteSlide, onSelectSlide, onUpdateSlide,
  onUpdateElement, onAddElement, onDeleteElement, onSettingsChange,
  onDownloadCarousel, onDownloadPDF, onDownloadSingleSlide, onRestart, setError,
  onGenerateAllImages, onGenerateSingleImage, onMakeViral, language, onLanguageChange,
  history, onLoadHistory, onDeleteHistory,
  referenceImage, referenceType, onReferenceImageChange, onReferenceTypeChange
}) => {
  const [story, setStory] = React.useState<string>('');
  const [activeTool, setActiveTool] = React.useState<MainTool>('project');
  const [isHistoryOpen, setIsHistoryOpen] = React.useState(false);
  const mediaInputRef = React.useRef<HTMLInputElement>(null);
  const refImageInputRef = React.useRef<HTMLInputElement>(null);
  const [tempPrompt, setTempPrompt] = React.useState('');

  // UX: Auto-switch tools based on selection
  React.useEffect(() => {
    if (!activeElement) {
        if (activeTool !== 'project' && activeTool !== 'frames') {
            setActiveTool('project');
        }
        return;
    }
    
    if (activeElement.type === 'text' || activeElement.type === 'heading') {
        setActiveTool('text');
    } else if (activeElement.type === 'image' || activeElement.type === 'sticker') {
        setActiveTool('tools'); // Images usually need layering/tools first
    }
  }, [activeElement?.id, activeElement?.type]);

  React.useEffect(() => {
    if (slides.length === 0) setActiveTool('project');
  }, [slides.length]);

  React.useEffect(() => {
      if (activeSlide) setTempPrompt(activeSlide.imagePrompt || '');
  }, [activeSlide?.id]);

  const handleBridgeElement = () => {
      if (!activeSlide || !activeElement) return;
      const currentIndex = slides.findIndex(s => s.id === activeSlide.id);
      if (currentIndex === -1 || currentIndex === slides.length - 1) return;
      
      const nextSlide = slides[currentIndex + 1];
      let newX = activeElement.position.x - 100;
      if (activeElement.position.x < 20) newX = activeElement.position.x + 100; 
      
      // Preserve ID for syncing
      const elementCopy: SlideElement = {
          ...activeElement,
          position: { ...activeElement.position, x: newX },
          shared: { enabled: true, slideIds: [activeSlide.id, nextSlide.id] }
      };

      const nextSlideElements = [...nextSlide.elements, elementCopy];
      onUpdateSlide(nextSlide.id, { elements: nextSlideElements });
      
      onUpdateElement(activeSlide.id, activeElement.id, { shared: { enabled: true, slideIds: [activeSlide.id, nextSlide.id] } });
      onSelectSlide(nextSlide.id);
  };

  const handleUnlink = () => {
    if (!activeSlide || !activeElement) return;
    const newId = `${Date.now()}-unlinked`;
    const newElement = { ...activeElement, id: newId, shared: undefined };
    const newElements = activeSlide.elements.map(e => e.id === activeElement.id ? newElement : e);
    onUpdateSlide(activeSlide.id, { elements: newElements });
  };

  const handleLayer = (direction: 'up' | 'down') => {
      if (!activeSlide || !activeElement) return;
      const idx = activeSlide.elements.findIndex(e => e.id === activeElement.id);
      if (idx === -1) return;

      const newElements = [...activeSlide.elements];
      if (direction === 'up' && idx < newElements.length - 1) {
          [newElements[idx], newElements[idx+1]] = [newElements[idx+1], newElements[idx]];
      } else if (direction === 'down' && idx > 0) {
          [newElements[idx], newElements[idx-1]] = [newElements[idx-1], newElements[idx]];
      }
      newElements.forEach((el, i) => el.zIndex = i + 1);
      onUpdateSlide(activeSlide.id, { elements: newElements });
  };

  const handleMediaUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
      const file = e.target.files?.[0];
      if (!file || !activeSlide) return;
      const reader = new FileReader();
      reader.onload = (ev) => {
          if (ev.target?.result) onAddElement(activeSlide.id, 'image', ev.target.result as string);
      };
      reader.readAsDataURL(file);
      if(e.target) e.target.value = '';
      setActiveTool('project');
  };

  const handleRegenerateSlide = async () => {
      if (!activeSlide) return;
      try {
          onUpdateSlide(activeSlide.id, { imagePrompt: tempPrompt });
          alert("Prompt updated. Use RE-ROLL in Project tab to regenerate images.");
      } catch (e) { console.error(e); }
  };

  // --- INITIAL VIEW (INPUT) ---
  if (slides.length === 0) {
      return (
          <div className="h-full bg-[#080808] p-6 lg:p-8 flex flex-col overflow-y-auto border-l border-white/5 relative">
              <div className="flex justify-between mb-8 items-center">
                <h2 className="text-sm font-black tracking-widest text-[#ccff00]">{TRANSLATIONS.CONTROL_DECK[language]}</h2>
                <button onClick={() => onLanguageChange(language === 'en' ? 'ru' : 'en')} className="text-[10px] font-bold border border-white/20 px-3 py-1 rounded-full text-white hover:bg-white/10 hover:border-[#ccff00] transition-colors">
                    {language.toUpperCase()}
                </button>
              </div>

              <div className="space-y-6 mb-8">
                  <div className="border border-white/10 p-4 rounded-lg bg-white/5">
                       <label className="block text-[10px] font-mono text-white/50 mb-3 uppercase">{TRANSLATIONS.REF_IMAGE_LABEL[language]}</label>
                       {referenceImage ? (
                           <div className="relative group rounded-md overflow-hidden border border-white/20">
                               <img src={referenceImage} className="w-full h-32 object-cover opacity-80 group-hover:opacity-100 transition-opacity" />
                               <button onClick={() => onReferenceImageChange(null)} className="absolute top-2 right-2 bg-black/80 hover:bg-red-500 text-white p-2 rounded-full backdrop-blur-sm transition-colors">
                                   <TrashIcon className="w-4 h-4"/>
                               </button>
                           </div>
                       ) : (
                           <button onClick={() => refImageInputRef.current?.click()} className="w-full h-28 border border-dashed border-white/20 hover:border-[#ccff00] hover:bg-[#ccff00]/5 flex flex-col items-center justify-center gap-2 text-white/40 hover:text-[#ccff00] transition-all rounded-md">
                               <UploadIcon className="w-5 h-5 mb-1"/> 
                               <span className="text-[9px] font-bold tracking-wider">{TRANSLATIONS.UPLOAD_REF_PLACEHOLDER[language]}</span>
                           </button>
                       )}
                       <input ref={refImageInputRef} type="file" hidden accept="image/*" onChange={(e) => {
                           const f = e.target.files?.[0]; if(f) { const r = new FileReader(); r.onload=(ev)=>onReferenceImageChange(ev.target?.result as string); r.readAsDataURL(f); }
                       }} />
                  </div>
              </div>

              <div className="flex-1 flex flex-col gap-4">
                  <div className="relative flex-1 min-h-[150px]">
                      <textarea 
                        value={story}
                        onChange={(e) => setStory(e.target.value)}
                        placeholder={INITIAL_STORY_PLACEHOLDER[language]}
                        className="w-full h-full bg-[#111] border border-white/20 rounded-lg p-5 text-sm text-white focus:border-[#ccff00] focus:ring-1 focus:ring-[#ccff00] focus:outline-none resize-none font-mono custom-scrollbar leading-relaxed"
                      />
                      <div className="absolute bottom-4 right-4 text-[10px] text-white/20 font-mono">
                          {story.length} CHARS
                      </div>
                  </div>
                  <button 
                    onClick={() => { if(story) onCreateCarousel(story); else setError(TRANSLATIONS.DATA_REQUIRED[language]); }}
                    className="w-full bg-[#ccff00] text-black font-black py-5 rounded-lg uppercase tracking-widest hover:bg-white transition-all transform active:scale-95 shadow-[0_0_20px_rgba(204,255,0,0.3)] hover:shadow-[0_0_30px_rgba(204,255,0,0.5)] flex items-center justify-center gap-2"
                  >
                      {isLoading ? (
                          <>
                            <Loader />
                            <span>{loadingMessage}</span>
                          </>
                      ) : (
                          <>
                            <span>{TRANSLATIONS.INITIALIZE[language]}</span>
                            <span className="text-lg">→</span>
                          </>
                      )}
                  </button>
              </div>
          </div>
      );
  }

  return (
    <div className="h-full bg-[#080808] text-white flex flex-col relative border-l border-white/5 shadow-2xl">
        {isLoading && (
            <div className="absolute inset-0 bg-black/90 backdrop-blur-sm z-[60] flex flex-col items-center justify-center">
                 <Loader />
                 <p className="mt-4 text-[#ccff00] font-mono text-xs animate-pulse">{loadingMessage}</p>
            </div>
        )}

        {/* --- MAIN CONTENT AREA --- */}
        <div className="flex-1 relative overflow-hidden">
            
            {/* PROJECT (HOME) TOOL */}
            {activeTool === 'project' && !isHistoryOpen && (
                <div className="p-5 space-y-6 overflow-y-auto h-full pb-32 custom-scrollbar">
                     <div className="grid grid-cols-2 gap-3">
                        <button onClick={onAddSlide} className="bg-white/5 hover:bg-white/10 border border-white/10 hover:border-white rounded-lg p-4 flex flex-col items-center gap-2 transition-all group">
                             <div className="p-2 bg-white/5 rounded-full group-hover:bg-white/20 transition-colors">
                                 <PlusIcon className="w-5 h-5 text-white"/>
                             </div>
                             <span className="text-[9px] font-bold uppercase tracking-wider text-white/70 group-hover:text-white">{TRANSLATIONS.ADD_FRAME[language]}</span>
                        </button>
                        <button onClick={onDownloadCarousel} className="bg-[#ccff00]/5 hover:bg-[#ccff00] hover:text-black border border-[#ccff00]/30 hover:border-[#ccff00] rounded-lg p-4 flex flex-col items-center gap-2 transition-all group text-[#ccff00]">
                             <div className="p-2 bg-[#ccff00]/10 rounded-full group-hover:bg-black/10 transition-colors">
                                <DownloadIcon className="w-5 h-5"/>
                             </div>
                             <span className="text-[9px] font-bold uppercase tracking-wider">{TRANSLATIONS.EXPORT[language]} (.ZIP)</span>
                        </button>
                     </div>

                     {/* EXTRA EXPORT OPTIONS */}
                     <div className="grid grid-cols-2 gap-3">
                        <button onClick={onDownloadPDF} className="bg-white/5 hover:bg-white/10 hover:text-white border border-white/10 hover:border-white rounded-lg p-4 flex flex-col items-center gap-2 transition-all group text-white/50">
                             <span className="text-lg font-bold">PDF</span>
                             <span className="text-[9px] font-bold uppercase tracking-wider">{TRANSLATIONS.DOWNLOAD_PACK[language].replace('ПАК', 'PDF')}</span>
                        </button>
                        <button onClick={() => activeSlide && onDownloadSingleSlide(activeSlide.id)} className="bg-white/5 hover:bg-white/10 hover:text-white border border-white/10 hover:border-white rounded-lg p-4 flex flex-col items-center gap-2 transition-all group text-white/50">
                             <span className="text-lg font-bold">PNG</span>
                             <span className="text-[9px] font-bold uppercase tracking-wider">SAVE SLIDE</span>
                        </button>
                     </div>

                     {/* DELETE SLIDE BUTTON */}
                     <button
                         onClick={() => activeSlide && window.confirm(language === 'en' ? 'Delete this slide?' : 'Удалить этот слайд?') && onDeleteSlide(activeSlide.id)}
                         className="w-full py-3 bg-red-500/5 hover:bg-red-500 border border-red-500/20 hover:border-red-500 rounded-lg flex items-center justify-center gap-2 group transition-all"
                         disabled={!activeSlide}
                     >
                         <TrashIcon className="w-4 h-4 text-red-500 group-hover:text-white transition-colors" />
                         <span className="text-[10px] font-bold uppercase tracking-wider text-red-500/70 group-hover:text-white transition-colors">
                             {TRANSLATIONS.ACTION_DELETE[language]} SLIDE
                         </span>
                     </button>

                     {/* Aspect Ratio Switcher */}
                     <div>
                         <span className="text-[9px] font-mono text-white/30 uppercase block mb-2 ml-1">FORMAT</span>
                         <div className="flex gap-2 bg-black p-1 rounded-lg border border-white/10">
                             {ASPECT_RATIOS.map(ar => (
                                 <button key={ar.value} onClick={() => onAspectRatioChange(ar.value)} className={`flex-1 text-[9px] font-bold py-3 rounded-md transition-all ${aspectRatio === ar.value ? 'bg-[#ccff00] text-black shadow-lg' : 'text-white/40 hover:text-white hover:bg-white/5'}`}>
                                     {ar.name.split(' ')[0]}
                                 </button>
                             ))}
                         </div>
                     </div>

                     <div className="space-y-3 pt-2">
                         <button onClick={() => activeSlide ? onGenerateSingleImage(activeSlide.id) : onGenerateAllImages()} className="w-full py-4 bg-white/5 border border-white/10 hover:border-[#ccff00] hover:text-[#ccff00] rounded-lg text-xs font-bold uppercase flex items-center justify-center gap-3 transition-all">
                             <RefreshIcon className="w-4 h-4"/> {TRANSLATIONS.RE_ROLL[language]}
                         </button>
                         <button onClick={() => setIsHistoryOpen(true)} className="w-full py-4 bg-white/5 border border-white/10 hover:border-white hover:text-white rounded-lg text-xs font-bold uppercase flex items-center justify-center gap-3 transition-all text-white/70">
                             <span>📜</span> {TRANSLATIONS.SAVED_LOGS[language]}
                         </button>
                     </div>

                     <div className="pt-6 mt-6 border-t border-white/10">
                         <button onClick={onRestart} className="w-full py-3 text-red-500/50 hover:text-red-500 text-[10px] font-bold uppercase tracking-widest hover:bg-red-500/5 rounded transition-all">
                             {TRANSLATIONS.RESET_SYSTEM[language]}
                         </button>
                     </div>
                </div>
            )}

            {/* HISTORY SUB-VIEW */}
            {activeTool === 'project' && isHistoryOpen && (
                <div className="absolute inset-0 bg-[#080808] z-20 flex flex-col animate-fadeIn">
                    <div className="flex items-center justify-between p-5 border-b border-white/10 bg-[#111]">
                         <span className="text-xs font-black text-[#ccff00] tracking-wider">{TRANSLATIONS.SAVED_LOGS[language]}</span>
                         <button onClick={() => setIsHistoryOpen(false)}><CloseIcon className="w-5 h-5 text-white/50 hover:text-white"/></button>
                    </div>
                    <div className="flex-1 overflow-y-auto p-4 space-y-2 custom-scrollbar">
                        {history.length === 0 && <div className="text-white/30 text-center text-xs py-10 font-mono">NO ARCHIVED LOGS</div>}
                        {history.map(item => (
                            <div key={item.id} className="p-3 border border-white/10 bg-white/5 rounded-lg flex justify-between items-center group hover:border-white/30 transition-all">
                                <button onClick={() => onLoadHistory(item)} className="text-left flex-1">
                                    <div className="text-xs font-bold text-white group-hover:text-[#ccff00] truncate w-48 mb-1">{item.topic}</div>
                                    <div className="text-[9px] text-white/40 font-mono">{new Date(item.timestamp).toLocaleDateString()} • {item.slides.length} SLIDES</div>
                                </button>
                                <button onClick={() => onDeleteHistory(item.id)} className="p-2 text-white/20 hover:text-red-500 hover:bg-white/5 rounded-full transition-colors">
                                    <TrashIcon className="w-4 h-4" />
                                </button>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {/* TEXT TOOL */}
            {activeTool === 'text' && (
                <SubToolbar title={TRANSLATIONS.TOOL_TEXT[language]} onClose={() => setActiveTool('project')}>
                    <div className="grid grid-cols-2 gap-3 mb-6">
                        <button onClick={() => onAddElement(activeSlide!.id, 'heading')} className="p-3 bg-white/5 hover:bg-white/20 text-center font-bold uppercase text-xs border border-transparent hover:border-white/20 rounded-md transition-colors flex flex-col items-center gap-1">
                            <span className="text-lg leading-none">H1</span>
                            <span className="text-[9px] opacity-50">{TRANSLATIONS.ADD_HEADING[language]}</span>
                        </button>
                        <button onClick={() => onAddElement(activeSlide!.id, 'text')} className="p-3 bg-white/5 hover:bg-white/20 text-center font-bold uppercase text-xs border border-transparent hover:border-white/20 rounded-md transition-colors flex flex-col items-center gap-1">
                            <span className="text-lg leading-none font-light">Txt</span>
                            <span className="text-[9px] opacity-50">{TRANSLATIONS.ADD_TEXT[language]}</span>
                        </button>
                    </div>
                    
                    {activeElement && (activeElement.type === 'text' || activeElement.type === 'heading') && (
                        <div className="space-y-6 border-t border-white/10 pt-6">
                             {/* AI REWRITE BUTTON */}
                             <button 
                                onClick={() => activeSlide && onMakeViral(activeSlide.id)}
                                className="w-full py-3 bg-gradient-to-r from-purple-900/40 to-blue-900/40 border border-purple-500/30 rounded-lg text-purple-200 font-bold uppercase text-xs flex items-center justify-center gap-2 hover:border-purple-400 hover:shadow-[0_0_15px_rgba(168,85,247,0.2)] transition-all"
                             >
                                 ✨ {TRANSLATIONS.AI_REWRITE[language]}
                             </button>

                             <div className="space-y-2">
                                <div className="text-[9px] text-white/50 font-mono uppercase">TYPOGRAPHY</div>
                                <select 
                                    value={activeElement.fontFamily || settings.fontFamily} 
                                    onChange={(e) => onUpdateElement(activeSlide!.id, activeElement.id, { fontFamily: e.target.value })}
                                    className="w-full bg-[#111] border border-white/20 rounded p-2.5 text-xs text-white outline-none focus:border-[#ccff00]"
                                >
                                    {FONTS.map(f => <option key={f.name} value={f.value}>{f.name}</option>)}
                                </select>
                             </div>
                             
                             <div className="flex gap-2 p-1 bg-[#111] rounded border border-white/10">
                                <button onClick={() => onUpdateElement(activeSlide!.id, activeElement.id, { textAlign: 'left' })} className={`flex-1 p-2 rounded hover:bg-white/10 transition-colors ${activeElement.textAlign === 'left' || (!activeElement.textAlign && settings.textAlign === 'left') ? 'bg-white/20 text-white' : 'text-white/50'}`}><AlignLeftIcon className="w-4 h-4 mx-auto"/></button>
                                <button onClick={() => onUpdateElement(activeSlide!.id, activeElement.id, { textAlign: 'center' })} className={`flex-1 p-2 rounded hover:bg-white/10 transition-colors ${activeElement.textAlign === 'center' || (!activeElement.textAlign && settings.textAlign === 'center') ? 'bg-white/20 text-white' : 'text-white/50'}`}><AlignCenterIcon className="w-4 h-4 mx-auto"/></button>
                                <button onClick={() => onUpdateElement(activeSlide!.id, activeElement.id, { textAlign: 'right' })} className={`flex-1 p-2 rounded hover:bg-white/10 transition-colors ${activeElement.textAlign === 'right' || (!activeElement.textAlign && settings.textAlign === 'right') ? 'bg-white/20 text-white' : 'text-white/50'}`}><AlignRightIcon className="w-4 h-4 mx-auto"/></button>
                             </div>

                             <div>
                                <label className="text-[9px] text-white/50 font-mono uppercase block mb-2">COLOR</label>
                                <div className="flex gap-3 items-center">
                                    <div className="relative w-10 h-10 rounded-full overflow-hidden border border-white/30">
                                         <input type="color" className="absolute -top-2 -left-2 w-16 h-16 p-0 border-0 cursor-pointer" value={activeElement.textColor || settings.textColor} onChange={(e) => onUpdateElement(activeSlide!.id, activeElement.id, { textColor: e.target.value })} />
                                    </div>
                                    {/* Quick colors */}
                                    <div className="flex gap-2">
                                        <button onClick={() => onUpdateElement(activeSlide!.id, activeElement.id, { textColor: '#ffffff' })} className="w-10 h-10 rounded-full bg-white border border-white/20 hover:scale-110 transition-transform"/>
                                        <button onClick={() => onUpdateElement(activeSlide!.id, activeElement.id, { textColor: '#000000' })} className="w-10 h-10 rounded-full bg-black border border-white/20 hover:scale-110 transition-transform"/>
                                        <button onClick={() => onUpdateElement(activeSlide!.id, activeElement.id, { textColor: '#ccff00' })} className="w-10 h-10 rounded-full bg-[#ccff00] border border-white/20 hover:scale-110 transition-transform"/>
                                        <button onClick={() => onUpdateElement(activeSlide!.id, activeElement.id, { textColor: '#ff0055' })} className="w-10 h-10 rounded-full bg-[#ff0055] border border-white/20 hover:scale-110 transition-transform"/>
                                    </div>
                                </div>
                             </div>
                        </div>
                    )}
                </SubToolbar>
            )}

            {/* STYLE TOOL */}
            {activeTool === 'style' && (
                <SubToolbar title={TRANSLATIONS.TAB_STYLE[language]} onClose={() => setActiveTool('project')}>
                    <div className="space-y-8">
                         {/* PRESETS */}
                         <div>
                             <label className="text-[9px] text-white/50 font-mono uppercase block mb-3">GLOBAL THEMES</label>
                             <div className="flex gap-3 overflow-x-auto pb-4 custom-scrollbar">
                                 {DESIGN_PALETTES.map(p => (
                                     <button key={p.name} onClick={() => onSettingsChange({...settings, ...p as any})} className="flex-shrink-0 w-20 h-24 border border-white/10 hover:border-[#ccff00] bg-[#111] rounded-lg p-2 flex flex-col items-center justify-center gap-2 group transition-all">
                                         <div className="w-6 h-6 rounded-full shadow-lg" style={{ background: p.highlightColor, border: `2px solid ${p.textColor}` }}/>
                                         <span className="text-[8px] font-bold uppercase text-white/50 group-hover:text-white text-center leading-tight">{p.name}</span>
                                     </button>
                                 ))}
                             </div>
                         </div>

                         {/* GRANULAR CONTROLS */}
                         <div className="border-t border-white/10 pt-6">
                             <SliderControl label="OVERLAY OPACITY" value={settings.overlayOpacity} min={0} max={100} step={5} onChange={(v) => onSettingsChange({...settings, overlayOpacity: v})} displayValue={`${settings.overlayOpacity}%`} />
                             <SliderControl label="SHADOW INTENSITY" value={settings.shadowIntensity} min={0} max={100} step={5} onChange={(v) => onSettingsChange({...settings, shadowIntensity: v})} displayValue={`${settings.shadowIntensity}%`} />
                             <SliderControl label="CORNER RADIUS" value={settings.cornerRadius} min={0} max={60} step={1} onChange={(v) => onSettingsChange({...settings, cornerRadius: v})} displayValue={`${settings.cornerRadius}px`} />
                             <SliderControl label="TEXT PADDING" value={settings.textPadding} min={0} max={40} step={1} onChange={(v) => onSettingsChange({...settings, textPadding: v})} displayValue={`${settings.textPadding}px`} />
                             
                             <div className="mt-6 flex justify-between items-center bg-[#111] p-3 rounded-lg border border-white/10">
                                <label className="text-[9px] text-white/50 font-mono uppercase">ACCENT COLORS</label>
                                <div className="flex gap-3">
                                    <div className="flex flex-col items-center gap-1">
                                        <input type="color" className="w-10 h-10 rounded cursor-pointer bg-transparent" value={settings.highlightColor} onChange={(e) => onSettingsChange({...settings, highlightColor: e.target.value})} />
                                        <span className="text-[8px] text-white/30">HILITE</span>
                                    </div>
                                    <div className="flex flex-col items-center gap-1">
                                        <input type="color" className="w-10 h-10 rounded cursor-pointer bg-transparent" value={settings.overlayColor} onChange={(e) => onSettingsChange({...settings, overlayColor: e.target.value})} />
                                        <span className="text-[8px] text-white/30">BG</span>
                                    </div>
                                </div>
                             </div>
                         </div>
                    </div>
                </SubToolbar>
            )}

            {/* MEDIA TOOL */}
            {activeTool === 'media' && (
                <SubToolbar title={TRANSLATIONS.TOOL_MEDIA[language]} onClose={() => setActiveTool('project')}>
                     <div className="space-y-8">
                         <div className="p-6 border border-dashed border-white/20 hover:border-[#ccff00] hover:bg-[#ccff00]/5 transition-all cursor-pointer group rounded-lg" onClick={() => mediaInputRef.current?.click()}>
                             <div className="flex flex-col items-center gap-3 text-white/50 group-hover:text-[#ccff00]">
                                 <UploadIcon className="w-8 h-8"/>
                                 <span className="text-[10px] font-bold uppercase tracking-widest">UPLOAD IMAGE FROM DEVICE</span>
                             </div>
                             <input ref={mediaInputRef} type="file" hidden accept="image/*" onChange={handleMediaUpload} />
                         </div>
                         
                         <div>
                            <span className="text-[9px] font-mono text-white/50 uppercase block mb-3 border-b border-white/10 pb-1">{TRANSLATIONS.TOOL_STICKERS[language]}</span>
                            <div className="grid grid-cols-4 gap-3">
                                {STICKERS.map(sticker => (
                                    <button 
                                        key={sticker.name}
                                        onClick={() => activeSlide && onAddElement(activeSlide.id, 'sticker', sticker.content)}
                                        className="aspect-square bg-[#111] hover:bg-white/10 flex items-center justify-center p-3 border border-white/5 hover:border-white/30 rounded-lg transition-all"
                                    >
                                        <svg viewBox="0 0 100 100" className="w-full h-full text-white">
                                            <path d={sticker.content} fill="none" stroke="currentColor" strokeWidth="5" />
                                        </svg>
                                    </button>
                                ))}
                            </div>
                         </div>

                         {activeSlide && (
                             <div className="border-t border-white/10 pt-4 bg-[#111] p-3 rounded-lg">
                                 <span className="text-[9px] font-mono text-[#ccff00] uppercase block mb-2">AI BACKGROUND PROMPT</span>
                                 <textarea 
                                     value={tempPrompt}
                                     onChange={(e) => setTempPrompt(e.target.value)}
                                     onBlur={handleRegenerateSlide}
                                     className="w-full h-20 bg-black border border-white/20 rounded p-3 text-xs text-white font-mono focus:border-[#ccff00] focus:outline-none focus:ring-1 focus:ring-[#ccff00]"
                                 />
                                 <div className="text-[9px] text-white/30 mt-2 flex justify-between">
                                    <span>Edit prompt & click outside to save.</span>
                                    <span>Use RE-ROLL to apply.</span>
                                 </div>
                             </div>
                         )}
                     </div>
                </SubToolbar>
            )}

            {/* FRAMES TOOL */}
            {activeTool === 'frames' && (
                <SubToolbar title={TRANSLATIONS.TOOL_FRAMES[language]} onClose={() => setActiveTool('project')}>
                    <div className="grid grid-cols-2 lg:grid-cols-2 gap-3">
                        {FRAMES.map(frame => (
                            <button 
                                key={frame.id}
                                onClick={() => activeSlide && onUpdateSlide(activeSlide.id, { frame: frame.id })}
                                className={`p-4 border rounded-lg transition-all relative overflow-hidden group ${activeSlide?.frame === frame.id ? 'border-[#ccff00] bg-[#ccff00]/10 text-[#ccff00]' : 'border-white/10 text-white/50 hover:border-white hover:text-white'}`}
                            >
                                <div className="text-[10px] font-bold uppercase relative z-10">{frame.name}</div>
                                {activeSlide?.frame === frame.id && <div className="absolute top-1 right-1 w-2 h-2 bg-[#ccff00] rounded-full shadow-[0_0_5px_#ccff00]"></div>}
                            </button>
                        ))}
                    </div>
                </SubToolbar>
            )}

            {/* TOOLS (PROPERTIES) */}
            {activeTool === 'tools' && (
                <SubToolbar title={TRANSLATIONS.TOOL_TOOLS[language]} onClose={() => setActiveTool('project')}>
                    {!activeElement ? (
                         <div className="text-center text-white/30 text-xs py-12 uppercase font-mono border border-dashed border-white/10 rounded-lg">
                             SELECT AN ELEMENT ON CANVAS
                         </div>
                    ) : (
                        <div className="space-y-6">
                            <div className="grid grid-cols-2 gap-3">
                                <button onClick={() => activeElement.shared?.enabled ? handleUnlink() : handleBridgeElement()} className={`p-4 border rounded-lg uppercase text-[10px] font-bold transition-all shadow-lg ${activeElement.shared?.enabled ? 'bg-[#ccff00] text-black border-[#ccff00] hover:bg-[#b3e600]' : 'bg-[#111] border-[#ccff00]/50 text-[#ccff00] hover:bg-[#ccff00]/10'}`}>
                                    {activeElement.shared?.enabled ? TRANSLATIONS.ACTION_UNLINK[language] : TRANSLATIONS.ACTION_BRIDGE[language]}
                                </button>
                                <button onClick={() => activeSlide && onDeleteElement(activeSlide.id, activeElement.id)} className="p-4 bg-red-500/10 border border-red-500/30 rounded-lg text-red-500 hover:bg-red-500 hover:text-white uppercase text-[10px] font-bold transition-all">
                                    {TRANSLATIONS.ACTION_DELETE[language]}
                                </button>
                                <button onClick={() => onUpdateElement(activeSlide!.id, activeElement.id, { lock: !activeElement.lock })} className={`p-3 border rounded-lg uppercase text-[10px] font-bold transition-all ${activeElement.lock ? 'bg-white text-black' : 'bg-white/5 border-white/10 hover:border-white hover:bg-white/10'}`}>
                                    {TRANSLATIONS.ACTION_LOCK[language]} {activeElement.lock ? '(ON)' : ''}
                                </button>
                                <div className="flex gap-1">
                                    <button onClick={() => handleLayer('up')} className="flex-1 p-3 bg-white/5 border border-white/10 rounded-l-lg hover:border-white uppercase text-[10px] font-bold">UP</button>
                                    <button onClick={() => handleLayer('down')} className="flex-1 p-3 bg-white/5 border border-white/10 rounded-r-lg hover:border-white uppercase text-[10px] font-bold">DOWN</button>
                                </div>
                            </div>

                            <div className="border-t border-white/10 pt-6">
                                <SliderControl label="ELEMENT OPACITY" value={activeElement.opacity ?? 1} min={0} max={1} step={0.05} onChange={(v) => onUpdateElement(activeSlide!.id, activeElement.id, { opacity: v })} displayValue={`${Math.round((activeElement.opacity ?? 1)*100)}%`} />
                            </div>

                             <div>
                                 <label className="text-[9px] text-white/50 font-mono uppercase block mb-3">VISUAL FILTER</label>
                                 <div className="grid grid-cols-3 gap-2">
                                     {['none', 'grayscale(100%)', 'sepia(100%)', 'blur(4px)', 'contrast(150%)', 'brightness(150%)'].map(f => (
                                         <button 
                                            key={f}
                                            onClick={() => onUpdateElement(activeSlide!.id, activeElement.id, { filter: f })}
                                            className={`p-2 border rounded text-[9px] uppercase transition-all ${activeElement.filter === f ? 'border-[#ccff00] text-[#ccff00] bg-[#ccff00]/10' : 'border-white/10 text-white/50 hover:border-white/30'}`}
                                         >
                                             {f.split('(')[0]}
                                         </button>
                                     ))}
                                 </div>
                             </div>
                        </div>
                    )}
                </SubToolbar>
            )}
        </div>

        {/* --- BOTTOM NAVIGATION BAR --- */}
        <div className="h-[88px] lg:h-[96px] border-t border-white/10 bg-black flex items-center justify-between px-2 z-50 shadow-[0_-5px_20px_rgba(0,0,0,0.5)] pb-[env(safe-area-inset-bottom)]">
            <ToolbarButton tool="project" icon={<div className="text-xl">☗</div>} label="PROJ" isActive={activeTool === 'project'} onToggle={setActiveTool} />
            <ToolbarButton tool="text" icon={<div className="text-xl font-serif italic">Aa</div>} label="TEXT" isActive={activeTool === 'text'} onToggle={setActiveTool} />
            <ToolbarButton tool="media" icon={<div className="text-xl">🖼</div>} label="MEDIA" isActive={activeTool === 'media'} onToggle={setActiveTool} />
            <ToolbarButton tool="style" icon={<div className="text-xl">🎨</div>} label="STYLE" isActive={activeTool === 'style'} onToggle={setActiveTool} />
            <ToolbarButton tool="frames" icon={<div className="w-5 h-5 border-2 border-current rounded-sm"/>} label="FRAME" isActive={activeTool === 'frames'} onToggle={setActiveTool} />
            <ToolbarButton tool="tools" icon={<div className="text-xl">⚙</div>} label="TOOLS" isActive={activeTool === 'tools'} onToggle={setActiveTool} />
        </div>
    </div>
  );
};
