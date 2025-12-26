
import * as React from 'react';
import { ControlPanel } from './components/ControlPanel';
import { Canvas } from './components/Canvas';
import type { Slide, GlobalSettings, AspectRatio, Language, HistoryItem, SlideElement, ElementType, ReferenceType } from './types';
import { ASPECT_RATIOS, ART_STYLES, TRANSLATIONS } from './constants';
import { RefreshIcon } from './components/icons/RefreshIcon';
import { MenuIcon } from './components/icons/MenuIcon';
import { CloseIcon } from './components/icons/CloseIcon';
import { PlusIcon } from './components/icons/PlusIcon';
import { parseStoryToSlides, generateImage, makeTextViral } from './services/geminiService';

// TypeScript declarations for libraries loaded via CDN
declare var htmlToImage: any;
declare var JSZip: any;
declare var jspdf: any;

const initialSettings: GlobalSettings = {
  fontFamily: '"Russo One", sans-serif',
  textAlign: 'left',
  fontSize: 26, 
  lineHeight: 1.1,
  textPadding: 8,
  isUppercase: true,
  textColor: '#FFFFFF',
  highlightColor: '#ccff00', 
  textBackgroundColor: '#000000',
  textBackgroundOpacity: 0,
  overlayMode: 'gradient',
  overlayColor: '#000000',
  overlayOpacity: 85,
  fadeHeight: 80,
  cornerRadius: 0,
  shadowIntensity: 60,
  showSwipeArrow: true,
  swipeIndicatorPosition: 'bottom',
  artStyle: ART_STYLES[0].value,
  brandingText: '',
  brandingPosition: 'top-left'
};

const getPlaceholderUrl = (index: number) => `https://placehold.co/1080x1350/050505/333?text=SLIDE+${index + 1}`;
const HISTORY_KEY = 'ai_carousel_history';

const App: React.FC = () => {
  const [slides, setSlides] = React.useState<Slide[]>([]);
  const [activeSlideId, setActiveSlideId] = React.useState<string | null>(null);
  const [activeElementId, setActiveElementId] = React.useState<string | null>(null);
  const [settings, setSettings] = React.useState<GlobalSettings>(initialSettings);
  const [isLoading, setIsLoading] = React.useState<boolean>(false);
  const [loadingMessage, setLoadingMessage] = React.useState<string>("");
  const [isDownloading, setIsDownloading] = React.useState<boolean>(false);
  const [error, setError] = React.useState<string | null>(null);
  const [aspectRatio, setAspectRatio] = React.useState<AspectRatio>('4:5');
  const [language, setLanguage] = React.useState<Language>('ru');
  const [history, setHistory] = React.useState<HistoryItem[]>([]);
  // Use simplified state for settings open/close if needed, but new bottom bar handles it well
  
  const [referenceImage, setReferenceImage] = React.useState<string | null>(null);
  const [referenceType, setReferenceType] = React.useState<ReferenceType>('product');

  const mobileScrollRef = React.useRef<HTMLDivElement>(null);
  const desktopScrollRef = React.useRef<HTMLDivElement>(null);

  const activeSlide = React.useMemo(() => 
    slides.find(s => s.id === activeSlideId) || (slides.length > 0 ? slides[0] : undefined), 
    [slides, activeSlideId]
  );
  
  const activeElement = React.useMemo(() => {
    if (!activeSlide || !activeElementId) return undefined;
    return activeSlide.elements.find(e => e.id === activeElementId);
  }, [activeSlide, activeElementId]);

  const downloadDimensions = React.useMemo(() => {
    const width = 1080;
    let height = 1080;
    if (aspectRatio === '4:5') height = 1350; 
    if (aspectRatio === '9:16') height = 1920; 
    return { width, height };
  }, [aspectRatio]);

  React.useEffect(() => {
      try {
          const stored = localStorage.getItem(HISTORY_KEY);
          if (stored) setHistory(JSON.parse(stored));
      } catch (e) { console.error("Failed to load history", e); }
  }, []);

  // Auto-scroll to active slide in Desktop view
  React.useEffect(() => {
      if (activeSlideId && desktopScrollRef.current) {
          const slideEl = document.getElementById(`desktop-slide-${activeSlideId}`);
          if (slideEl) {
              slideEl.scrollIntoView({ behavior: 'smooth', block: 'center', inline: 'center' });
          }
      }
  }, [activeSlideId]);

  const saveToHistory = React.useCallback((newSlides: Slide[], newSettings: GlobalSettings, newAspectRatio: AspectRatio, topic: string) => {
      const newItem: HistoryItem = {
          id: Date.now().toString(),
          timestamp: Date.now(),
          topic: topic.slice(0, 50) + (topic.length > 50 ? '...' : ''),
          slides: newSlides,
          settings: newSettings,
          aspectRatio: newAspectRatio
      };
      setHistory(prev => {
          const trySave = (list: HistoryItem[]): HistoryItem[] => {
              try {
                  localStorage.setItem(HISTORY_KEY, JSON.stringify(list));
                  return list;
              } catch (e: any) {
                  if (list.length > 1) { return trySave(list.slice(0, -1)); }
                  return prev; 
              }
          };
          return trySave([newItem, ...prev].slice(0, 3));
      });
  }, []);

  const loadFromHistory = React.useCallback((item: HistoryItem) => {
      if (window.confirm("Load this record?")) {
          setSlides(item.slides);
          setSettings(item.settings);
          setAspectRatio(item.aspectRatio);
          setActiveSlideId(item.slides[0]?.id || null);
          setActiveElementId(item.slides[0]?.elements[0]?.id || null);
      }
  }, []);

  const deleteFromHistory = React.useCallback((id: string) => {
      setHistory(prev => {
          const updated = prev.filter(i => i.id !== id);
          localStorage.setItem(HISTORY_KEY, JSON.stringify(updated));
          return updated;
      });
  }, []);

  React.useEffect(() => {
    const container = mobileScrollRef.current;
    if (!container || slides.length === 0) return;
    const observer = new IntersectionObserver((entries) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            const slideId = entry.target.getAttribute('data-slide-id');
            if (slideId) setActiveSlideId(slideId);
          }
        });
    }, { root: container, threshold: 0.6 });
    const elements = container.querySelectorAll('.mobile-slide-item');
    elements.forEach(el => observer.observe(el));
    return () => observer.disconnect();
  }, [slides.length, aspectRatio]);

  const handleCreateCarousel = React.useCallback(async (story: string) => {
    setError(null);
    setIsLoading(true);
    setLoadingMessage(TRANSLATIONS.INIT_NEURAL[language]);

    try {
        const parsedSlides = await parseStoryToSlides(story, language);
        
        const newSlides: Slide[] = parsedSlides.map((s, index) => {
            const elementId = `${Date.now()}-${index}-text`;
            return {
                id: `${Date.now()}-${index}`,
                elements: [{
                    id: elementId,
                    type: 'text',
                    content: s.text,
                    position: { x: 50, y: 60 },
                    scale: 1,
                    rotation: 0,
                    zIndex: 1
                }],
                imageUrl: getPlaceholderUrl(index),
                imagePrompt: s.imagePrompt
            };
        });

        setSlides(newSlides);
        if (newSlides.length > 0) {
             setActiveSlideId(newSlides[0].id);
             if (newSlides[0].elements.length > 0) {
                 setActiveElementId(newSlides[0].elements[0].id);
             }
        }

        saveToHistory(newSlides, settings, aspectRatio, story);

        const updatedSlides = [...newSlides];
        for (let i = 0; i < newSlides.length; i++) {
            setLoadingMessage(`${TRANSLATIONS.RENDERING[language]} [${i + 1}/${newSlides.length}]...`);
            try {
                const imageUrl = await generateImage(newSlides[i].imagePrompt, settings.artStyle, referenceImage || undefined, referenceType);
                updatedSlides[i] = { ...updatedSlides[i], imageUrl };
                setSlides(prev => prev.map(s => s.id === updatedSlides[i].id ? { ...s, imageUrl } : s));
            } catch (e) { console.error(`Failed to generate image for slide ${i}`, e); }
        }
        saveToHistory(updatedSlides, settings, aspectRatio, story);
    } catch (err: any) {
        setError(TRANSLATIONS.SYSTEM_FAILURE[language]);
        console.error(err);
    } finally { setIsLoading(false); setLoadingMessage(""); }
  }, [settings.artStyle, language, aspectRatio, settings, saveToHistory, referenceImage, referenceType]);

  const handleGenerateAllImages = React.useCallback(async () => {
      if (slides.length === 0) return;
      setIsLoading(true);
      setError(null);
      let currentSlides = [...slides];
      try {
          for (let i = 0; i < currentSlides.length; i++) {
              setLoadingMessage(`${TRANSLATIONS.BATCH_PROCESS[language]} [${i + 1}/${currentSlides.length}]...`);
              const slide = currentSlides[i];
              const promptToUse = slide.imagePrompt || "Abstract background";
              try {
                const imageUrl = await generateImage(promptToUse, settings.artStyle, referenceImage || undefined, referenceType);
                const updatedSlide = { ...slide, imageUrl };
                currentSlides[i] = updatedSlide;
                setSlides([...currentSlides]);
              } catch (e) { console.error(e); }
          }
          saveToHistory(currentSlides, settings, aspectRatio, "Batch Generated");
      } catch (err) { setError(TRANSLATIONS.BATCH_FAIL[language]); } finally { setIsLoading(false); setLoadingMessage(""); }
  }, [slides, settings.artStyle, language, aspectRatio, settings, saveToHistory, referenceImage, referenceType]);

  const handleGenerateSingleImage = React.useCallback(async (slideId: string) => {
      const slide = slides.find(s => s.id === slideId);
      if (!slide) return;
      
      setIsLoading(true);
      setError(null);
      setLoadingMessage(TRANSLATIONS.RENDERING[language]);

      try {
        const imageUrl = await generateImage(slide.imagePrompt || "Abstract background", settings.artStyle, referenceImage || undefined, referenceType);
        
        const updatedSlides = slides.map(s => s.id === slideId ? { ...s, imageUrl } : s);
        setSlides(updatedSlides);
        saveToHistory(updatedSlides, settings, aspectRatio, "Single Slide Re-roll");

      } catch (err: any) { 
          console.error(err); 
          setError(TRANSLATIONS.SYSTEM_FAILURE[language]); 
      } finally { 
          setIsLoading(false); 
          setLoadingMessage(""); 
      }
  }, [slides, settings, aspectRatio, language, referenceImage, referenceType, saveToHistory]);

  const handleMakeViral = React.useCallback(async (slideId: string) => {
      const slideIndex = slides.findIndex(s => s.id === slideId);
      if (slideIndex === -1) return;
      setIsLoading(true);
      setLoadingMessage(TRANSLATIONS.OPTIMIZING[language]);
      try {
          const slide = slides[slideIndex];
          const textElement = slide.elements.find(e => e.type === 'heading' || e.type === 'text');
          if (!textElement) return;

          let type: 'hook' | 'cta' | 'body' = 'body';
          if (slideIndex === 0) type = 'hook';
          else if (slideIndex === slides.length - 1) type = 'cta';
          
          const newText = await makeTextViral(textElement.content, type, language);
          setSlides(prev => prev.map(s => s.id === slideId ? { ...s, elements: s.elements.map(e => e.id === textElement.id ? { ...e, content: newText } : e) } : s));
      } catch (err) { setError(TRANSLATIONS.OPT_ERROR[language]); } finally { setIsLoading(false); setLoadingMessage(""); }
  }, [slides, language]);
  
  const handleUpdateSlide = React.useCallback((id: string, updatedProps: Partial<Slide>) => {
    setSlides(prev => prev.map(s => s.id === id ? { ...s, ...updatedProps } : s));
  }, []);

  const handleUpdateElement = React.useCallback((slideId: string, elementId: string, updatedProps: Partial<SlideElement>) => {
      setSlides(prev => {
          const currentSlide = prev.find(s => s.id === slideId);
          const currentElement = currentSlide?.elements.find(e => e.id === elementId);
          let slidesToUpdate = [slideId];
          if (currentElement?.shared?.enabled && currentElement.shared.slideIds) {
              slidesToUpdate = currentElement.shared.slideIds;
          }
          return prev.map(s => {
              if (slidesToUpdate.includes(s.id)) {
                  return { ...s, elements: s.elements.map(e => e.id === elementId ? { ...e, ...updatedProps } : e) };
              }
              return s;
          });
      });
  }, []);

  const handleAddElement = React.useCallback((slideId: string, type: ElementType, content?: string) => {
      setSlides(prev => prev.map(s => {
          if (s.id !== slideId) return s;
          const newElement: SlideElement = {
              id: `${Date.now()}-${type}`,
              type,
              content: content || (type === 'heading' ? TRANSLATIONS.DEFAULT_HEADING[language] : TRANSLATIONS.DEFAULT_TEXT[language]),
              position: { x: 50, y: 60 },
              scale: type === 'heading' ? 1.5 : 1,
              rotation: 0,
              zIndex: s.elements.length + 1
          };
          setActiveElementId(newElement.id);
          return { ...s, elements: [...s.elements, newElement] };
      }));
  }, [language]);

  const handleDeleteElement = React.useCallback((slideId: string, elementId: string) => {
      setSlides(prev => prev.map(s => {
          if (s.id !== slideId) return s;
          const newElements = s.elements.filter(e => e.id !== elementId);
          if (activeElementId === elementId) {
              setActiveElementId(newElements[0]?.id || null);
          }
          return { ...s, elements: newElements };
      }));
  }, [activeElementId]);

  const handleAddSlide = React.useCallback(() => {
    if (slides.length >= 10) return;
    const index = slides.length;
    const newTextId = `${Date.now()}-new-text`;
    const newSlide: Slide = {
        id: `${Date.now()}-new`,
        imageUrl: getPlaceholderUrl(index),
        imagePrompt: 'Abstract dark aesthetic',
        elements: [{
            id: newTextId,
            type: 'text',
            content: TRANSLATIONS.NEW_BLOCK[language],
            position: { x: 50, y: 60 },
            scale: 1,
            rotation: 0,
            zIndex: 1
        }]
    };
    setSlides(prev => [...prev, newSlide]);
    setActiveSlideId(newSlide.id);
    setActiveElementId(newTextId);
  }, [slides.length, language]);

  const handleDeleteSlide = React.useCallback((id: string) => {
    setSlides(prev => {
      const slideIndex = prev.findIndex(s => s.id === id);
      if (slideIndex === -1) return prev;
      const newSlides = prev.filter(s => s.id !== id);
      if (activeSlideId === id) {
        if (newSlides.length === 0) {
            setActiveSlideId(null);
            setActiveElementId(null);
        } else {
            const nextSlide = newSlides[Math.max(0, slideIndex - 1)];
            setActiveSlideId(nextSlide.id);
            setActiveElementId(nextSlide.elements[0]?.id || null);
        }
      }
      return newSlides;
    });
  }, [activeSlideId]);

  const aspectRatioClassName = React.useMemo(() => {
    return ASPECT_RATIOS.find(ar => ar.value === aspectRatio)?.className || 'aspect-[4/5]';
  }, [aspectRatio]);

  const handleDownloadCarousel = React.useCallback(async () => {
    if (!slides.length) return;
    setIsDownloading(true);
    setLoadingMessage(TRANSLATIONS.COMPRESSING[language]);
    setError(null);
    const zip = new JSZip();
    try {
      await new Promise(r => setTimeout(r, 1000));
      for (let i = 0; i < slides.length; i++) {
        const slide = slides[i];
        const node = document.getElementById(`download-container-${slide.id}`);
        if (node) {
          const blob = await htmlToImage.toBlob(node, { 
            quality: 1.0, pixelRatio: 2, 
            width: downloadDimensions.width, height: downloadDimensions.height,
            backgroundColor: settings.overlayMode === 'solid' ? settings.overlayColor : undefined,
            style: { transform: 'none' }
           });
          if (blob) zip.file(`${i + 1}_slide.png`, blob);
        }
      }
      const zipBlob = await zip.generateAsync({ type: 'blob' });
      const link = document.createElement('a');
      link.href = URL.createObjectURL(zipBlob);
      link.download = 'VIRAL_PACK.zip';
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      URL.revokeObjectURL(link.href);
    } catch (err: any) { console.error(err); setError(TRANSLATIONS.EXPORT_FAIL[language]); } finally { setIsDownloading(false); setLoadingMessage(""); }
  }, [slides, settings, downloadDimensions, language]);

  const handleDownloadPDF = React.useCallback(async () => {
    if (!slides.length) return;
    setIsDownloading(true);
    setLoadingMessage(TRANSLATIONS.GENERATING_DOC[language]);
    setError(null);
    try {
      const { jsPDF } = jspdf;
      const pdf = new jsPDF({
        orientation: downloadDimensions.height > downloadDimensions.width ? 'p' : 'l',
        unit: 'px',
        format: [downloadDimensions.width, downloadDimensions.height]
      });
      await new Promise(r => setTimeout(r, 1000));
      for (let i = 0; i < slides.length; i++) {
        const slide = slides[i];
        const node = document.getElementById(`download-container-${slide.id}`);
        if (node) {
          const dataUrl = await htmlToImage.toPng(node, { 
            quality: 1.0, pixelRatio: 2, 
            width: downloadDimensions.width, height: downloadDimensions.height,
            backgroundColor: settings.overlayMode === 'solid' ? settings.overlayColor : undefined,
            style: { transform: 'none' }
          });
          if (i > 0) pdf.addPage([downloadDimensions.width, downloadDimensions.height]);
          pdf.addImage(dataUrl, 'PNG', 0, 0, downloadDimensions.width, downloadDimensions.height);
        }
      }
      pdf.save('VIRAL_CAROUSEL.pdf');
    } catch (err: any) { console.error(err); setError(TRANSLATIONS.EXPORT_FAIL[language]); } finally { setIsDownloading(false); setLoadingMessage(""); }
  }, [slides, settings, downloadDimensions, language]);

  const handleDownloadSingleSlide = React.useCallback(async (slideId: string) => {
    setIsDownloading(true);
    try {
      const node = document.getElementById(`download-container-${slideId}`);
      if (node) {
        const blob = await htmlToImage.toBlob(node, { quality: 1.0, pixelRatio: 2, width: downloadDimensions.width, height: downloadDimensions.height });
        if (blob) {
          const link = document.createElement('a');
          link.href = URL.createObjectURL(blob);
          link.download = `SLIDE_${slideId}.png`;
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
        }
      }
    } catch (err) { setError(TRANSLATIONS.EXPORT_FAIL[language]); } finally { setIsDownloading(false); }
  }, [downloadDimensions, language]);

  const handleRestart = React.useCallback(() => {
    if (window.confirm(TRANSLATIONS.RESET_SYSTEM[language])) {
        setSlides([]); setActiveSlideId(null); setActiveElementId(null); setSettings(initialSettings);
        setReferenceImage(null); setReferenceType('product');
    }
  }, [language]);

  return (
    <div className="h-[100dvh] w-full bg-[#050505] text-white font-sans flex flex-col lg:flex-row overflow-hidden relative">
        <div className="absolute inset-0 z-0 opacity-20 pointer-events-none" style={{ backgroundImage: 'linear-gradient(#222 1px, transparent 1px), linear-gradient(90deg, #222 1px, transparent 1px)', backgroundSize: '50px 50px' }}></div>
        
        {/* MOBILE HEADER */}
        <header className="lg:hidden flex-none flex justify-between items-center px-4 py-3 border-b border-white/10 bg-black/80 backdrop-blur z-20">
            <h1 className="text-lg font-black tracking-widest text-[#ccff00]">AI<span className="text-white">CAROUSEL</span></h1>
            <div className="flex items-center gap-3">
                 <button onClick={() => setLanguage(l => l === 'en' ? 'ru' : 'en')} className="text-[10px] font-mono border border-white/20 px-2 py-1 text-white hover:text-[#ccff00] hover:border-[#ccff00]">{language === 'en' ? 'EN' : 'RU'}</button>
                <button onClick={handleRestart} className="p-2 text-white/50 hover:text-[#ccff00]"><RefreshIcon className="w-5 h-5" /></button>
            </div>
        </header>

        <main className="flex-1 relative flex flex-col items-center justify-center h-full overflow-hidden z-10">
          
          {/* DESKTOP VIEW (Horizontal Workspace) */}
          <div className="hidden lg:flex relative flex-col w-full h-full bg-[#0a0a0a]">
             <div className="absolute top-4 left-6 px-2 bg-black text-[#ccff00] text-[10px] font-mono tracking-widest border border-[#ccff00]/30 z-20 shadow-lg">{TRANSLATIONS.VIEWPORT[language]}</div>
             
             {/* Info overlay */}
             {activeElement && (
                 <div className="absolute bottom-4 left-6 z-20 flex gap-4 font-mono text-[9px] text-white/40 tracking-widest bg-black/50 p-2 rounded backdrop-blur-sm border border-white/5">
                    <span>X: {activeElement.position.x.toFixed(0)}</span>
                    <span>Y: {activeElement.position.y.toFixed(0)}</span>
                    <span>SCALE: {activeElement.scale?.toFixed(2) || '1.00'}</span>
                 </div>
            )}

            {/* Scrolling Workspace Container */}
            <div ref={desktopScrollRef} className="flex-1 w-full overflow-x-auto overflow-y-hidden custom-scrollbar flex items-center px-[10vw]">
                {slides.length === 0 ? (
                    <div className="w-full h-full flex items-center justify-center">
                         <div className="p-8 border-2 border-dashed border-white/10 rounded-2xl flex flex-col items-center justify-center text-white/20 gap-4">
                             <div className="text-sm font-mono uppercase tracking-widest">Workspace Empty</div>
                         </div>
                    </div>
                ) : (
                    <div className="flex items-center gap-8 mx-auto min-w-max py-12">
                         {slides.map((slide, index) => (
                             <div 
                                key={slide.id}
                                id={`desktop-slide-${slide.id}`}
                                onClick={() => setActiveSlideId(slide.id)}
                                className={`relative transition-all duration-300 group cursor-pointer ${
                                    activeSlideId === slide.id 
                                    ? 'z-10 scale-100 opacity-100 shadow-[0_20px_50px_rgba(0,0,0,0.5)] ring-2 ring-[#ccff00] ring-offset-2 ring-offset-black' 
                                    : 'z-0 scale-95 opacity-40 hover:opacity-80 hover:scale-[0.97] hover:shadow-xl grayscale hover:grayscale-0'
                                }`}
                                style={{
                                    height: '75vh', // Enforce height-based sizing
                                    aspectRatio: aspectRatio.replace(':', '/'), // Let width auto-calculate
                                }}
                             >
                                 <Canvas 
                                    slide={slide}
                                    activeElementId={activeSlideId === slide.id ? activeElementId : null}
                                    settings={settings}
                                    isLastSlide={index === slides.length - 1}
                                    aspectRatioClassName={aspectRatioClassName}
                                    aspectRatio={aspectRatio}
                                    onUpdateElement={handleUpdateElement}
                                    onSelectElement={setActiveElementId}
                                    onDownloadSingleSlide={handleDownloadSingleSlide}
                                    language={language}
                                />
                                {/* Slide Number Indicator */}
                                <div className={`absolute -top-8 left-1/2 -translate-x-1/2 text-[10px] font-black font-mono tracking-widest transition-colors ${activeSlideId === slide.id ? 'text-[#ccff00]' : 'text-white/20'}`}>
                                    {index + 1 < 10 ? `0${index + 1}` : index + 1}
                                </div>
                             </div>
                         ))}

                         {/* Add Slide Button at end of strip */}
                         <button 
                            onClick={handleAddSlide} 
                            className="h-[60vh] rounded-xl border border-dashed border-white/10 hover:border-[#ccff00] hover:bg-[#ccff00]/5 flex items-center justify-center text-white/20 hover:text-[#ccff00] transition-all group"
                            style={{ aspectRatio: aspectRatio.replace(':', '/') }}
                         >
                            <PlusIcon className="w-12 h-12 group-hover:scale-110 transition-transform" />
                         </button>
                    </div>
                )}
            </div>
          </div>

          {/* MOBILE VIEW */}
          <div ref={mobileScrollRef} className="lg:hidden w-full h-full flex overflow-x-auto overflow-y-hidden snap-x snap-mandatory z-10 custom-scrollbar pb-[88px]" style={{ scrollBehavior: 'smooth' }}>
              {slides.length === 0 ? (
                   <div className="w-full h-full flex items-center justify-center p-8">
                        <Canvas 
                            slide={undefined}
                            activeElementId={null}
                            settings={settings}
                            isLastSlide={false}
                            aspectRatioClassName={aspectRatioClassName}
                            aspectRatio={aspectRatio}
                            onUpdateElement={() => {}}
                            onSelectElement={() => {}}
                            language={language}
                        />
                   </div>
              ) : (
                  slides.map((slide, index) => (
                      <div key={slide.id} data-slide-id={slide.id} className="mobile-slide-item w-full h-full flex-shrink-0 flex items-center justify-center snap-center p-4">
                        <div className="w-full flex items-center justify-center" style={{ height: '70vh' }}>
                            <Canvas 
                                slide={slide}
                                activeElementId={activeElementId}
                                settings={settings}
                                isLastSlide={index === slides.length - 1}
                                aspectRatioClassName={aspectRatioClassName}
                                aspectRatio={aspectRatio}
                                onUpdateElement={handleUpdateElement}
                                onSelectElement={setActiveElementId}
                                onDownloadSingleSlide={handleDownloadSingleSlide}
                                language={language}
                            />
                         </div>
                      </div>
                  ))
              )}
          </div>
        </main>

        <aside className="fixed lg:static bottom-0 left-0 right-0 z-40 lg:h-auto lg:w-[420px] lg:border-l lg:border-white/10">
          <ControlPanel
            slides={slides}
            activeSlide={activeSlide}
            activeElement={activeElement}
            settings={settings}
            isLoading={isLoading}
            loadingMessage={loadingMessage}
            isDownloading={isDownloading}
            error={error}
            aspectRatio={aspectRatio}
            onAspectRatioChange={setAspectRatio}
            onCreateCarousel={handleCreateCarousel}
            onAddSlide={handleAddSlide}
            onDeleteSlide={handleDeleteSlide}
            onSelectSlide={(id) => {
                setActiveSlideId(id);
                const sl = slides.find(s => s.id === id);
                if (sl && sl.elements.length > 0) setActiveElementId(sl.elements[0].id);
                if (id && mobileScrollRef.current) {
                   const el = mobileScrollRef.current.querySelector(`[data-slide-id="${id}"]`);
                   el?.scrollIntoView({ behavior: 'smooth', inline: 'center' });
                }
            }}
            onUpdateSlide={handleUpdateSlide}
            onUpdateElement={handleUpdateElement}
            onAddElement={handleAddElement}
            onDeleteElement={handleDeleteElement}
            onSettingsChange={setSettings}
            onDownloadCarousel={handleDownloadCarousel}
            onDownloadPDF={handleDownloadPDF}
            onDownloadSingleSlide={handleDownloadSingleSlide}
            onRestart={handleRestart}
            setError={setError}
            onGenerateAllImages={handleGenerateAllImages}
            onGenerateSingleImage={handleGenerateSingleImage}
            onMakeViral={handleMakeViral}
            language={language}
            onLanguageChange={setLanguage}
            history={history}
            onLoadHistory={loadFromHistory}
            onDeleteHistory={deleteFromHistory}
            referenceImage={referenceImage}
            referenceType={referenceType}
            onReferenceImageChange={setReferenceImage}
            onReferenceTypeChange={setReferenceType}
          />
        </aside>

      <div aria-hidden="true" style={{ position: 'fixed', left: '-9999px', top: '-9999px' }}>
        {slides.map((s, index) => (
          <div key={s.id} id={`download-container-${s.id}`} style={{ width: `${downloadDimensions.width}px`, height: `${downloadDimensions.height}px` }}>
            <Canvas
              slide={s}
              activeElementId={null}
              settings={settings}
              isLastSlide={index === slides.length - 1}
              aspectRatioClassName={aspectRatioClassName}
              aspectRatio={aspectRatio}
              onUpdateElement={() => {}}
              onSelectElement={() => {}}
              isExportMode={true}
              language={language}
            />
          </div>
        ))}
      </div>
    </div>
  );
};

export default App;
