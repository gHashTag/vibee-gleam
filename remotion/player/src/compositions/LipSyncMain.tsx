import React from 'react';
import {
  AbsoluteFill,
  Audio,
  Img,
  Sequence,
  Video,
  useCurrentFrame,
  useVideoConfig,
  interpolate,
  Easing,
  staticFile,
} from 'remotion';
import { Captions } from '@/components/Captions';
import type { Caption } from '@remotion/captions';
import type { CaptionItem, CaptionStyle } from '@/store/types';

/**
 * 🎨 LipSync Main - Профессиональный шаблон для Instagram Reels
 * Динамическая трансформация аватара в круглый бейдж с glassmorphism эффектом
 */

// Helper: wrap local paths with staticFile() for server render, direct paths for browser player
const resolveMediaPath = (path: string): string => {
  // In browser context (player), use paths directly - Vite proxy will handle them
  if (typeof window !== 'undefined' && typeof document !== 'undefined') {
    console.log('🌐 [BROWSER] Using direct path:', path);
    return path;
  }
  // In render context (server), use staticFile for local paths
  if (path.startsWith('/') && !path.startsWith('//') && !path.includes('://')) {
    console.log('🖥️ [SERVER] Using staticFile:', path);
    return staticFile(path);
  }
  return path;
};

// Helper: detect if URL is an image or video
const isImageUrl = (url: string): boolean => {
  const imageExtensions = ['.jpg', '.jpeg', '.png', '.gif', '.webp', '.bmp', '.svg'];
  const lowerUrl = url.toLowerCase();

  // Check file extension
  if (imageExtensions.some(ext => lowerUrl.includes(ext))) {
    return true;
  }

  // Check common image hosting patterns
  if (lowerUrl.includes('mj.run') || // Midjourney
      lowerUrl.includes('midjourney') ||
      lowerUrl.includes('imgur') ||
      lowerUrl.includes('unsplash') ||
      lowerUrl.includes('pexels')) {
    return true;
  }

  return false;
};

export interface LipSyncMainProps {
  lipSyncVideo: string;
  coverImage: string;
  backgroundMusic: string;
  musicVolume: number;
  backgroundVideos: string[]; // 4 видео
  coverDuration: number;
  vignetteStrength: number;
  colorCorrection: number;
  // 🎯 Динамические параметры Avatar2 (круглый аватар)
  circleSizePercent?: number; // Размер круга в % от высоты экрана (default: 25.2)
  circleBottomPercent?: number; // Отступ снизу в % от высоты (default: 15)
  circleLeftPx?: number; // Отступ слева в пикселях (default: 40)
  // 📝 Captions (TikTok-style subtitles)
  captions?: CaptionItem[];
  captionStyle?: CaptionStyle;
  showCaptions?: boolean;
}

export const LipSyncMain: React.FC<LipSyncMainProps> = ({
  lipSyncVideo,
  coverImage,
  backgroundMusic,
  musicVolume,
  backgroundVideos,
  coverDuration = 0.5, // Короткая обложка
  vignetteStrength = 0.7,
  colorCorrection = 1.2,
  // 🎯 Динамические параметры Avatar2
  circleSizePercent = 25.2, // 25.2% от высоты экрана
  circleBottomPercent = 15, // 15% от высоты отступ снизу
  circleLeftPx = 40, // 40px отступ слева
  // 📝 Captions
  captions = [],
  captionStyle = {},
  showCaptions = true,
}) => {
  const { fps, durationInFrames, width, height } = useVideoConfig();
  const frame = useCurrentFrame();

  console.log('🎨 [LIPSYNC MAIN] Инициализация:', {
    durationInFrames,
    fps,
    backgroundVideos: backgroundVideos.length,
  });

  const coverFrames = Math.floor(coverDuration * fps);

  // 🎬 ДИНАМИЧЕСКАЯ СИСТЕМА СЕГМЕНТОВ
  const transitionDuration = 4; // секунд на каждый BG segment
  const transitionFrames = transitionDuration * fps;
  const gapDuration = 1.5; // секунд аватара между сегментами
  const gapFrames = gapDuration * fps;

  // 📊 Расчет количества сегментов
  const maxSegments = Math.floor(
    (durationInFrames - coverFrames - gapFrames) /
      (transitionFrames + gapFrames)
  );
  const actualSegments = Math.min(
    backgroundVideos.length,
    Math.max(1, maxSegments)
  );

  console.log('🎨 [SEGMENTS]:', { maxSegments, actualSegments });

  // 🎨 СОЗДАЕМ СЕГМЕНТЫ с разными стилями микширования
  const bgSegments = [];
  let currentFrame = coverFrames + gapFrames;

  for (let i = 0; i < actualSegments && i < backgroundVideos.length; i++) {
    const remainingFrames = durationInFrames - currentFrame;
    const segmentDuration = Math.max(
      0,
      Math.min(transitionFrames, remainingFrames)
    );

    if (segmentDuration > 0) {
      // 🎨 СТИЛИ МИКШИРОВАНИЯ (циклически)
      const mixStyle = i % 4;
      let blendMode: string = 'normal';
      let baseOpacity = 1.0;

      switch (mixStyle) {
        case 0: // Легкая прозрачность - аватар слегка просвечивает
          blendMode = 'normal';
          baseOpacity = 0.85;
          break;
        case 1: // Screen blend - светлые участки ярче
          blendMode = 'screen';
          baseOpacity = 0.9;
          break;
        case 2: // Normal с легкой прозрачностью
          blendMode = 'normal';
          baseOpacity = 0.9;
          break;
        case 3: // Полное перекрытие
          blendMode = 'normal';
          baseOpacity = 1.0;
          break;
      }

      bgSegments.push({
        name: `BG_${String(i + 1).padStart(2, '0')}`,
        video: backgroundVideos[i] || backgroundVideos[0],
        startFrame: currentFrame,
        durationFrames: segmentDuration,
        blendMode,
        baseOpacity,
      });

      currentFrame += segmentDuration + gapFrames;
    }
  }

  console.log('🎨 [SEGMENTS] Сегментов создано:', bgSegments.length);

  // 🎨 Цветокоррекция
  const cinematicFilter = `brightness(${
    colorCorrection * 0.95
  }) contrast(1.15) saturate(1.3)`;

  // 📏 Crossfade длительность
  const crossfadeDuration = fps * 0.8;

  // 🎭 ДИНАМИЧЕСКАЯ ТРАНСФОРМАЦИЯ АВАТАРА + CROSSFADE
  // Аватар превращается в кружочек в углу когда активен BG segment
  let avatarScale = 1.0; // 1.0 = полный экран
  let isBgActive = false;
  let avatarCrossfadeOpacity = 1.0; // Для плавного crossfade с Avatar2
  const crossfadeDur = fps * 0.5; // 0.5 сек crossfade
  const targetCircleScale = circleSizePercent / 100; // 🎯 Размер круга из props (динамический)

  for (const segment of bgSegments) {
    const segmentFrame = frame - segment.startFrame;

    if (segmentFrame >= 0 && segmentFrame < segment.durationFrames) {
      // Этот сегмент активен
      isBgActive = true;

      // 🎯 УВЕЛИЧЕННАЯ ПЛАВНАЯ трансформация - 1.2 секунды для супер-плавного морфинга
      const smoothTransform = fps * 1.2;

      // 🎭 УЛЬТРА-плавная трансформация аватара: 100% → 50% → 25% → 50% → 100%
      // Начинается СРАЗУ с началом B-roll (segmentFrame 0)
      const targetScale = interpolate(
        segmentFrame,
        [
          0,
          smoothTransform * 0.33, // 33%: 100% → 75%
          smoothTransform * 0.66, // 66%: 75% → targetCircleScale (25.2%)
          segment.durationFrames - smoothTransform * 0.66, // Начало увеличения
          segment.durationFrames - smoothTransform * 0.33, // 75% → 100%
          segment.durationFrames, // Завершение увеличения
        ],
        [1.0, 0.75, targetCircleScale, targetCircleScale, 0.75, 1.0], // Супер-плавная кривая без скачков
        {
          extrapolateLeft: 'clamp',
          extrapolateRight: 'clamp',
          easing: Easing.bezier(0.4, 0, 0.2, 1), // Наиболее плавная кривая Безье
        }
      );

      // 🔄 ПОЛНОСТЬЮ ПЛАВНЫЙ CROSSFADE: от 0% до 100% без скачков
      const extendedCrossfade = fps * 1.5; // Увеличили до 1.5 сек для супер-плавности
      avatarCrossfadeOpacity = interpolate(
        segmentFrame,
        [
          -fps * 0.5, // Начинаем fade за 0.5 сек ДО b-roll
          -fps * 0.5 + extendedCrossfade * 0.33, // 33% времени: 100% → 75%
          -fps * 0.5 + extendedCrossfade * 0.66, // 66% времени: 75% → 25%
          segment.durationFrames - extendedCrossfade * 0.66, // Начинаем появляться: 25% → 75%
          segment.durationFrames - extendedCrossfade * 0.33, // 75% → 100%
          segment.durationFrames, // Полностью видим
        ],
        [1.0, 0.75, 0.25, 0.25, 0.75, 1.0], // Плавная кривая без резких скачков
        {
          extrapolateLeft: 'clamp',
          extrapolateRight: 'clamp',
          easing: Easing.bezier(0.4, 0, 0.2, 1), // Еще более плавная кривая Безье
        }
      );

      avatarScale = Math.min(avatarScale, targetScale);
    }
  }

  // 🎯 Позиционирование кружка - ДИНАМИЧЕСКИЕ ПАРАМЕТРЫ из props
  const circleSize = height * (circleSizePercent / 100); // Размер из props
  const circleMargin = 60; // базовый отступ
  const circleBottomOffset =
    circleMargin + height * (circleBottomPercent / 100); // Отступ снизу из props
  const circleLeftOffset = circleLeftPx; // Отступ слева из props

  // УЛЬТРА-плавное скругление без резких переходов
  const avatarBorderRadius = interpolate(
    avatarScale,
    [targetCircleScale, 0.5, 0.75, 1.0], // 4 точки для плавности
    [50, 35, 15, 0], // 50%=круг → 35% → 15% → 0% (прямоугольник)
    {
      extrapolateLeft: 'clamp',
      extrapolateRight: 'clamp',
      easing: Easing.bezier(0.4, 0, 0.2, 1), // Супер-плавная кривая Безье
    }
  );

  return (
    <AbsoluteFill style={{ backgroundColor: '#000' }}>
      {/* 🎵 Аудио - ТОЛЬКО из lipSyncVideo, backgroundMusic убран для избежания конфликта */}
      {/* <Audio src={backgroundMusic} volume={musicVolume} /> */}

      {/* 🎤 БАЗА: Аватар с ДИНАМИЧЕСКОЙ ТРАНСФОРМАЦИЕЙ - СКРЫВАЕТСЯ при b-roll */}
      <Sequence
        name="🎤 Аватар (БАЗА → СКРЫТ при B-ROLL)"
        from={0}
        durationInFrames={durationInFrames}
      >
        <AbsoluteFill>
          {/* Wrapper для стеклянного эффекта (overflow: visible) */}
          <div
            style={{
              position: 'absolute',
              // Плавное перемещение: слева внизу → полный экран
              top: interpolate(
                avatarScale,
                [targetCircleScale, 1.0],
                [height - circleSize - circleBottomOffset, 0], // Круг внизу (выше на 20%) → полный экран от верха
                { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
              ),
              left: interpolate(
                avatarScale,
                [targetCircleScale, 1.0],
                [circleLeftOffset, 0], // Круг слева → полный экран от левого края
                { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
              ),
              // Плавное изменение размера
              width: interpolate(
                avatarScale,
                [targetCircleScale, 1.0],
                [circleSize, width],
                { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
              ),
              // Плавное изменение высоты
              height: interpolate(
                avatarScale,
                [targetCircleScale, 1.0],
                [circleSize, height],
                { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
              ),
              overflow: 'visible', // Для стеклянного свечения снаружи
              // ДИНАМИЧЕСКИЙ Z-INDEX: круг поверх B-roll, полный экран - под B-roll!
              zIndex: avatarScale < 0.5 ? 10 : 1,
              // ✅ CROSSFADE: плавное исчезновение/появление вместо резкого скрытия
              opacity: avatarCrossfadeOpacity,
            }}
          >
            {/* 💎 СТЕКЛЯННЫЙ ЭФФЕКТ С БЛИКАМИ - плавное появление/исчезновение */}
            {(() => {
              // Плавная opacity для стеклянных эффектов (0% → 100%)
              const glassOpacity = interpolate(
                avatarBorderRadius,
                [0, 25, 50],
                [0, 0.6, 1.0],
                {
                  extrapolateLeft: 'clamp',
                  extrapolateRight: 'clamp',
                  easing: Easing.bezier(0.4, 0, 0.2, 1),
                }
              );

              if (glassOpacity <= 0) return null;

              return (
                <>
                  {/* Основное стекло - glassmorphism */}
                  <div
                    style={{
                      position: 'absolute',
                      width: '110%',
                      height: '110%',
                      top: '-5%',
                      left: '-5%',
                      borderRadius: '50%',
                      background: 'rgba(255, 255, 255, 0.1)',
                      backdropFilter: 'blur(10px)',
                      border: '2px solid rgba(255, 255, 255, 0.2)',
                      boxShadow: `
                        0 8px 32px rgba(0, 0, 0, 0.1),
                        inset 0 2px 8px rgba(255, 255, 255, 0.15)
                      `,
                      opacity: glassOpacity,
                      zIndex: 0,
                    }}
                  />

                  {/* Вращающийся световой блик */}
                  <div
                    style={{
                      position: 'absolute',
                      width: '110%',
                      height: '110%',
                      top: '-5%',
                      left: '-5%',
                      borderRadius: '50%',
                      background: `conic-gradient(
                        from ${frame * 3}deg,
                        rgba(255, 255, 255, 0.4) 0deg,
                        rgba(255, 255, 255, 0.2) 30deg,
                        transparent 90deg,
                        transparent 270deg,
                        rgba(255, 255, 255, 0.2) 330deg,
                        rgba(255, 255, 255, 0.4) 360deg
                      )`,
                      filter: 'blur(4px)',
                      opacity: glassOpacity,
                      zIndex: 0,
                    }}
                  />
                </>
              );
            })()}

            {/* 🎤 Контейнер аватара с clip (overflow: hidden) */}
            <div
              style={{
                position: 'relative',
                width: '100%',
                height: '100%',
                borderRadius: `${avatarBorderRadius}%`,
                overflow: 'hidden', // ← КЛЮЧЕВОЕ! Clip для круга
                zIndex: 1,
              }}
            >
              {/* 💎 Стеклянная рамка - плавное появление */}
              {(() => {
                const borderOpacity = interpolate(
                  avatarBorderRadius,
                  [0, 25, 50],
                  [0, 0.6, 1.0],
                  {
                    extrapolateLeft: 'clamp',
                    extrapolateRight: 'clamp',
                    easing: Easing.bezier(0.4, 0, 0.2, 1),
                  }
                );

                if (borderOpacity <= 0) return null;

                return (
                  <div
                    style={{
                      position: 'absolute',
                      width: '100%',
                      height: '100%',
                      borderRadius: '50%',
                      border: '3px solid rgba(255, 255, 255, 0.3)',
                      boxShadow: `
                        0 0 20px rgba(255, 255, 255, 0.2),
                        0 4px 16px rgba(0, 0, 0, 0.15),
                        inset 0 2px 6px rgba(255, 255, 255, 0.25),
                        inset 0 -2px 6px rgba(0, 0, 0, 0.1)
                      `,
                      opacity: borderOpacity,
                      zIndex: 2,
                      pointerEvents: 'none',
                    }}
                  />
                );
              })()}

              {/* 🎤 Видео аватара - со звуком */}
              <Video
                src={resolveMediaPath(lipSyncVideo)}
                preload="auto"
                volume={1}
                style={{
                  width: '100%',
                  height: '100%',
                  objectFit: avatarScale < 0.5 ? 'cover' : 'contain',
                  filter: cinematicFilter,
                }}
              />
            </div>
          </div>
        </AbsoluteFill>
      </Sequence>

      {/* 🎤 AVATAR2: Всплывающий круглый аватар при b-roll */}
      {/* ВАЖНО: Всегда монтируем для синхронизации Video, видимость через popupScale */}
      <Sequence
        name="🎤 Avatar2 (ВСПЛЫВАЮЩИЙ КРУГ)"
        from={0}
        durationInFrames={durationInFrames}
      >
        <AbsoluteFill>
          {(() => {
              // 🔄 УЛУЧШЕННОЕ всплытие Avatar2 с супер-плавными переходами
              let popupScale = 0;
              const extendedPopupDuration = fps * 1.4; // Увеличили до 1.4 сек
              const preStartOffset = fps * 0.5; // Увеличили до 0.5 сек ДО начала b-roll
              const postEndOffset = fps * 0.8; // Добавили 0.8 сек ПОСЛЕ конца b-roll

              for (const segment of bgSegments) {
                // Avatar2 появляется за 0.5 сек РАНЬШЕ b-roll и исчезает за 0.8 сек ПОСЛЕ
                const segmentFrame =
                  frame - (segment.startFrame - preStartOffset);

                if (
                  segmentFrame >= 0 &&
                  segmentFrame <
                    segment.durationFrames + preStartOffset + postEndOffset
                ) {
                  // СУПЕР-плавное всплытие без резких скачков
                  popupScale = interpolate(
                    segmentFrame,
                    [
                      0,
                      extendedPopupDuration * 0.25, // Медленное начало
                      extendedPopupDuration * 0.75, // Продолжение всплытия
                      segment.durationFrames +
                        preStartOffset -
                        extendedPopupDuration * 0.75, // Начало исчезновения
                      segment.durationFrames +
                        preStartOffset -
                        extendedPopupDuration * 0.25, // Продолжение исчезновения
                      segment.durationFrames + preStartOffset + postEndOffset, // Полное исчезновение
                    ],
                    [0, 0.3, 0.85, 0.85, 0.3, 0], // Плавная кривая от 0% до 85%
                    {
                      extrapolateLeft: 'clamp',
                      extrapolateRight: 'clamp',
                      easing: Easing.bezier(0.4, 0, 0.2, 1), // Супер-плавная кривая
                    }
                  );
                  break;
                }
              }

              if (popupScale <= 0) return null;

              return (
                <div
                  style={{
                    position: 'absolute',
                    top: height - circleSize - circleBottomOffset,
                    left: circleLeftOffset,
                    width: circleSize,
                    height: circleSize,
                    transform: `scale(${popupScale})`,
                    zIndex: 15, // Поверх b-roll и оригинального аватара
                    overflow: 'visible',
                  }}
                >
                  {/* 💎 СТЕКЛЯННЫЙ ЭФФЕКТ для Avatar2 */}
                  <div
                    style={{
                      position: 'absolute',
                      width: '110%',
                      height: '110%',
                      top: '-5%',
                      left: '-5%',
                      borderRadius: '50%',
                      background: 'rgba(255, 255, 255, 0.1)',
                      backdropFilter: 'blur(10px)',
                      border: '2px solid rgba(255, 255, 255, 0.2)',
                      boxShadow: `
                        0 0 20px rgba(255, 255, 255, 0.15),
                        inset 0 2px 8px rgba(255, 255, 255, 0.15)
                      `,
                      zIndex: 0,
                    }}
                  />

                  {/* Вращающийся световой блик */}
                  <div
                    style={{
                      position: 'absolute',
                      width: '110%',
                      height: '110%',
                      top: '-5%',
                      left: '-5%',
                      borderRadius: '50%',
                      background: `conic-gradient(
                        from ${frame * 3}deg,
                        rgba(255, 255, 255, 0.4) 0deg,
                        rgba(255, 255, 255, 0.2) 30deg,
                        transparent 90deg,
                        transparent 270deg,
                        rgba(255, 255, 255, 0.2) 330deg,
                        rgba(255, 255, 255, 0.4) 360deg
                      )`,
                      filter: 'blur(4px)',
                      zIndex: 0,
                    }}
                  />

                  {/* Контейнер с clip для круга */}
                  <div
                    style={{
                      position: 'relative',
                      width: '100%',
                      height: '100%',
                      borderRadius: '50%',
                      overflow: 'hidden',
                      zIndex: 1,
                    }}
                  >
                    {/* Стеклянная рамка */}
                    <div
                      style={{
                        position: 'absolute',
                        width: '100%',
                        height: '100%',
                        borderRadius: '50%',
                        border: '3px solid rgba(255, 255, 255, 0.3)',
                        boxShadow: `
                          0 0 20px rgba(255, 255, 255, 0.2),
                          inset 0 2px 6px rgba(255, 255, 255, 0.25)
                        `,
                        zIndex: 2,
                        pointerEvents: 'none',
                      }}
                    />

                    {/* 🎤 Видео аватара с lip-sync (БЕЗ ЗВУКА - muted) */}
                    <Video
                      src={resolveMediaPath(lipSyncVideo)}
                      preload="auto"
                      muted
                      style={{
                        width: '100%',
                        height: '100%',
                        objectFit: 'cover',
                        filter: cinematicFilter,
                      }}
                    />
                  </div>
                </div>
              );
            })()}
          </AbsoluteFill>
        </Sequence>

      {/* 🎬 BG СЕГМЕНТЫ: Динамическая система с разными blend modes */}
      {bgSegments.map((segment, index) => {
        const segmentFrame = frame - segment.startFrame;

        // ✅ УБРАЛИ ЗАТЕМНЕНИЕ - постоянная opacity без fade
        // Теперь b-roll появляется сразу, без затемнения
        const finalOpacity = segment.baseOpacity;

        // 🎨 РАЗНЫЕ АНИМАЦИИ для каждого сегмента
        const animationType = index % 5; // 5 типов анимаций
        let transform = '';

        switch (animationType) {
          case 0: // Zoom In
            const zoomIn = interpolate(
              segmentFrame,
              [0, segment.durationFrames],
              [1.0, 1.1],
              { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
            );
            transform = `scale(${zoomIn})`;
            break;

          case 1: // Zoom Out
            const zoomOut = interpolate(
              segmentFrame,
              [0, segment.durationFrames],
              [1.1, 1.0],
              { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
            );
            transform = `scale(${zoomOut})`;
            break;

          case 2: // Pan Right
            const panRight = interpolate(
              segmentFrame,
              [0, segment.durationFrames],
              [0, 5],
              { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
            );
            transform = `scale(1.15) translateX(${panRight}%)`;
            break;

          case 3: // Pan Left
            const panLeft = interpolate(
              segmentFrame,
              [0, segment.durationFrames],
              [5, 0],
              { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
            );
            transform = `scale(1.15) translateX(${panLeft}%)`;
            break;

          case 4: // Subtle Rotate + Zoom
            const rotateAngle = interpolate(
              segmentFrame,
              [0, segment.durationFrames],
              [-1, 1],
              { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
            );
            const rotateZoom = interpolate(
              segmentFrame,
              [0, segment.durationFrames],
              [1.05, 1.12],
              { extrapolateLeft: 'clamp', extrapolateRight: 'clamp' }
            );
            transform = `scale(${rotateZoom}) rotate(${rotateAngle}deg)`;
            break;
        }

        // Check if this segment is an image or video
        const isImage = isImageUrl(segment.video);

        return (
          <Sequence
            key={`segment-${index}`}
            name={`🎨 ${segment.name}`}
            from={segment.startFrame}
            durationInFrames={segment.durationFrames}
          >
            <AbsoluteFill
              style={{
                opacity: finalOpacity,
                mixBlendMode: segment.blendMode as any,
                zIndex: 5, // Поверх аватара в полном экране (1), под кругом (10)
              }}
            >
              {isImage ? (
                // 🖼️ Render as Image with Ken Burns effect
                <Img
                  src={resolveMediaPath(segment.video)}
                  style={{
                    width: '100%',
                    height: '100%',
                    objectFit: 'cover',
                    transform,
                    filter: cinematicFilter,
                  }}
                />
              ) : (
                // 🎬 Render as Video
                <Video
                  src={resolveMediaPath(segment.video)}
                  preload="auto"
                  muted={true}
                  style={{
                    width: '100%',
                    height: '100%',
                    objectFit: 'cover',
                    transform,
                    filter: cinematicFilter,
                  }}
                />
              )}
            </AbsoluteFill>
          </Sequence>
        );
      })}

      {/* 📸 COVER - короткая обложка */}
      {frame < coverFrames && (
        <AbsoluteFill
          style={{
            opacity: interpolate(
              frame,
              [0, coverFrames * 0.7, coverFrames],
              [1, 1, 0]
            ),
          }}
        >
          <Img
            src={resolveMediaPath(coverImage)}
            style={{
              width: '100%',
              height: '100%',
              objectFit: 'cover',
              filter: 'brightness(1.2) contrast(1.1)',
            }}
          />
        </AbsoluteFill>
      )}

      {/* 🌟 ВИНЬЕТКА - всегда поверх */}
      <AbsoluteFill
        style={{
          background: `radial-gradient(ellipse at center, transparent 40%, rgba(0,0,0,${
            vignetteStrength * 0.4
          }) 70%, rgba(0,0,0,${vignetteStrength}) 100%)`,
          pointerEvents: 'none',
        }}
      />

      {/* 📝 ТОНКИЙ Film grain - всегда */}
      <AbsoluteFill
        style={{
          background: `repeating-linear-gradient(
            ${frame % 360}deg,
            transparent,
            transparent 2px,
            rgba(255,255,255,0.01) 2px,
            rgba(255,255,255,0.01) 4px
          )`,
          opacity: 0.3,
          mixBlendMode: 'overlay',
          pointerEvents: 'none',
        }}
      />

      {/* 📝 CAPTIONS - TikTok-style subtitles */}
      {showCaptions && captions.length > 0 && (
        <Captions
          captions={captions as Caption[]}
          fontSize={captionStyle.fontSize ?? 48}
          textColor={captionStyle.textColor ?? '#ffffff'}
          highlightColor={captionStyle.highlightColor ?? '#f59e0b'}
          backgroundColor={captionStyle.backgroundColor ?? 'rgba(0, 0, 0, 0.6)'}
          bottomPercent={captionStyle.bottomPercent ?? 20}
          maxWidthPercent={captionStyle.maxWidthPercent ?? 85}
          fontFamily={captionStyle.fontFamily}
          fontWeight={captionStyle.fontWeight ?? 700}
          showShadow={captionStyle.showShadow ?? true}
        />
      )}
    </AbsoluteFill>
  );
};
