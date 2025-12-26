-- Migration: Add AI trigger words and disable crypto triggers
-- Run via Neon MCP or psql

-- =============================================================================
-- Step 1: Disable all crypto triggers (keep in DB for history)
-- =============================================================================
UPDATE trigger_words SET is_active = false WHERE category LIKE 'crypto_%';

-- =============================================================================
-- Step 2: Add AI trigger words
-- =============================================================================
INSERT INTO trigger_words (word, category, is_active) VALUES
-- === УСТАЛОСТЬ ОТ КОНТЕНТА / БОЛИ ===
('устал снимать', 'ai_pain', true),
('нет времени на контент', 'ai_pain', true),
('выгорание от блога', 'ai_pain', true),
('не успеваю снимать', 'ai_pain', true),
('надоело снимать', 'ai_pain', true),
('контент отнимает время', 'ai_pain', true),
('как делать рилсы быстро', 'ai_pain', true),
('нужен контент', 'ai_pain', true),
('не знаю что снимать', 'ai_pain', true),
('идеи для рилс', 'ai_pain', true),
('идеи для reels', 'ai_pain', true),
('идеи для видео', 'ai_pain', true),
('выгорел от контента', 'ai_pain', true),
('устал от соцсетей', 'ai_pain', true),
('нет идей для контента', 'ai_pain', true),
('как снимать чаще', 'ai_pain', true),
('как постить каждый день', 'ai_pain', true),
('контент-план', 'ai_pain', true),
('контентмейкер', 'ai_pain', true),
('контент менеджер', 'ai_pain', true),

-- === AI ИНСТРУМЕНТЫ ===
('нейросеть для видео', 'ai_tools', true),
('ai аватар', 'ai_tools', true),
('цифровой клон', 'ai_tools', true),
('говорящая голова', 'ai_tools', true),
('нейроконтент', 'ai_tools', true),
('ai для блогера', 'ai_tools', true),
('автоматизация контента', 'ai_tools', true),
('synthetic media', 'ai_tools', true),
('digital twin', 'ai_tools', true),
('talking head', 'ai_tools', true),
('midjourney', 'ai_tools', true),
('миджорни', 'ai_tools', true),
('мидджорни', 'ai_tools', true),
('runway', 'ai_tools', true),
('runwayml', 'ai_tools', true),
('ранвей', 'ai_tools', true),
('heygen', 'ai_tools', true),
('хейген', 'ai_tools', true),
('хейджен', 'ai_tools', true),
('d-id', 'ai_tools', true),
('did', 'ai_tools', true),
('eleven labs', 'ai_tools', true),
('elevenlabs', 'ai_tools', true),
('suno', 'ai_tools', true),
('suno ai', 'ai_tools', true),
('kling', 'ai_tools', true),
('kling ai', 'ai_tools', true),
('pika', 'ai_tools', true),
('pika labs', 'ai_tools', true),
('stable diffusion', 'ai_tools', true),
('dall-e', 'ai_tools', true),
('dalle', 'ai_tools', true),
('flux', 'ai_tools', true),
('chatgpt', 'ai_tools', true),
('gpt-4', 'ai_tools', true),
('gpt4', 'ai_tools', true),
('claude', 'ai_tools', true),
('anthropic', 'ai_tools', true),

-- === КОНКРЕТНЫЕ ФУНКЦИИ ===
('lip sync', 'ai_features', true),
('лип синк', 'ai_features', true),
('липсинк', 'ai_features', true),
('синхронизация губ', 'ai_features', true),
('озвучка видео', 'ai_features', true),
('генерация видео', 'ai_features', true),
('text to video', 'ai_features', true),
('текст в видео', 'ai_features', true),
('клонирование голоса', 'ai_features', true),
('voice clone', 'ai_features', true),
('голосовой клон', 'ai_features', true),
('клон голоса', 'ai_features', true),
('нейрофото', 'ai_features', true),
('ai фото', 'ai_features', true),
('генерация фото', 'ai_features', true),
('фото нейросетью', 'ai_features', true),
('face swap', 'ai_features', true),
('фейс свап', 'ai_features', true),
('замена лица', 'ai_features', true),
('deepfake', 'ai_features', true),
('дипфейк', 'ai_features', true),
('deep fake', 'ai_features', true),
('аватар из фото', 'ai_features', true),
('видео из фото', 'ai_features', true),
('фото в видео', 'ai_features', true),
('text to speech', 'ai_features', true),
('tts', 'ai_features', true),
('озвучка текста', 'ai_features', true),
('голосовой бот', 'ai_features', true),
('чат-бот', 'ai_features', true),
('чатбот', 'ai_features', true),
('chatbot', 'ai_features', true),
('автоответчик', 'ai_features', true),
('бот для ответов', 'ai_features', true),

-- === КОНТЕНТ И БЛОГИНГ ===
('reels', 'ai_content', true),
('рилс', 'ai_content', true),
('рилсы', 'ai_content', true),
('shorts', 'ai_content', true),
('шортс', 'ai_content', true),
('тикток', 'ai_content', true),
('tiktok', 'ai_content', true),
('видеоконтент', 'ai_content', true),
('видео контент', 'ai_content', true),
('контент для инстаграма', 'ai_content', true),
('нейроблогер', 'ai_content', true),
('ai блогер', 'ai_content', true),
('автоблогер', 'ai_content', true),
('генератор видео', 'ai_content', true),
('автоконтент', 'ai_content', true),
('авто контент', 'ai_content', true),
('виртуальный блогер', 'ai_content', true),
('виртуальный инфлюенсер', 'ai_content', true),
('ugc', 'ai_content', true),
('user generated content', 'ai_content', true),
('ugc creator', 'ai_content', true),
('сторис', 'ai_content', true),
('stories', 'ai_content', true),
('контент для stories', 'ai_content', true),

-- === МОНЕТИЗАЦИЯ ===
('монетизация знаний', 'ai_monetization', true),
('пассивный доход с контента', 'ai_monetization', true),
('как продавать курсы', 'ai_monetization', true),
('инфобизнес', 'ai_monetization', true),
('онлайн-курс', 'ai_monetization', true),
('инфопродукт', 'ai_monetization', true),
('цифровой продукт', 'ai_monetization', true),
('продажа курсов', 'ai_monetization', true),
('воронка продаж', 'ai_monetization', true),
('автоворонка', 'ai_monetization', true),
('лид-магнит', 'ai_monetization', true),
('вебинар', 'ai_monetization', true),
('марафон', 'ai_monetization', true),
('запуск курса', 'ai_monetization', true),

-- === ВОПРОСЫ ===
('как масштабировать контент', 'ai_question', true),
('где найти ai для видео', 'ai_question', true),
('посоветуйте нейросеть', 'ai_question', true),
('какой ai для видео лучше', 'ai_question', true),
('как создать аватар', 'ai_question', true),
('бот для генерации', 'ai_question', true),
('какую нейросеть использовать', 'ai_question', true),
('лучшая нейросеть', 'ai_question', true),
('как делать видео без лица', 'ai_question', true),
('как снимать без камеры', 'ai_question', true)
ON CONFLICT DO NOTHING;

-- =============================================================================
-- Step 3: Link AI triggers to all active chats
-- =============================================================================
INSERT INTO chat_trigger_words (chat_config_id, trigger_word_id)
SELECT c.id, w.id
FROM trigger_chat_configs c
CROSS JOIN trigger_words w
WHERE w.category LIKE 'ai_%' AND w.is_active = true AND c.is_active = true
ON CONFLICT DO NOTHING;

-- =============================================================================
-- Step 4: Update chat configs to use AI patterns
-- =============================================================================
UPDATE trigger_chat_configs SET
  expected_forward_pattern = 'ЛИД|Клиент|нейро|AI|контент',
  response_template = 'Напиши мне в личку, помогу с нейроконтентом',
  updated_at = NOW()
WHERE is_active = true;
