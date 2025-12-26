-- Migration: 018_carousel_photos.sql
-- Add combo mode support: user photos + LoRA generation

-- Add source tracking to slides
ALTER TABLE carousel_slides ADD COLUMN IF NOT EXISTS source_type VARCHAR(20) DEFAULT 'ai_generated';
-- Values: 'user_uploaded', 'lora_generated'

ALTER TABLE carousel_slides ADD COLUMN IF NOT EXISTS original_photo_url TEXT;
-- For uploaded photos, stores the original URL

-- Uploaded photos storage (for combo mode workflow)
CREATE TABLE IF NOT EXISTS carousel_uploaded_photos (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    carousel_id UUID NOT NULL REFERENCES carousels(id) ON DELETE CASCADE,
    slide_position INT,  -- NULL means not assigned to slide yet
    photo_url TEXT NOT NULL,
    analysis_json JSONB,  -- Vision LLM analysis results: {description, mood, style}
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_uploaded_photos_carousel ON carousel_uploaded_photos(carousel_id);

-- Add lora_url to carousels for custom LoRA per carousel
ALTER TABLE carousels ADD COLUMN IF NOT EXISTS lora_url TEXT;
