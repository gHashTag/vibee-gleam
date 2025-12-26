-- Instagram Carousel Creator Schema
-- Migration: 017_carousels.sql

-- Main carousels table
CREATE TABLE IF NOT EXISTS carousels (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id BIGINT,
    topic TEXT NOT NULL,
    aspect_ratio VARCHAR(10) NOT NULL DEFAULT '4:5',
    language VARCHAR(5) NOT NULL DEFAULT 'ru',
    settings JSONB NOT NULL DEFAULT '{}',
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Slides table
CREATE TABLE IF NOT EXISTS carousel_slides (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    carousel_id UUID NOT NULL REFERENCES carousels(id) ON DELETE CASCADE,
    position INT NOT NULL,
    image_url TEXT,
    image_prompt TEXT,
    frame_type VARCHAR(20) DEFAULT 'none',
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Slide elements table
CREATE TABLE IF NOT EXISTS slide_elements (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    slide_id UUID NOT NULL REFERENCES carousel_slides(id) ON DELETE CASCADE,
    element_type VARCHAR(20) NOT NULL,  -- 'heading', 'text', 'image', 'sticker'
    content TEXT NOT NULL,
    position_x FLOAT NOT NULL DEFAULT 50,
    position_y FLOAT NOT NULL DEFAULT 60,
    scale FLOAT NOT NULL DEFAULT 1,
    rotation FLOAT NOT NULL DEFAULT 0,
    z_index INT NOT NULL DEFAULT 1,
    width INT,
    locked BOOLEAN DEFAULT FALSE,
    style_overrides JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Generation jobs tracking (async BFL image generation)
CREATE TABLE IF NOT EXISTS carousel_generation_jobs (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    carousel_id UUID NOT NULL REFERENCES carousels(id) ON DELETE CASCADE,
    slide_id UUID REFERENCES carousel_slides(id) ON DELETE CASCADE,
    bfl_task_id TEXT,
    status VARCHAR(20) NOT NULL DEFAULT 'pending',  -- 'pending', 'processing', 'completed', 'failed'
    error_message TEXT,
    result_url TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    completed_at TIMESTAMPTZ
);

-- Indexes for performance
CREATE INDEX IF NOT EXISTS idx_carousels_user ON carousels(user_id);
CREATE INDEX IF NOT EXISTS idx_carousels_created ON carousels(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_slides_carousel ON carousel_slides(carousel_id);
CREATE INDEX IF NOT EXISTS idx_slides_position ON carousel_slides(carousel_id, position);
CREATE INDEX IF NOT EXISTS idx_elements_slide ON slide_elements(slide_id);
CREATE INDEX IF NOT EXISTS idx_jobs_carousel ON carousel_generation_jobs(carousel_id);
CREATE INDEX IF NOT EXISTS idx_jobs_status ON carousel_generation_jobs(status) WHERE status IN ('pending', 'processing');
CREATE INDEX IF NOT EXISTS idx_jobs_bfl_task ON carousel_generation_jobs(bfl_task_id) WHERE bfl_task_id IS NOT NULL;

-- Update trigger for updated_at
CREATE OR REPLACE FUNCTION update_carousel_timestamp()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS carousel_updated_at ON carousels;
CREATE TRIGGER carousel_updated_at
    BEFORE UPDATE ON carousels
    FOR EACH ROW
    EXECUTE FUNCTION update_carousel_timestamp();
