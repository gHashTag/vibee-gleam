-- Migration 005: GitHub Documents for Digital Clone Knowledge Base
-- Stores GitHub profile data (users, repos, READMEs) with vector embeddings

-- Enable pgvector extension if not exists
CREATE EXTENSION IF NOT EXISTS vector;

-- GitHub documents table
CREATE TABLE IF NOT EXISTS github_documents (
    id SERIAL PRIMARY KEY,

    -- Owner identification
    username VARCHAR(255) NOT NULL,

    -- Document type: 'profile', 'repo', 'readme', 'languages'
    doc_type VARCHAR(50) NOT NULL,

    -- Repository name (NULL for profile docs)
    repo_name VARCHAR(255),

    -- Content
    title TEXT NOT NULL,
    content TEXT NOT NULL,
    source_url TEXT,

    -- Metadata
    tags TEXT[] DEFAULT '{}',
    stars INT DEFAULT 0,
    language VARCHAR(100),
    topics TEXT[] DEFAULT '{}',

    -- Vector embedding for semantic search (OpenAI text-embedding-3-small = 1536 dims)
    embedding vector(1536),

    -- Timestamps
    fetched_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),

    -- Constraints
    CONSTRAINT github_docs_valid_type CHECK (
        doc_type IN ('profile', 'repo', 'readme', 'languages')
    )
);

-- Indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_github_docs_username
    ON github_documents(username);

CREATE INDEX IF NOT EXISTS idx_github_docs_type
    ON github_documents(doc_type);

CREATE INDEX IF NOT EXISTS idx_github_docs_repo
    ON github_documents(repo_name);

CREATE INDEX IF NOT EXISTS idx_github_docs_language
    ON github_documents(language);

CREATE INDEX IF NOT EXISTS idx_github_docs_stars
    ON github_documents(stars DESC);

-- Vector index for semantic search (IVFFlat with cosine distance)
CREATE INDEX IF NOT EXISTS idx_github_docs_embedding
    ON github_documents
    USING ivfflat (embedding vector_cosine_ops)
    WITH (lists = 100);

-- Full-text search index
CREATE INDEX IF NOT EXISTS idx_github_docs_content_fts
    ON github_documents
    USING gin(to_tsvector('english', title || ' ' || content));

-- GIN index for tags array search
CREATE INDEX IF NOT EXISTS idx_github_docs_tags
    ON github_documents
    USING gin(tags);

-- GIN index for topics array search
CREATE INDEX IF NOT EXISTS idx_github_docs_topics
    ON github_documents
    USING gin(topics);

-- Composite index for common queries
CREATE INDEX IF NOT EXISTS idx_github_docs_username_type
    ON github_documents(username, doc_type);

-- Unique constraint to prevent duplicates
CREATE UNIQUE INDEX IF NOT EXISTS idx_github_docs_unique
    ON github_documents(username, doc_type, COALESCE(repo_name, '__profile__'));

-- Comments
COMMENT ON TABLE github_documents IS 'GitHub profile data indexed for digital clone knowledge base RAG';
COMMENT ON COLUMN github_documents.embedding IS 'OpenAI text-embedding-3-small vector (1536 dimensions) for semantic search';
COMMENT ON COLUMN github_documents.doc_type IS 'profile=user bio, repo=repository info, readme=README content, languages=language stats';

-- Function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_github_docs_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger to auto-update updated_at
DROP TRIGGER IF EXISTS github_docs_updated_at ON github_documents;
CREATE TRIGGER github_docs_updated_at
    BEFORE UPDATE ON github_documents
    FOR EACH ROW
    EXECUTE FUNCTION update_github_docs_updated_at();
