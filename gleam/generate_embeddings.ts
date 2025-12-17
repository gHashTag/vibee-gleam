#!/usr/bin/env bun
// Generate embeddings for messages without them

const DATABASE_URL = "postgresql://neondb_owner:REDACTED_DB_PASSWORD@ep-bitter-frog-a1bewei7.ap-southeast-1.aws.neon.tech/neondb?sslmode=require";
const OPENAI_API_KEY = process.env.OPENAI_API_KEY || ""; // Set via env

import { Client } from 'pg';

interface Message {
  id: bigint;
  text_content: string;
}

async function getEmbedding(text: string): Promise<number[]> {
  // Use Ollama locally (free) or OpenAI
  if (OPENAI_API_KEY) {
    const resp = await fetch("https://api.openai.com/v1/embeddings", {
      method: "POST",
      headers: {
        "Authorization": `Bearer ${OPENAI_API_KEY}`,
        "Content-Type": "application/json"
      },
      body: JSON.stringify({
        model: "text-embedding-3-small",
        input: text.slice(0, 8000) // Max input length
      })
    });
    const data = await resp.json() as any;
    if (data.error) {
      throw new Error(data.error.message);
    }
    return data.data[0].embedding;
  } else {
    // Use Ollama locally
    const resp = await fetch("http://localhost:11434/api/embeddings", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        model: "nomic-embed-text",
        prompt: text.slice(0, 8000)
      })
    });
    const data = await resp.json() as any;
    return data.embedding;
  }
}

async function main() {
  console.log("=== Embedding Generator ===\n");

  const db = new Client({ connectionString: DATABASE_URL });
  await db.connect();

  // Get messages without embeddings
  const result = await db.query(`
    SELECT id, text_content
    FROM telegram_messages
    WHERE embedding IS NULL
      AND text_content IS NOT NULL
      AND text_content != ''
    ORDER BY timestamp DESC
    LIMIT 1000
  `);

  const messages = result.rows as Message[];
  console.log(`Found ${messages.length} messages without embeddings\n`);

  if (messages.length === 0) {
    console.log("All messages have embeddings!");
    await db.end();
    return;
  }

  let processed = 0;
  let errors = 0;

  for (const msg of messages) {
    try {
      const text = msg.text_content;
      if (!text || text.length < 10) {
        continue;
      }

      const embedding = await getEmbedding(text);

      // Convert to postgres vector format
      const vectorStr = `[${embedding.join(',')}]`;

      await db.query(`
        UPDATE telegram_messages
        SET embedding = $1::vector,
            embedding_model = $2,
            embedding_created_at = NOW()
        WHERE id = $3
      `, [vectorStr, OPENAI_API_KEY ? 'text-embedding-3-small' : 'nomic-embed-text', msg.id]);

      processed++;
      if (processed % 50 === 0) {
        console.log(`Processed ${processed}/${messages.length} messages`);
      }

      // Rate limit
      await new Promise(r => setTimeout(r, 100));

    } catch (e: any) {
      console.log(`Error for message ${msg.id}: ${e.message}`);
      errors++;
      if (errors > 10) {
        console.log("Too many errors, stopping");
        break;
      }
    }
  }

  console.log(`\n=== COMPLETE ===`);
  console.log(`Processed: ${processed}`);
  console.log(`Errors: ${errors}`);

  await db.end();
}

main().catch(console.error);
