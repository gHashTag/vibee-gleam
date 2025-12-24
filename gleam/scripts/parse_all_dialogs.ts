#!/usr/bin/env npx tsx
// Full Telegram Dialog Parser - Direct API version
// This script parses ALL dialogs and saves to database

const API_KEY = "vibee-mcp-secret-2024";
const BRIDGE_URL = "https://vibee-telegram-bridge.fly.dev";
const SESSION_ID = "sess_deyznjf1vzn6"; // NEURO_SAGE
const DATABASE_URL = "postgresql://neondb_owner:npg_A9z2dErbkfhw@ep-bitter-frog-a1bewei7.ap-southeast-1.aws.neon.tech/neondb?sslmode=require";

import { Client } from 'pg';

interface Dialog {
  id: number;
  title: string;
  type: string;
  unread_count?: number;
}

interface Message {
  id: number;
  date: string;
  text: string;
  from_id?: number;
  from_name?: string;
  media_type?: string;
}

async function fetchFromBridge(path: string, method = "GET", body?: any): Promise<any> {
  const url = `${BRIDGE_URL}${path}`;
  const opts: RequestInit = {
    method,
    headers: {
      "Authorization": `Bearer ${API_KEY}`,
      "X-Session-ID": SESSION_ID,
      "Content-Type": "application/json"
    }
  };
  if (body) {
    opts.body = JSON.stringify(body);
  }

  const resp = await fetch(url, opts);
  if (!resp.ok) {
    const errText = await resp.text();
    throw new Error(`Bridge error ${resp.status}: ${errText}`);
  }
  return resp.json();
}

async function sleep(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function main() {
  console.log("=== Full Telegram Dialog Parser ===\n");

  // Connect to database
  console.log("1. Connecting to database...");
  const db = new Client({ connectionString: DATABASE_URL });
  await db.connect();
  console.log("   Connected!\n");

  // Get all dialogs
  console.log("2. Fetching dialogs from Telegram...");
  const dialogsResp = await fetchFromBridge("/api/v1/dialogs?limit=200");
  const dialogs: Dialog[] = dialogsResp.dialogs || [];
  console.log(`   Found ${dialogs.length} dialogs\n`);

  // Filter only user dialogs (personal chats)
  const userDialogs = dialogs.filter(d => d.type === "user");
  console.log(`3. Found ${userDialogs.length} personal user chats\n`);

  let totalMessages = 0;
  let totalVoice = 0;
  let errors = 0;

  for (let i = 0; i < userDialogs.length; i++) {
    const dialog = userDialogs[i];
    console.log(`\n[${i+1}/${userDialogs.length}] Processing: ${dialog.title} (ID: ${dialog.id})`);

    try {
      // Save dialog to database (matching actual schema)
      await db.query(`
        INSERT INTO telegram_dialogs (id, type, title, parse_status)
        VALUES ($1, $2, $3, 'in_progress')
        ON CONFLICT (id) DO UPDATE SET
          title = EXCLUDED.title,
          parse_status = 'in_progress',
          updated_at = NOW()
      `, [
        dialog.id,
        dialog.type,
        dialog.title
      ]);

      // Fetch messages (handle pagination)
      let offsetId = 0;
      let messageCount = 0;
      let voiceCount = 0;
      let hasMore = true;

      while (hasMore) {
        try {
          const msgPath = `/api/v1/history/${dialog.id}?limit=100${offsetId ? `&offset_id=${offsetId}` : ''}`;
          const msgsResp = await fetchFromBridge(msgPath);
          const messages: Message[] = msgsResp.messages || [];

          if (messages.length === 0) {
            hasMore = false;
            break;
          }

          for (const msg of messages) {
            // Map content type to valid values
            const validContentTypes = ['text', 'photo', 'voice', 'video', 'document', 'sticker', 'animation', 'video_note', 'audio', 'contact', 'location', 'poll', 'other'];
            let contentType = (msg.media_type || 'text').toLowerCase();
            if (!validContentTypes.includes(contentType)) {
              contentType = 'other';
            }

            // Save message (matching actual schema)
            // Use dialog_id + message_id as unique key
            try {
              await db.query(`
                INSERT INTO telegram_messages (id, message_id, dialog_id, sender_id, sender_name, content_type, text_content, timestamp)
                VALUES (DEFAULT, $1, $2, $3, $4, $5, $6, $7)
                ON CONFLICT (dialog_id, message_id) DO UPDATE SET
                  text_content = EXCLUDED.text_content,
                  content_type = EXCLUDED.content_type
              `, [
                msg.id, // message_id
                dialog.id,
                msg.from_id || null,
                msg.from_name || null,
                contentType,
                msg.text || '',
                msg.date
              ]);
            } catch (insertErr: any) {
              // Ignore duplicate errors, continue
              if (!insertErr.message.includes('duplicate')) {
                throw insertErr;
              }
            }

            messageCount++;
            if (msg.media_type === 'voice' || msg.media_type === 'audio') {
              voiceCount++;
            }
          }

          // Update offset for pagination
          offsetId = messages[messages.length - 1].id;

          // Check if we have more (less than limit means end)
          if (messages.length < 100) {
            hasMore = false;
          }

          // Rate limiting - wait between batches
          await sleep(500);

        } catch (e: any) {
          if (e.message.includes('FLOOD_WAIT')) {
            const waitMatch = e.message.match(/FLOOD_WAIT_(\d+)/);
            const waitSec = waitMatch ? parseInt(waitMatch[1]) : 60;
            console.log(`   ⏳ FLOOD_WAIT - sleeping ${waitSec}s...`);
            await sleep(waitSec * 1000);
          } else {
            console.log(`   ⚠️ Error fetching messages: ${e.message}`);
            hasMore = false;
          }
        }
      }

      totalMessages += messageCount;
      totalVoice += voiceCount;
      console.log(`   ✅ ${messageCount} messages (${voiceCount} voice)`);

      // Update dialog status to completed
      await db.query(`
        UPDATE telegram_dialogs
        SET parse_status = 'completed',
            parsed_messages = $1,
            total_messages = $1,
            last_parsed_at = NOW()
        WHERE id = $2
      `, [messageCount, dialog.id]);

      // Add delay between dialogs
      await sleep(1000);

    } catch (e: any) {
      console.log(`   ❌ Error: ${e.message}`);
      errors++;
    }
  }

  // Summary
  console.log("\n\n=== PARSING COMPLETE ===");
  console.log(`Dialogs processed: ${userDialogs.length}`);
  console.log(`Total messages: ${totalMessages}`);
  console.log(`Voice messages: ${totalVoice}`);
  console.log(`Errors: ${errors}`);

  await db.end();
  console.log("\nDatabase connection closed.");
}

main().catch(e => {
  console.error("Fatal error:", e);
  process.exit(1);
});
