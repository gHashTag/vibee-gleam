import { Client } from 'pg';

const DATABASE_URL = "postgresql://neondb_owner:npg_A9z2dErbkfhw@ep-bitter-frog-a1bewei7.ap-southeast-1.aws.neon.tech/neondb?sslmode=require";

async function checkSchema() {
  const db = new Client({ connectionString: DATABASE_URL });
  await db.connect();

  console.log("=== Database Schema Check ===\n");

  // Check telegram_dialogs
  console.log("1. telegram_dialogs columns:");
  const dialogsCols = await db.query(`
    SELECT column_name, data_type, is_nullable
    FROM information_schema.columns
    WHERE table_name = 'telegram_dialogs'
    ORDER BY ordinal_position
  `);
  dialogsCols.rows.forEach(r => console.log(`   ${r.column_name}: ${r.data_type} (${r.is_nullable})`));

  // Check telegram_messages
  console.log("\n2. telegram_messages columns:");
  const msgCols = await db.query(`
    SELECT column_name, data_type, is_nullable
    FROM information_schema.columns
    WHERE table_name = 'telegram_messages'
    ORDER BY ordinal_position
  `);
  msgCols.rows.forEach(r => console.log(`   ${r.column_name}: ${r.data_type} (${r.is_nullable})`));

  // Check if tables exist
  console.log("\n3. Tables count:");
  const tables = await db.query(`
    SELECT table_name FROM information_schema.tables
    WHERE table_schema = 'public' AND table_type = 'BASE TABLE'
  `);
  tables.rows.forEach(r => console.log(`   ${r.table_name}`));

  await db.end();
}

checkSchema().catch(console.error);
