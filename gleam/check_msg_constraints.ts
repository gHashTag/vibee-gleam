import { Client } from 'pg';

const DATABASE_URL = "postgresql://neondb_owner:npg_A9z2dErbkfhw@ep-bitter-frog-a1bewei7.ap-southeast-1.aws.neon.tech/neondb?sslmode=require";

async function checkConstraints() {
  const db = new Client({ connectionString: DATABASE_URL });
  await db.connect();

  console.log("=== telegram_messages Constraints ===\n");

  const result = await db.query(`
    SELECT conname, pg_get_constraintdef(oid)
    FROM pg_constraint
    WHERE conrelid = 'telegram_messages'::regclass
  `);

  result.rows.forEach(r => {
    console.log(`${r.conname}:`);
    console.log(`  ${r.pg_get_constraintdef}\n`);
  });

  // Check current message count
  const count = await db.query(`SELECT COUNT(*) FROM telegram_messages`);
  console.log(`Total messages in DB: ${count.rows[0].count}`);

  await db.end();
}

checkConstraints().catch(console.error);
