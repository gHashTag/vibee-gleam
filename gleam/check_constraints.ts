import { Client } from 'pg';

const DATABASE_URL = "postgresql://neondb_owner:npg_A9z2dErbkfhw@ep-bitter-frog-a1bewei7.ap-southeast-1.aws.neon.tech/neondb?sslmode=require";

async function checkConstraints() {
  const db = new Client({ connectionString: DATABASE_URL });
  await db.connect();

  console.log("=== Check Constraints ===\n");

  const result = await db.query(`
    SELECT conname, pg_get_constraintdef(oid)
    FROM pg_constraint
    WHERE conrelid = 'telegram_dialogs'::regclass
  `);

  result.rows.forEach(r => {
    console.log(`${r.conname}:`);
    console.log(`  ${r.pg_get_constraintdef}\n`);
  });

  await db.end();
}

checkConstraints().catch(console.error);
