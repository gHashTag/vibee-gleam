#!/usr/bin/env npx ts-node
/**
 * Generate Variants Script
 *
 * CLI for generating template variations without rendering.
 * Outputs a JSON manifest of all variants.
 *
 * Usage:
 *   npx ts-node scripts/generate-variants.ts --template talking-head --limit 100
 *   npx ts-node scripts/generate-variants.ts --limit 50 --output variants.json
 */

import path from 'path';
import fs from 'fs';

import {
  generateVariations,
  loadTemplate,
  estimateRenderCost,
  type Template,
} from '../src/factory';

// ============================================================
// CLI Arguments
// ============================================================

interface CliArgs {
  template: string;
  limit: number;
  minPriority: number;
  output: string;
  format: 'json' | 'csv' | 'table';
  axes: string[];
}

function parseArgs(): CliArgs {
  const args = process.argv.slice(2);
  const result: CliArgs = {
    template: 'talking-head',
    limit: 100,
    minPriority: 0,
    output: '',
    format: 'json',
    axes: ['avatarPosition', 'hookStyle', 'captionStyle', 'bRollPattern'],
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    const nextArg = args[i + 1];

    switch (arg) {
      case '--template':
      case '-t':
        result.template = nextArg;
        i++;
        break;
      case '--limit':
      case '-l':
        result.limit = parseInt(nextArg, 10);
        i++;
        break;
      case '--min-priority':
        result.minPriority = parseInt(nextArg, 10);
        i++;
        break;
      case '--output':
      case '-o':
        result.output = nextArg;
        i++;
        break;
      case '--format':
      case '-f':
        result.format = nextArg as 'json' | 'csv' | 'table';
        i++;
        break;
      case '--axes':
        result.axes = nextArg.split(',');
        i++;
        break;
      case '--help':
      case '-h':
        printHelp();
        process.exit(0);
    }
  }

  return result;
}

function printHelp(): void {
  console.log(`
Generate Variants - Vibe Reels Template Factory

Usage:
  npx ts-node scripts/generate-variants.ts [options]

Options:
  -t, --template <id>    Template ID (default: talking-head)
  -l, --limit <n>        Maximum variants (default: 100)
  --min-priority <n>     Minimum priority 0-100 (default: 0)
  -o, --output <file>    Output file (default: stdout)
  -f, --format <type>    Output format: json, csv, table (default: json)
  --axes <list>          Axes to vary (comma-separated)
  -h, --help             Show this help

Examples:
  # Generate 100 variants as JSON
  npx ts-node scripts/generate-variants.ts --limit 100 -o variants.json

  # Generate only avatar + hook variations
  npx ts-node scripts/generate-variants.ts --axes avatarPosition,hookStyle

  # Show as table
  npx ts-node scripts/generate-variants.ts --format table --limit 20
`);
}

// ============================================================
// Main
// ============================================================

async function main(): Promise<void> {
  const args = parseArgs();

  // Load template
  let template: Template;
  try {
    template = loadTemplate(args.template as 'talking-head') as unknown as Template;
  } catch (err) {
    console.error(`Error loading template: ${err}`);
    process.exit(1);
  }

  // Generate variations
  const validAxes = args.axes.filter((a) =>
    ['avatarPosition', 'hookStyle', 'captionStyle', 'bRollPattern'].includes(a)
  ) as Array<'avatarPosition' | 'hookStyle' | 'captionStyle' | 'bRollPattern'>;

  const result = generateVariations(template, {
    limit: args.limit,
    minPriority: args.minPriority,
    varyAxes: validAxes,
  });

  // Cost estimate
  const costEstimate = estimateRenderCost(result.variants, 30, 30);

  // Format output
  let output: string;

  switch (args.format) {
    case 'csv':
      output = formatAsCSV(result);
      break;
    case 'table':
      output = formatAsTable(result);
      break;
    case 'json':
    default:
      output = JSON.stringify(
        {
          generatedAt: new Date().toISOString(),
          template: {
            id: template.id,
            name: template.name,
            version: template.version,
          },
          statistics: {
            totalPossible: result.totalPossible,
            afterExclusions: result.afterExclusions,
            finalCount: result.finalCount,
            ...result.stats,
          },
          costEstimate,
          variants: result.variants,
        },
        null,
        2
      );
  }

  // Output
  if (args.output) {
    const outputPath = path.resolve(args.output);
    fs.writeFileSync(outputPath, output);
    console.error(`✓ Written to ${outputPath}`);
    console.error(`  Variants: ${result.finalCount}`);
    console.error(`  Est. Cost: $${costEstimate.estimatedCost}`);
  } else {
    console.log(output);
  }
}

// ============================================================
// Formatters
// ============================================================

function formatAsCSV(result: ReturnType<typeof generateVariations>): string {
  const headers = [
    'id',
    'priority',
    'avatarPosition',
    'hookStyle',
    'captionStyle',
    'bRollPattern',
  ];

  const rows = result.variants.map((v) => [
    v.id,
    v.priority.toString(),
    v.axes.avatarPosition,
    v.axes.hookStyle,
    v.axes.captionStyle,
    v.axes.bRollPattern,
  ]);

  return [headers.join(','), ...rows.map((r) => r.join(','))].join('\n');
}

function formatAsTable(result: ReturnType<typeof generateVariations>): string {
  const lines: string[] = [];

  // Header
  lines.push('┌' + '─'.repeat(78) + '┐');
  lines.push(
    `│ Vibe Reels Variants (${result.finalCount} of ${result.totalPossible})`.padEnd(78) + ' │'
  );
  lines.push('├' + '─'.repeat(78) + '┤');

  // Stats
  lines.push(
    `│ Avg Priority: ${result.stats.avgPriority} | Max: ${result.stats.maxPriority} | Min: ${result.stats.minPriority}`.padEnd(
      78
    ) + ' │'
  );
  lines.push('├' + '─'.repeat(78) + '┤');

  // Column headers
  lines.push(
    '│ ' +
      'ID'.padEnd(24) +
      'Pri'.padEnd(5) +
      'Avatar'.padEnd(15) +
      'Hook'.padEnd(12) +
      'Caption'.padEnd(12) +
      'BRoll'.padEnd(8) +
      '│'
  );
  lines.push('├' + '─'.repeat(78) + '┤');

  // Rows
  for (const v of result.variants) {
    lines.push(
      '│ ' +
        v.id.padEnd(24) +
        v.priority.toString().padEnd(5) +
        v.axes.avatarPosition.slice(0, 13).padEnd(15) +
        v.axes.hookStyle.slice(0, 10).padEnd(12) +
        v.axes.captionStyle.slice(0, 10).padEnd(12) +
        v.axes.bRollPattern.slice(0, 6).padEnd(8) +
        '│'
    );
  }

  lines.push('└' + '─'.repeat(78) + '┘');

  return lines.join('\n');
}

// ============================================================
// Run
// ============================================================

main().catch((err) => {
  console.error('Error:', err);
  process.exit(1);
});
