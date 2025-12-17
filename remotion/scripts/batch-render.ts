#!/usr/bin/env npx ts-node
/**
 * Batch Render Script
 *
 * CLI for generating and rendering template variations.
 *
 * Usage:
 *   npx ts-node scripts/batch-render.ts --template talking-head --limit 10 --dry-run
 *   npx ts-node scripts/batch-render.ts --template talking-head --limit 100 --output ./renders
 *
 * Options:
 *   --template    Template ID (default: talking-head)
 *   --limit       Maximum variants to generate (default: 10)
 *   --min-priority Minimum priority score (default: 0)
 *   --output      Output directory (default: ./out)
 *   --dry-run     Only generate variants, don't render
 *   --concurrency Number of parallel renders (default: 3)
 *   --duration    Video duration in seconds (default: 30)
 *   --test-mode   Use placeholder assets
 */

import { bundle } from '@remotion/bundler';
import { renderMedia, selectComposition } from '@remotion/renderer';
import path from 'path';
import fs from 'fs';
import { fileURLToPath } from 'url';

// ESM compatibility - __dirname is not available in ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Import factory
import {
  generateVariations,
  loadTemplate,
  estimateRenderCost,
  type TemplateVariant,
  type Template,
} from '../src/factory';

// ============================================================
// CLI Arguments Parser
// ============================================================

interface CliArgs {
  template: string;
  limit: number;
  minPriority: number;
  output: string;
  dryRun: boolean;
  concurrency: number;
  duration: number;
  testMode: boolean;
  verbose: boolean;
}

function parseArgs(): CliArgs {
  const args = process.argv.slice(2);
  const result: CliArgs = {
    template: 'talking-head',
    limit: 10,
    minPriority: 0,
    output: './out',
    dryRun: false,
    concurrency: 3,
    duration: 30,
    testMode: false,
    verbose: false,
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
      case '--dry-run':
      case '-d':
        result.dryRun = true;
        break;
      case '--concurrency':
      case '-c':
        result.concurrency = parseInt(nextArg, 10);
        i++;
        break;
      case '--duration':
        result.duration = parseInt(nextArg, 10);
        i++;
        break;
      case '--test-mode':
        result.testMode = true;
        break;
      case '--verbose':
      case '-v':
        result.verbose = true;
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
Batch Render - Vibe Reels Template Factory

Usage:
  npx ts-node scripts/batch-render.ts [options]

Options:
  -t, --template <id>     Template ID (default: talking-head)
  -l, --limit <n>         Maximum variants to generate (default: 10)
  --min-priority <n>      Minimum priority score 0-100 (default: 0)
  -o, --output <dir>      Output directory (default: ./out)
  -d, --dry-run           Generate variants without rendering
  -c, --concurrency <n>   Parallel renders (default: 3)
  --duration <sec>        Video duration in seconds (default: 30)
  --test-mode             Use placeholder test assets
  -v, --verbose           Verbose output
  -h, --help              Show this help

Examples:
  # Generate 10 variants (dry run)
  npx ts-node scripts/batch-render.ts --dry-run --limit 10

  # Render 100 variants with high priority
  npx ts-node scripts/batch-render.ts --limit 100 --min-priority 20

  # Test mode with placeholders
  npx ts-node scripts/batch-render.ts --test-mode --limit 5
`);
}

// ============================================================
// Batch Job Types
// ============================================================

interface BatchJob {
  id: string;
  variant: TemplateVariant;
  status: 'pending' | 'rendering' | 'completed' | 'failed';
  startedAt?: Date;
  completedAt?: Date;
  outputPath?: string;
  error?: string;
}

interface BatchResult {
  batchId: string;
  templateId: string;
  totalVariants: number;
  completed: number;
  failed: number;
  startedAt: Date;
  completedAt?: Date;
  jobs: BatchJob[];
  outputDir: string;
  estimatedCost: number;
  actualDuration?: number;
}

// ============================================================
// Main Functions
// ============================================================

async function main(): Promise<void> {
  const args = parseArgs();

  console.log('\n🎬 Vibe Reels Batch Render\n');
  console.log('Configuration:');
  console.log(`  Template:    ${args.template}`);
  console.log(`  Limit:       ${args.limit}`);
  console.log(`  Min Priority: ${args.minPriority}`);
  console.log(`  Output:      ${args.output}`);
  console.log(`  Dry Run:     ${args.dryRun}`);
  console.log(`  Concurrency: ${args.concurrency}`);
  console.log(`  Duration:    ${args.duration}s`);
  console.log(`  Test Mode:   ${args.testMode}`);
  console.log('');

  // Load template
  console.log('📝 Loading template...');
  let template: Template;
  try {
    template = loadTemplate(args.template as 'talking-head') as unknown as Template;
    console.log(`  ✓ Loaded: ${template.name} v${template.version}`);
  } catch (err) {
    console.error(`  ✗ Failed to load template: ${err}`);
    process.exit(1);
  }

  // Generate variations
  console.log('\n🔄 Generating variations...');
  const result = generateVariations(template, {
    limit: args.limit,
    minPriority: args.minPriority,
  });

  console.log(`  Total possible:   ${result.totalPossible}`);
  console.log(`  After exclusions: ${result.afterExclusions}`);
  console.log(`  Final count:      ${result.finalCount}`);
  console.log('');
  console.log('  Statistics:');
  console.log(`    Avg Priority: ${result.stats.avgPriority}`);
  console.log(`    Max Priority: ${result.stats.maxPriority}`);
  console.log(`    Min Priority: ${result.stats.minPriority}`);

  // Estimate cost
  const costEstimate = estimateRenderCost(result.variants, args.duration, 30);
  console.log('\n💰 Cost Estimate:');
  console.log(`  Total Frames:  ${costEstimate.totalFrames.toLocaleString()}`);
  console.log(`  Total Minutes: ${costEstimate.totalMinutes.toFixed(1)}`);
  console.log(`  Est. Cost:     $${costEstimate.estimatedCost}`);

  if (args.verbose) {
    console.log('\n📊 Distribution:');
    console.log('  By Avatar Position:', result.stats.byAvatarPosition);
    console.log('  By Hook Style:', result.stats.byHookStyle);
    console.log('  By Caption Style:', result.stats.byCaptionStyle);
    console.log('  By B-Roll Pattern:', result.stats.byBRollPattern);
  }

  // List variants
  console.log('\n📋 Variants:');
  for (const variant of result.variants.slice(0, 20)) {
    console.log(`  ${variant.id} (priority: ${variant.priority})`);
  }
  if (result.variants.length > 20) {
    console.log(`  ... and ${result.variants.length - 20} more`);
  }

  // Dry run - stop here
  if (args.dryRun) {
    console.log('\n✅ Dry run complete. No renders were executed.');

    // Save manifest
    const manifestPath = path.join(args.output, 'manifest.json');
    await ensureDir(args.output);
    fs.writeFileSync(
      manifestPath,
      JSON.stringify(
        {
          generatedAt: new Date().toISOString(),
          template: args.template,
          variants: result.variants,
          stats: result.stats,
          costEstimate,
        },
        null,
        2
      )
    );
    console.log(`  Manifest saved: ${manifestPath}`);
    return;
  }

  // Execute batch render
  console.log('\n🚀 Starting batch render...\n');
  const batchResult = await executeBatchRender(
    template,
    result.variants,
    args
  );

  // Summary
  console.log('\n📊 Batch Complete:');
  console.log(`  Completed: ${batchResult.completed}/${batchResult.totalVariants}`);
  console.log(`  Failed:    ${batchResult.failed}`);
  if (batchResult.actualDuration) {
    console.log(`  Duration:  ${batchResult.actualDuration.toFixed(1)}s`);
  }
  console.log(`  Output:    ${batchResult.outputDir}`);
}

// ============================================================
// Batch Execution
// ============================================================

async function executeBatchRender(
  template: Template,
  variants: TemplateVariant[],
  args: CliArgs
): Promise<BatchResult> {
  const batchId = `batch-${Date.now()}`;
  const outputDir = path.resolve(args.output, batchId);
  await ensureDir(outputDir);
  await ensureDir(path.join(outputDir, 'videos'));
  await ensureDir(path.join(outputDir, 'thumbnails'));

  const startedAt = new Date();

  // Create jobs
  const jobs: BatchJob[] = variants.map((variant) => ({
    id: variant.id,
    variant,
    status: 'pending' as const,
  }));

  // Bundle the project
  console.log('📦 Bundling Remotion project...');
  const bundleLocation = await bundle({
    entryPoint: path.resolve(__dirname, '../src/index.ts'),
    publicDir: path.resolve(__dirname, '../public'),  // Include static assets
    onProgress: (progress: number) => {
      if (args.verbose) {
        process.stdout.write(`\r  Bundling: ${Math.round(progress * 100)}%`);
      }
    },
  });
  console.log('\n  ✓ Bundle complete');

  // Process jobs with concurrency
  const results: BatchJob[] = [];
  const queue = [...jobs];

  const processJob = async (job: BatchJob): Promise<void> => {
    job.status = 'rendering';
    job.startedAt = new Date();

    const outputPath = path.join(outputDir, 'videos', `${job.id}.mp4`);

    try {
      console.log(`  ⏳ Rendering ${job.id}...`);

      // Get composition
      const composition = await selectComposition({
        serveUrl: bundleLocation,
        id: 'FactoryTalkingHead',
        inputProps: resolveAssets(job.variant.props, args.testMode),
      });

      // Override duration
      const durationInFrames = args.duration * 30;

      // Render
      await renderMedia({
        composition: {
          ...composition,
          durationInFrames,
        },
        serveUrl: bundleLocation,
        codec: 'h264',
        outputLocation: outputPath,
        inputProps: resolveAssets(job.variant.props, args.testMode),
        onProgress: ({ progress }) => {
          if (args.verbose) {
            process.stdout.write(
              `\r    Progress: ${Math.round(progress * 100)}%`
            );
          }
        },
      });

      job.status = 'completed';
      job.outputPath = outputPath;
      console.log(`  ✓ ${job.id} complete`);
    } catch (err) {
      job.status = 'failed';
      job.error = err instanceof Error ? err.message : String(err);
      console.log(`  ✗ ${job.id} failed: ${job.error}`);
    }

    job.completedAt = new Date();
    results.push(job);
  };

  // Process with concurrency
  const activeJobs: Promise<void>[] = [];

  while (queue.length > 0 || activeJobs.length > 0) {
    // Start new jobs if we have capacity
    while (activeJobs.length < args.concurrency && queue.length > 0) {
      const job = queue.shift()!;
      const promise = processJob(job).then(() => {
        const index = activeJobs.indexOf(promise);
        if (index > -1) activeJobs.splice(index, 1);
      });
      activeJobs.push(promise);
    }

    // Wait for at least one job to complete
    if (activeJobs.length > 0) {
      await Promise.race(activeJobs);
    }
  }

  const completedAt = new Date();
  const actualDuration = (completedAt.getTime() - startedAt.getTime()) / 1000;

  const batchResult: BatchResult = {
    batchId,
    templateId: template.id,
    totalVariants: variants.length,
    completed: results.filter((j) => j.status === 'completed').length,
    failed: results.filter((j) => j.status === 'failed').length,
    startedAt,
    completedAt,
    jobs: results,
    outputDir,
    estimatedCost: estimateRenderCost(variants, args.duration, 30).estimatedCost,
    actualDuration,
  };

  // Save batch result
  const resultPath = path.join(outputDir, 'result.json');
  fs.writeFileSync(resultPath, JSON.stringify(batchResult, null, 2));

  return batchResult;
}

// ============================================================
// Asset Resolution
// ============================================================

function resolveAssets(
  props: Record<string, unknown>,
  testMode: boolean
): Record<string, unknown> {
  // Always add sample content for demos
  const resolved = addSampleContent({ ...props });

  if (!testMode) {
    return resolved;
  }

  // Test assets (using /public/ prefix for bundled assets)
  const testAssets = {
    lipSyncVideo: '/public/test/test-avatar.mp4',
    backgroundVideos: [
      '/public/test/test-broll-01.mp4',
      '/public/test/test-broll-02.mp4',
      '/public/test/test-broll-03.mp4',
      '/public/test/test-broll-04.mp4',
    ],
    backgroundMusic: '/public/test/test-music.mp3',
  };

  // Replace placeholder patterns
  const lipSync = resolved.lipSyncVideo as string;
  if (lipSync?.includes('{{') || !lipSync) {
    resolved.lipSyncVideo = testAssets.lipSyncVideo;
  }

  const videos = resolved.backgroundVideos as string[];
  if (videos?.some((v) => v?.includes('{{'))) {
    resolved.backgroundVideos = testAssets.backgroundVideos;
  }

  const music = resolved.backgroundMusic as string;
  if (music?.includes('{{') || !music) {
    resolved.backgroundMusic = testAssets.backgroundMusic;
  }

  return resolved;
}

/**
 * Add sample content for demo renders
 * Adds hook text, captions, and B-roll videos if not provided
 */
function addSampleContent(props: Record<string, unknown>): Record<string, unknown> {
  const resolved = { ...props };

  // Add sample hook text if empty
  const hookText = resolved.hookText as string;
  if (!hookText) {
    resolved.hookText = '🔥 Pro Tip';
  }

  // Add sample captions if empty
  const captions = resolved.captions as Array<{ text: string; startFrame: number; endFrame: number }>;
  if (!captions || captions.length === 0) {
    resolved.captions = [
      { text: 'This is how factory variations work', startFrame: 60, endFrame: 150 },
      { text: 'Each variant has unique style', startFrame: 180, endFrame: 270 },
      { text: 'Avatar • Hook • Captions • B-Roll', startFrame: 300, endFrame: 390 },
    ];
  }

  // Add sample B-roll videos if placeholders or empty
  // Note: Keep these as placeholders - they get resolved by resolveMediaPath in components
  const bgVideos = resolved.backgroundVideos as string[];
  if (!bgVideos || bgVideos.length === 0 || !bgVideos.some((v) => v?.includes('{{'))) {
    // Set as placeholders - will be resolved by TemplateFactory's resolveMediaPath
    resolved.backgroundVideos = [
      '{{broll_01}}',
      '{{broll_02}}',
      '{{broll_03}}',
      '{{broll_04}}',
    ];
  }

  // Keep lipSyncVideo as placeholder - resolveMediaPath in AvatarMorph handles it
  // Don't override here as direct paths bypass staticFile resolution

  return resolved;
}

// ============================================================
// Utilities
// ============================================================

async function ensureDir(dir: string): Promise<void> {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}

// ============================================================
// Run
// ============================================================

main().catch((err) => {
  console.error('\n❌ Error:', err);
  process.exit(1);
});
