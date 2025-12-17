#!/usr/bin/env npx ts-node
/**
 * Generate Preview Gallery
 *
 * Generates static HTML gallery with all variant previews.
 * Creates thumbnails for each variant using Remotion's still renderer.
 *
 * Usage:
 *   npx ts-node scripts/generate-previews.ts --limit 50 --output ./previews
 */

import { bundle } from '@remotion/bundler';
import { renderStill, selectComposition } from '@remotion/renderer';
import path from 'path';
import fs from 'fs';

import {
  generateVariations,
  loadTemplate,
  type Template,
  type TemplateVariant,
} from '../src/factory';

// ============================================================
// CLI Arguments
// ============================================================

interface CliArgs {
  template: string;
  limit: number;
  output: string;
  frameAt: number; // Frame number for thumbnail
  width: number;
  height: number;
}

function parseArgs(): CliArgs {
  const args = process.argv.slice(2);
  const result: CliArgs = {
    template: 'talking-head',
    limit: 50,
    output: './previews',
    frameAt: 90, // 3 seconds into video
    width: 540,
    height: 960,
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
      case '--output':
      case '-o':
        result.output = nextArg;
        i++;
        break;
      case '--frame':
        result.frameAt = parseInt(nextArg, 10);
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
Generate Previews - Vibe Reels Template Factory

Creates a static HTML gallery with thumbnails of all variants.

Usage:
  npx ts-node scripts/generate-previews.ts [options]

Options:
  -t, --template <id>   Template ID (default: talking-head)
  -l, --limit <n>       Maximum variants (default: 50)
  -o, --output <dir>    Output directory (default: ./previews)
  --frame <n>           Frame number for thumbnail (default: 90)
  -h, --help            Show this help

Output:
  ./previews/
  ├── index.html        # Gallery page
  ├── thumbnails/       # PNG thumbnails
  └── data.json         # Variant metadata
`);
}

// ============================================================
// Main
// ============================================================

async function main(): Promise<void> {
  const args = parseArgs();

  console.log('\n🖼️  Vibe Reels Preview Generator\n');

  // Load template
  let template: Template;
  try {
    template = loadTemplate(args.template as 'talking-head') as unknown as Template;
    console.log(`📝 Template: ${template.name}`);
  } catch (err) {
    console.error(`Error: ${err}`);
    process.exit(1);
  }

  // Generate variations
  const result = generateVariations(template, { limit: args.limit });
  console.log(`🔄 Generated ${result.variants.length} variants\n`);

  // Setup output directory
  const outputDir = path.resolve(args.output);
  const thumbnailDir = path.join(outputDir, 'thumbnails');
  await ensureDir(outputDir);
  await ensureDir(thumbnailDir);

  // Bundle project
  console.log('📦 Bundling...');
  const bundleLocation = await bundle({
    entryPoint: path.resolve(__dirname, '../src/index.ts'),
  });
  console.log('  ✓ Bundle ready\n');

  // Generate thumbnails
  console.log('🖼️  Generating thumbnails...');
  const thumbnails: Array<{ id: string; path: string }> = [];

  for (let i = 0; i < result.variants.length; i++) {
    const variant = result.variants[i];
    const thumbnailPath = path.join(thumbnailDir, `${variant.id}.png`);

    try {
      const composition = await selectComposition({
        serveUrl: bundleLocation,
        id: 'FactoryTalkingHead',
        inputProps: resolveTestAssets(variant.props),
      });

      await renderStill({
        composition,
        serveUrl: bundleLocation,
        output: thumbnailPath,
        frame: args.frameAt,
        scale: 0.5, // Half resolution for thumbnails
        inputProps: resolveTestAssets(variant.props),
      });

      thumbnails.push({ id: variant.id, path: `thumbnails/${variant.id}.png` });
      process.stdout.write(`\r  Progress: ${i + 1}/${result.variants.length}`);
    } catch (err) {
      console.error(`\n  ✗ Failed: ${variant.id}`);
    }
  }

  console.log('\n  ✓ Thumbnails complete\n');

  // Generate HTML gallery
  console.log('📄 Generating gallery...');
  const html = generateGalleryHTML(result.variants, thumbnails, template);
  fs.writeFileSync(path.join(outputDir, 'index.html'), html);

  // Save data
  fs.writeFileSync(
    path.join(outputDir, 'data.json'),
    JSON.stringify(
      {
        generatedAt: new Date().toISOString(),
        template: template.id,
        variants: result.variants.map((v) => ({
          id: v.id,
          name: v.name,
          priority: v.priority,
          axes: v.axes,
          thumbnail: `thumbnails/${v.id}.png`,
        })),
        stats: result.stats,
      },
      null,
      2
    )
  );

  console.log(`\n✅ Gallery generated!`);
  console.log(`   Open: file://${path.join(outputDir, 'index.html')}`);
  console.log(`   Or serve: npx serve ${outputDir}`);
}

// ============================================================
// HTML Generator
// ============================================================

function generateGalleryHTML(
  variants: TemplateVariant[],
  thumbnails: Array<{ id: string; path: string }>,
  template: Template
): string {
  const thumbnailMap = new Map(thumbnails.map((t) => [t.id, t.path]));

  const cards = variants
    .map((v) => {
      const thumb = thumbnailMap.get(v.id) || '';
      return `
      <div class="card" data-id="${v.id}" data-priority="${v.priority}">
        <div class="thumbnail">
          ${thumb ? `<img src="${thumb}" alt="${v.id}" loading="lazy" />` : '<div class="placeholder">No Preview</div>'}
        </div>
        <div class="info">
          <div class="id">${v.id}</div>
          <div class="priority">Priority: ${v.priority}</div>
          <div class="axes">
            <span class="tag avatar">${v.axes.avatarPosition}</span>
            <span class="tag hook">${v.axes.hookStyle}</span>
            <span class="tag caption">${v.axes.captionStyle}</span>
            <span class="tag broll">${v.axes.bRollPattern}</span>
          </div>
        </div>
      </div>`;
    })
    .join('\n');

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Vibe Reels - Template Variants</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: #0a0a0f;
      color: #fff;
      padding: 20px;
    }
    header {
      text-align: center;
      padding: 40px 20px;
      margin-bottom: 30px;
    }
    h1 { font-size: 2.5rem; margin-bottom: 10px; }
    .subtitle { color: #888; font-size: 1.1rem; }
    .stats {
      display: flex;
      justify-content: center;
      gap: 30px;
      margin-top: 20px;
    }
    .stat { text-align: center; }
    .stat-value { font-size: 2rem; font-weight: bold; color: #6c5ce7; }
    .stat-label { font-size: 0.8rem; color: #666; }

    .filters {
      display: flex;
      justify-content: center;
      gap: 10px;
      margin-bottom: 30px;
      flex-wrap: wrap;
    }
    .filter-btn {
      padding: 8px 16px;
      border: 1px solid #333;
      background: transparent;
      color: #fff;
      border-radius: 20px;
      cursor: pointer;
      transition: all 0.2s;
    }
    .filter-btn:hover, .filter-btn.active {
      background: #6c5ce7;
      border-color: #6c5ce7;
    }

    .gallery {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
      gap: 20px;
      max-width: 1600px;
      margin: 0 auto;
    }
    .card {
      background: #1a1a2e;
      border-radius: 12px;
      overflow: hidden;
      transition: transform 0.2s, box-shadow 0.2s;
    }
    .card:hover {
      transform: translateY(-4px);
      box-shadow: 0 10px 30px rgba(108, 92, 231, 0.3);
    }
    .thumbnail {
      aspect-ratio: 9/16;
      background: #0f0f1a;
      display: flex;
      align-items: center;
      justify-content: center;
    }
    .thumbnail img {
      width: 100%;
      height: 100%;
      object-fit: cover;
    }
    .placeholder {
      color: #444;
      font-size: 0.9rem;
    }
    .info {
      padding: 15px;
    }
    .id {
      font-family: monospace;
      font-size: 0.85rem;
      color: #6c5ce7;
      margin-bottom: 5px;
    }
    .priority {
      font-size: 0.8rem;
      color: #888;
      margin-bottom: 10px;
    }
    .axes {
      display: flex;
      flex-wrap: wrap;
      gap: 5px;
    }
    .tag {
      font-size: 0.7rem;
      padding: 3px 8px;
      border-radius: 10px;
      background: #2a2a4e;
    }
    .tag.avatar { background: #e94560; }
    .tag.hook { background: #00cec9; }
    .tag.caption { background: #ffeaa7; color: #000; }
    .tag.broll { background: #6c5ce7; }
  </style>
</head>
<body>
  <header>
    <h1>🎬 Vibe Reels Variants</h1>
    <p class="subtitle">${template.name} - ${variants.length} variations</p>
    <div class="stats">
      <div class="stat">
        <div class="stat-value">${variants.length}</div>
        <div class="stat-label">Variants</div>
      </div>
      <div class="stat">
        <div class="stat-value">8×7×7×4</div>
        <div class="stat-label">Axes</div>
      </div>
      <div class="stat">
        <div class="stat-value">${Math.round(variants.reduce((s, v) => s + v.priority, 0) / variants.length)}</div>
        <div class="stat-label">Avg Priority</div>
      </div>
    </div>
  </header>

  <div class="filters">
    <button class="filter-btn active" data-filter="all">All</button>
    <button class="filter-btn" data-filter="high">High Priority (>50)</button>
    <button class="filter-btn" data-filter="fullscreen">Fullscreen</button>
    <button class="filter-btn" data-filter="circle">Circle Avatar</button>
  </div>

  <div class="gallery">
    ${cards}
  </div>

  <script>
    document.querySelectorAll('.filter-btn').forEach(btn => {
      btn.addEventListener('click', () => {
        document.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));
        btn.classList.add('active');

        const filter = btn.dataset.filter;
        document.querySelectorAll('.card').forEach(card => {
          const id = card.dataset.id;
          const priority = parseInt(card.dataset.priority);

          let show = true;
          if (filter === 'high') show = priority > 50;
          else if (filter === 'fullscreen') show = id.includes('_fs_');
          else if (filter === 'circle') show = id.includes('_cbl_') || id.includes('_cbr_');

          card.style.display = show ? 'block' : 'none';
        });
      });
    });
  </script>
</body>
</html>`;
}

// ============================================================
// Helpers
// ============================================================

function resolveTestAssets(props: Record<string, unknown>): Record<string, unknown> {
  return {
    ...props,
    lipSyncVideo: '/test/lipsync/test-avatar.mp4',
    backgroundVideos: [
      '/test/broll/test-broll-01.mp4',
      '/test/broll/test-broll-02.mp4',
      '/test/broll/test-broll-03.mp4',
      '/test/broll/test-broll-04.mp4',
    ],
    backgroundMusic: '',
  };
}

async function ensureDir(dir: string): Promise<void> {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}

// ============================================================
// Run
// ============================================================

main().catch((err) => {
  console.error('Error:', err);
  process.exit(1);
});
