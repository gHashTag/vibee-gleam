// Factory Panel - Template Variants Gallery
// Web UI for previewing all Remotion template variations

import gleam/int
import gleam/list
import gleam/string

/// Render the factory gallery HTML page
pub fn render() -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>Vibe Reels - Template Factory</title>
  <style>
:root {
  --bg-primary: #0a0a0f;
  --bg-secondary: #12121a;
  --bg-card: #1a1a2e;
  --text-primary: #ffffff;
  --text-secondary: #888;
  --accent: #6c5ce7;
  --accent-hover: #5b4cdb;
  --border: #2a2a3e;
  --avatar: #e94560;
  --hook: #00cec9;
  --caption: #ffeaa7;
  --broll: #6c5ce7;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  background: var(--bg-primary);
  color: var(--text-primary);
  min-height: 100vh;
}
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1.5rem 2rem;
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border);
  position: sticky;
  top: 0;
  z-index: 100;
}
.title { font-size: 1.5rem; font-weight: 600; display: flex; align-items: center; gap: 0.75rem; }
.title-icon { font-size: 1.75rem; }
.subtitle { color: var(--text-secondary); font-size: 0.9rem; margin-left: 1rem; }
.stats {
  display: flex;
  gap: 2rem;
  background: var(--bg-card);
  padding: 0.75rem 1.5rem;
  border-radius: 12px;
  border: 1px solid var(--border);
}
.stat { text-align: center; }
.stat-value { font-size: 1.5rem; font-weight: bold; color: var(--accent); }
.stat-label { font-size: 0.75rem; color: var(--text-secondary); text-transform: uppercase; }
.controls { display: flex; gap: 0.75rem; }
.btn {
  padding: 0.6rem 1.2rem;
  border: 1px solid var(--border);
  background: transparent;
  color: var(--text-primary);
  border-radius: 8px;
  cursor: pointer;
  font-size: 0.9rem;
  transition: all 0.2s;
}
.btn:hover { border-color: var(--accent); background: rgba(108, 92, 231, 0.1); }
.btn-primary { background: var(--accent); border-color: var(--accent); }
.btn-primary:hover { background: var(--accent-hover); }

.container { padding: 2rem; }

.filters {
  display: flex;
  gap: 1rem;
  margin-bottom: 2rem;
  flex-wrap: wrap;
  align-items: center;
}
.filter-group { display: flex; align-items: center; gap: 0.5rem; }
.filter-label { color: var(--text-secondary); font-size: 0.85rem; }
.filter-select {
  padding: 0.5rem 1rem;
  background: var(--bg-card);
  border: 1px solid var(--border);
  color: var(--text-primary);
  border-radius: 8px;
  font-size: 0.9rem;
  cursor: pointer;
}
.filter-select:focus { outline: none; border-color: var(--accent); }

.search-box {
  flex: 1;
  max-width: 300px;
  padding: 0.5rem 1rem;
  background: var(--bg-card);
  border: 1px solid var(--border);
  color: var(--text-primary);
  border-radius: 8px;
  font-size: 0.9rem;
}
.search-box::placeholder { color: var(--text-secondary); }
.search-box:focus { outline: none; border-color: var(--accent); }

.gallery {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  gap: 1.5rem;
}

.variant-card {
  background: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: 16px;
  overflow: hidden;
  transition: transform 0.2s, box-shadow 0.2s;
  cursor: pointer;
}
.variant-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 12px 40px rgba(108, 92, 231, 0.2);
  border-color: var(--accent);
}

.thumbnail {
  aspect-ratio: 9/16;
  background: var(--bg-secondary);
  display: flex;
  align-items: center;
  justify-content: center;
  position: relative;
  overflow: hidden;
}
.thumbnail img {
  width: 100%;
  height: 100%;
  object-fit: cover;
}
.thumbnail-placeholder {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.5rem;
  color: var(--text-secondary);
}
.thumbnail-icon { font-size: 3rem; opacity: 0.5; }
.thumbnail-text { font-size: 0.8rem; }

.play-overlay {
  position: absolute;
  inset: 0;
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  opacity: 0;
  transition: opacity 0.2s;
}
.variant-card:hover .play-overlay { opacity: 1; }
.play-btn {
  width: 60px;
  height: 60px;
  background: var(--accent);
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 1.5rem;
}

.variant-info { padding: 1rem 1.25rem; }
.variant-id {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.85rem;
  color: var(--accent);
  margin-bottom: 0.5rem;
}
.variant-priority {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.75rem;
}
.priority-bar {
  flex: 1;
  height: 6px;
  background: var(--border);
  border-radius: 3px;
  overflow: hidden;
}
.priority-fill {
  height: 100%;
  background: linear-gradient(90deg, var(--accent), #a29bfe);
  border-radius: 3px;
}
.priority-value { font-size: 0.8rem; color: var(--text-secondary); min-width: 35px; }

.variant-axes {
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;
}
.axis-tag {
  font-size: 0.7rem;
  padding: 0.2rem 0.5rem;
  border-radius: 6px;
  font-weight: 500;
}
.axis-avatar { background: rgba(233, 69, 96, 0.2); color: var(--avatar); }
.axis-hook { background: rgba(0, 206, 201, 0.2); color: var(--hook); }
.axis-caption { background: rgba(255, 234, 167, 0.2); color: #f39c12; }
.axis-broll { background: rgba(108, 92, 231, 0.2); color: var(--broll); }

.loading {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 4rem;
  color: var(--text-secondary);
}
.loading-spinner {
  width: 40px;
  height: 40px;
  border: 3px solid var(--border);
  border-top-color: var(--accent);
  border-radius: 50%;
  animation: spin 1s linear infinite;
  margin-bottom: 1rem;
}
@keyframes spin { to { transform: rotate(360deg); } }

.empty-state {
  text-align: center;
  padding: 4rem;
  color: var(--text-secondary);
}
.empty-icon { font-size: 4rem; margin-bottom: 1rem; opacity: 0.5; }

.modal {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.8);
  display: none;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}
.modal.active { display: flex; }
.modal-content {
  background: var(--bg-card);
  border-radius: 16px;
  max-width: 90vw;
  max-height: 90vh;
  overflow: hidden;
}
.modal-close {
  position: absolute;
  top: 1rem;
  right: 1rem;
  background: var(--bg-card);
  border: none;
  color: var(--text-primary);
  width: 40px;
  height: 40px;
  border-radius: 50%;
  cursor: pointer;
  font-size: 1.5rem;
}
  </style>
</head>
<body>
  <div class=\"header\">
    <div style=\"display: flex; align-items: center;\">
      <div class=\"title\">
        <span class=\"title-icon\">🎬</span>
        Vibe Reels Factory
      </div>
      <span class=\"subtitle\">Template Variations Gallery</span>
    </div>
    <div class=\"stats\">
      <div class=\"stat\">
        <div class=\"stat-value\" id=\"totalVariants\">-</div>
        <div class=\"stat-label\">Variants</div>
      </div>
      <div class=\"stat\">
        <div class=\"stat-value\">8×7×7×4</div>
        <div class=\"stat-label\">Axes</div>
      </div>
      <div class=\"stat\">
        <div class=\"stat-value\" id=\"avgPriority\">-</div>
        <div class=\"stat-label\">Avg Priority</div>
      </div>
    </div>
    <div class=\"controls\">
      <button class=\"btn\" onclick=\"generateVariants()\">🔄 Generate</button>
      <button class=\"btn\" onclick=\"openStudio()\">🎥 Studio</button>
      <button class=\"btn\" onclick=\"location.href='/'\">🏠 Home</button>
    </div>
  </div>

  <div class=\"container\">
    <div class=\"filters\">
      <div class=\"filter-group\">
        <span class=\"filter-label\">Avatar:</span>
        <select class=\"filter-select\" id=\"filterAvatar\" onchange=\"applyFilters()\">
          <option value=\"all\">All Positions</option>
          <option value=\"circle-bottom-left\">Circle Bottom Left</option>
          <option value=\"circle-bottom-right\">Circle Bottom Right</option>
          <option value=\"fullscreen\">Fullscreen</option>
          <option value=\"side-left\">Side Left</option>
          <option value=\"side-right\">Side Right</option>
          <option value=\"floating-center\">Floating Center</option>
        </select>
      </div>
      <div class=\"filter-group\">
        <span class=\"filter-label\">Hook:</span>
        <select class=\"filter-select\" id=\"filterHook\" onchange=\"applyFilters()\">
          <option value=\"all\">All Styles</option>
          <option value=\"zoom-impact\">Zoom Impact</option>
          <option value=\"slide-reveal\">Slide Reveal</option>
          <option value=\"typewriter\">Typewriter</option>
          <option value=\"glitch\">Glitch</option>
          <option value=\"pulse\">Pulse</option>
          <option value=\"question\">Question</option>
          <option value=\"none\">None</option>
        </select>
      </div>
      <div class=\"filter-group\">
        <span class=\"filter-label\">Caption:</span>
        <select class=\"filter-select\" id=\"filterCaption\" onchange=\"applyFilters()\">
          <option value=\"all\">All Styles</option>
          <option value=\"karaoke\">Karaoke</option>
          <option value=\"bounce\">Bounce</option>
          <option value=\"modern\">Modern</option>
          <option value=\"typewriter\">Typewriter</option>
          <option value=\"classic\">Classic</option>
          <option value=\"none\">None</option>
        </select>
      </div>
      <div class=\"filter-group\">
        <span class=\"filter-label\">B-Roll:</span>
        <select class=\"filter-select\" id=\"filterBRoll\" onchange=\"applyFilters()\">
          <option value=\"all\">All Patterns</option>
          <option value=\"hook-content-cta\">Hook → Content → CTA</option>
          <option value=\"even-distribution\">Even Distribution</option>
          <option value=\"progressive\">Progressive</option>
          <option value=\"random-weighted\">Random Weighted</option>
        </select>
      </div>
      <input type=\"text\" class=\"search-box\" placeholder=\"Search variant ID...\" id=\"searchBox\" oninput=\"applyFilters()\">
    </div>

    <div class=\"gallery\" id=\"gallery\">
      <div class=\"loading\">
        <div class=\"loading-spinner\"></div>
        <div>Loading variants...</div>
      </div>
    </div>
  </div>

  <div class=\"modal\" id=\"previewModal\">
    <button class=\"modal-close\" onclick=\"closeModal()\">&times;</button>
    <div class=\"modal-content\" style=\"padding: 2rem; min-width: 400px;\">
      <h2 style=\"margin-bottom: 1rem; color: var(--accent);\">🎬 <span id=\"modalVariantId\"></span></h2>
      <div id=\"modalAxes\" style=\"margin-bottom: 1.5rem;\"></div>
      <div style=\"background: var(--bg-secondary); padding: 1rem; border-radius: 8px; margin-bottom: 1.5rem;\">
        <div style=\"font-size: 0.8rem; color: var(--text-secondary); margin-bottom: 0.5rem;\">Props JSON:</div>
        <pre id=\"modalProps\" style=\"font-size: 0.75rem; overflow-x: auto; color: #a29bfe;\"></pre>
      </div>
      <div style=\"display: flex; gap: 0.75rem; flex-wrap: wrap;\">
        <button class=\"btn btn-primary\" onclick=\"renderPreview()\">🎥 Render Preview</button>
        <button class=\"btn\" onclick=\"copyProps()\">📋 Copy Props</button>
        <button class=\"btn\" onclick=\"copyMcpCommand()\">🤖 Copy MCP Command</button>
      </div>
      <div id=\"renderStatus\" style=\"margin-top: 1rem; font-size: 0.85rem; color: var(--text-secondary);\"></div>
    </div>
  </div>

  <script>
let variants = [];
let filteredVariants = [];

async function loadVariants() {
  try {
    const resp = await fetch('/api/v1/factory/variants?limit=100');
    const data = await resp.json();
    variants = data.variants || [];
    document.getElementById('totalVariants').textContent = variants.length;
    document.getElementById('avgPriority').textContent = data.stats?.avgPriority || '-';
    filteredVariants = [...variants];
    renderGallery();
  } catch (err) {
    console.error('Failed to load variants:', err);
    // Show demo data
    loadDemoData();
  }
}

function loadDemoData() {
  // Demo variants for testing
  variants = [
    { id: 'TH_cbl_zi_k_hcc', priority: 85, axes: { avatarPosition: 'circle-bottom-left', hookStyle: 'zoom-impact', captionStyle: 'karaoke', bRollPattern: 'hook-content-cta' } },
    { id: 'TH_cbr_sr_b_ed', priority: 78, axes: { avatarPosition: 'circle-bottom-right', hookStyle: 'slide-reveal', captionStyle: 'bounce', bRollPattern: 'even-distribution' } },
    { id: 'TH_fs_tw_m_pr', priority: 72, axes: { avatarPosition: 'fullscreen', hookStyle: 'typewriter', captionStyle: 'modern', bRollPattern: 'progressive' } },
    { id: 'TH_sl_gl_c_rw', priority: 65, axes: { avatarPosition: 'side-left', hookStyle: 'glitch', captionStyle: 'classic', bRollPattern: 'random-weighted' } },
    { id: 'TH_sr_pu_wh_hcc', priority: 60, axes: { avatarPosition: 'side-right', hookStyle: 'pulse', captionStyle: 'word-highlight', bRollPattern: 'hook-content-cta' } },
    { id: 'TH_fc_qu_tw_ed', priority: 55, axes: { avatarPosition: 'floating-center', hookStyle: 'question', captionStyle: 'typewriter', bRollPattern: 'even-distribution' } },
  ];

  document.getElementById('totalVariants').textContent = variants.length;
  document.getElementById('avgPriority').textContent = Math.round(variants.reduce((a, v) => a + v.priority, 0) / variants.length);
  filteredVariants = [...variants];
  renderGallery();
}

function renderGallery() {
  const gallery = document.getElementById('gallery');

  if (filteredVariants.length === 0) {
    gallery.innerHTML = `
      <div class=\"empty-state\" style=\"grid-column: 1/-1;\">
        <div class=\"empty-icon\">🎬</div>
        <div>No variants match your filters</div>
      </div>
    `;
    return;
  }

  gallery.innerHTML = filteredVariants.map(v => `
    <div class=\"variant-card\" onclick=\"openPreview('${v.id}')\">
      <div class=\"thumbnail\">
        <div class=\"thumbnail-placeholder\">
          <div class=\"thumbnail-icon\">🎬</div>
          <div class=\"thumbnail-text\">Preview</div>
        </div>
        <div class=\"play-overlay\">
          <div class=\"play-btn\">▶</div>
        </div>
      </div>
      <div class=\"variant-info\">
        <div class=\"variant-id\">${v.id}</div>
        <div class=\"variant-priority\">
          <div class=\"priority-bar\">
            <div class=\"priority-fill\" style=\"width: ${v.priority}%\"></div>
          </div>
          <span class=\"priority-value\">${v.priority}%</span>
        </div>
        <div class=\"variant-axes\">
          <span class=\"axis-tag axis-avatar\">${formatAxis(v.axes.avatarPosition)}</span>
          <span class=\"axis-tag axis-hook\">${formatAxis(v.axes.hookStyle)}</span>
          <span class=\"axis-tag axis-caption\">${formatAxis(v.axes.captionStyle)}</span>
          <span class=\"axis-tag axis-broll\">${formatAxis(v.axes.bRollPattern)}</span>
        </div>
      </div>
    </div>
  `).join('');
}

function formatAxis(value) {
  if (!value) return '';
  return value.split('-').map(w => w.charAt(0).toUpperCase()).join('');
}

function applyFilters() {
  const avatar = document.getElementById('filterAvatar').value;
  const hook = document.getElementById('filterHook').value;
  const caption = document.getElementById('filterCaption').value;
  const broll = document.getElementById('filterBRoll').value;
  const search = document.getElementById('searchBox').value.toLowerCase();

  filteredVariants = variants.filter(v => {
    if (avatar !== 'all' && v.axes.avatarPosition !== avatar) return false;
    if (hook !== 'all' && v.axes.hookStyle !== hook) return false;
    if (caption !== 'all' && v.axes.captionStyle !== caption) return false;
    if (broll !== 'all' && v.axes.bRollPattern !== broll) return false;
    if (search && !v.id.toLowerCase().includes(search)) return false;
    return true;
  });

  renderGallery();
}

function generateVariants() {
  alert('Generate variants: Use MCP tool factory_generate_variants');
}

function openStudio() {
  window.open('http://localhost:3000', '_blank');
}

let currentVariant = null;

function openPreview(variantId) {
  const variant = variants.find(v => v.id === variantId);
  if (!variant) return;

  currentVariant = variant;
  const modal = document.getElementById('previewModal');

  document.getElementById('modalVariantId').textContent = variant.id;
  document.getElementById('modalAxes').innerHTML = `
    <span class=\"axis-tag axis-avatar\">${variant.axes.avatarPosition}</span>
    <span class=\"axis-tag axis-hook\">${variant.axes.hookStyle}</span>
    <span class=\"axis-tag axis-caption\">${variant.axes.captionStyle}</span>
    <span class=\"axis-tag axis-broll\">${variant.axes.bRollPattern}</span>
  `;

  const props = {
    avatarPosition: variant.axes.avatarPosition,
    hookStyle: variant.axes.hookStyle,
    captionStyle: variant.axes.captionStyle,
    bRollPattern: variant.axes.bRollPattern,
    lipSyncVideo: '/test/lipsync/test-avatar.mp4',
    backgroundVideos: ['/test/broll/test-broll-01.mp4', '/test/broll/test-broll-02.mp4'],
    durationInFrames: 900,
    fps: 30
  };
  document.getElementById('modalProps').textContent = JSON.stringify(props, null, 2);
  document.getElementById('renderStatus').textContent = '';

  modal.classList.add('active');
}

function closeModal() {
  document.getElementById('previewModal').classList.remove('active');
  currentVariant = null;
}

async function renderPreview() {
  if (!currentVariant) return;
  const status = document.getElementById('renderStatus');
  status.innerHTML = '⏳ Rendering preview... (this may take 30-60 seconds)';

  try {
    const resp = await fetch('/api/v1/factory/render', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ variantId: currentVariant.id })
    });
    const data = await resp.json();
    if (data.videoUrl) {
      status.innerHTML = `✅ Ready! <a href=\"${data.videoUrl}\" target=\"_blank\" style=\"color: var(--accent);\">Open Video</a>`;
    } else {
      status.innerHTML = '⚠️ Render queued. Check back in 1-2 minutes.';
    }
  } catch (err) {
    status.innerHTML = '❌ Render not available. Use MCP tool: factory_render_preview';
  }
}

function copyProps() {
  if (!currentVariant) return;
  const props = {
    avatarPosition: currentVariant.axes.avatarPosition,
    hookStyle: currentVariant.axes.hookStyle,
    captionStyle: currentVariant.axes.captionStyle,
    bRollPattern: currentVariant.axes.bRollPattern
  };
  navigator.clipboard.writeText(JSON.stringify(props, null, 2));
  document.getElementById('renderStatus').textContent = '✅ Props copied to clipboard!';
}

function copyMcpCommand() {
  if (!currentVariant) return;
  const cmd = `factory_render_preview variant_id=\"${currentVariant.id}\"`;
  navigator.clipboard.writeText(cmd);
  document.getElementById('renderStatus').textContent = '✅ MCP command copied!';
}

// Close modal on escape
document.addEventListener('keydown', (e) => {
  if (e.key === 'Escape') closeModal();
});

// Load on page load
loadVariants();
  </script>
</body>
</html>"
}
