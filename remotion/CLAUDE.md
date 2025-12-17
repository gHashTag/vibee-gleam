# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

VIBEE Remotion is a video composition and rendering service that provides:
- Remotion video compositions (TextOverlay, VideoIntro, DynamicVideo, LipSyncMain)
- Node.js render server with S3/Tigris upload support
- React/Vite video editor UI (player/)

Part of the larger VIBEE ecosystem with Gleam MCP backend at `../gleam/`.

## Build & Run Commands

```bash
# Remotion Studio (composition development)
npm start              # Opens Remotion Studio at localhost:3000

# Render Server (production rendering + S3 upload)
npx tsx render-server.ts    # Starts server on port 3333

# Player UI (video editor frontend)
cd player
npm run dev            # Vite dev server at localhost:5174
npm run build          # Production build
npm run lint           # ESLint

# Build Remotion bundle
npm run build          # Creates bundle in ./build

# Render video via CLI
npm run render -- --composition=LipSyncMain --output=out/video.mp4
```

### Deployment

```bash
# Deploy render server to Fly.io
fly deploy -a vibee-remotion

# Deploy player to Fly.io
cd player && fly deploy -a vibee-player
```

## Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                      Video Editor UI                            │
│                    (player/ - React/Vite)                       │
│                                                                 │
│  pages/           components/              store/               │
│  ├─ Home.tsx      ├─ Canvas/              └─ editorStore.ts    │
│  └─ Editor.tsx    │  └─ InteractiveCanvas.tsx   (Zustand)      │
│                   ├─ Timeline/                                  │
│                   │  ├─ Timeline.tsx                           │
│                   │  ├─ Track.tsx                              │
│                   │  └─ TrackItem.tsx                          │
│                   └─ Panels/                                    │
│                      ├─ AssetsPanel.tsx                        │
│                      └─ PropertiesPanel.tsx                    │
└────────────────────────────┬───────────────────────────────────┘
                             │ HTTP (render, upload)
                             ▼
┌────────────────────────────────────────────────────────────────┐
│              Render Server (render-server.ts)                   │
│                                                                 │
│  Endpoints:                                                     │
│  GET  /health        - Health check + bundle status            │
│  GET  /compositions  - List available compositions             │
│  POST /render        - Render video/still                      │
│  GET  /renders/:id   - Download rendered file                  │
│  POST /upload        - Upload asset to S3/Tigris               │
│  GET  /assets        - List S3 assets                          │
│  GET  /public/*      - Serve static assets                     │
└────────────────────────────┬───────────────────────────────────┘
                             │
                             ▼
┌────────────────────────────────────────────────────────────────┐
│               Remotion Compositions (src/)                      │
│                                                                 │
│  Root.tsx           - Composition registry                     │
│  compositions/                                                  │
│  ├─ TextOverlay.tsx    - Animated text                        │
│  ├─ VideoIntro.tsx     - Brand intro animation                │
│  ├─ DynamicVideo.tsx   - Data-driven video                    │
│  └─ LipSyncMain.tsx    - Avatar lip-sync template (main)      │
└────────────────────────────────────────────────────────────────┘
```

## Key Files

| File | Purpose |
|------|---------|
| `render-server.ts` | Node.js server handling rendering and S3 uploads |
| `src/Root.tsx` | Remotion composition registry |
| `src/compositions/LipSyncMain.tsx` | Primary Instagram Reels template |
| `player/src/store/editorStore.ts` | Zustand state management for editor |
| `player/src/store/types.ts` | TypeScript types for editor state |
| `fly.toml` | Fly.io deployment (performance-1x, 2 CPU, 4GB RAM) |
| `Dockerfile` | Multi-stage build with Chrome for headless rendering |

## LipSyncMain Template Props

The primary composition for Instagram Reels-style videos:

```typescript
interface LipSyncMainProps {
  // Media files
  lipSyncVideo: string;           // Avatar video with lip-sync
  coverImage: string;             // Cover frame
  backgroundMusic: string;        // Background audio track
  backgroundVideos: string[];     // B-roll video array

  // Effects
  musicVolume: number;            // 0-1
  coverDuration: number;          // seconds (default: 0.5)
  vignetteStrength: number;       // 0-1 (default: 0.7)
  colorCorrection: number;        // 1.0-1.5 (default: 1.2)

  // Avatar circle position
  circleSizePercent: number;      // % of width (default: 25.2)
  circleBottomPercent: number;    // % from bottom (default: 15)
  circleLeftPx: number;           // px from left (default: 40)
}
```

## Editor Store (Zustand)

The editor uses Zustand with immer for state management:

```typescript
// Access store
const { tracks, currentFrame, templateProps } = useEditorStore();

// Actions
addTrack(type: TrackType, name?: string): string
addItem(trackId: string, itemData: Partial<TrackItem>): string
updateItem(itemId: string, updates: Partial<TrackItem>): void
updateTemplateProp<K>(key: K, value: LipSyncMainProps[K]): void
syncBackgroundVideosFromTimeline(): void  // Sync timeline → templateProps.backgroundVideos
```

State is persisted to localStorage (`vibee-editor-storage-v4`).

## Environment Variables

### Render Server

| Variable | Description | Default |
|----------|-------------|---------|
| `PORT` | Server port | 3333 |
| `OUTPUT_DIR` | Rendered files directory | ./out |
| `AWS_ENDPOINT_URL_S3` | S3/Tigris endpoint | https://fly.storage.tigris.dev |
| `BUCKET_NAME` | S3 bucket name | vibee-assets |
| `AWS_ACCESS_KEY_ID` | S3 credentials | - |
| `AWS_SECRET_ACCESS_KEY` | S3 credentials | - |

### Player

| Variable | Description | Default |
|----------|-------------|---------|
| `VITE_RENDER_SERVER_URL` | Render server URL | https://vibee-remotion.fly.dev |

## Render API

### POST /render

```json
{
  "type": "video",           // "video" | "still"
  "compositionId": "LipSyncMain",
  "inputProps": {
    "lipSyncVideo": "/lipsync/video.mp4",
    "backgroundVideos": ["/backgrounds/01.mp4"]
  },
  "codec": "h264",           // h264, h265, vp8, vp9, prores, gif
  "frame": 0                 // For stills only
}
```

Response:
```json
{
  "success": true,
  "renderId": "uuid",
  "outputPath": "/app/out/uuid.mp4",
  "outputUrl": "/renders/uuid.mp4"
}
```

### POST /upload

Send raw file data with headers:
- `x-filename`: filename with extension
- `content-type`: MIME type

Response:
```json
{
  "success": true,
  "url": "https://fly.storage.tigris.dev/vibee-assets/assets/123-video.mp4",
  "key": "assets/123-video.mp4"
}
```

## Production URLs

- **Render Server**: https://vibee-remotion.fly.dev
- **Player UI**: https://vibee-player.fly.dev (if deployed)
- **S3 Storage**: https://fly.storage.tigris.dev/vibee-assets/
- **Gleam MCP**: https://vibee-mcp.fly.dev (parent project)

## Adding a New Composition

1. Create component in `src/compositions/NewComp.tsx`:
```typescript
import { useCurrentFrame, AbsoluteFill } from 'remotion';
import { z } from 'zod';

export const NewCompSchema = z.object({
  title: z.string(),
});

export const NewComp: React.FC<z.infer<typeof NewCompSchema>> = ({ title }) => {
  const frame = useCurrentFrame();
  return <AbsoluteFill>{/* ... */}</AbsoluteFill>;
};
```

2. Register in `src/Root.tsx`:
```typescript
<Composition
  id="NewComp"
  component={NewComp}
  durationInFrames={300}
  fps={30}
  width={1080}
  height={1920}
  schema={NewCompSchema}
  defaultProps={{ title: "Default" }}
/>
```

3. Update `/compositions` endpoint in `render-server.ts` if needed.

## Common Issues

**"Bundle not initialized"**: Wait for Remotion bundle at server startup (takes 30-60s).

**S3 upload fails**: Check AWS credentials are set on Fly.io:
```bash
fly secrets set AWS_ACCESS_KEY_ID=xxx AWS_SECRET_ACCESS_KEY=xxx -a vibee-remotion
```

**Server not accessible on Fly.io**: Ensure listening on `0.0.0.0`:
```typescript
server.listen(Number(PORT), "0.0.0.0", () => { ... });
```

**Video won't play in browser**: Check Range request support in render-server.ts (already implemented).

## Integration with Gleam MCP

Storage tools in `../gleam/src/vibee/mcp/storage_tools.gleam` proxy uploads through this render server:
- `storage_upload` → POST /upload
- `storage_list` → GET /assets
- `storage_config` → Returns S3 configuration

The Erlang FFI at `../gleam/src/vibee_storage_ffi.erl` handles HTTP communication.
