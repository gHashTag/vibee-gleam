# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

VIBEE Remotion is a video composition and rendering service that provides:
- Remotion video compositions (SplitTalkingHead with TikTok-style captions)
- Node.js render server with S3/Tigris upload support
- React/Vite video editor UI (player/)

Part of the larger VIBEE ecosystem with Gleam MCP backend at `../gleam/`.

## Build & Run Commands

```bash
# Remotion Studio (composition development)
npm start                      # Opens Remotion Studio at localhost:3000

# Render Server (production rendering + S3 upload)
npx tsx render-server.ts       # Starts server on port 3333

# Auto-generate captions from lipsync video
npm run captions               # Transcribe and generate captions.json
npm run captions:force         # Force re-transcribe even if cached

# Build Remotion bundle
npm run build                  # Creates bundle in ./build

# Render video via CLI
npm run render -- --composition=SplitTalkingHead --output=out/video.mp4

# Deploy render server
npm run deploy                 # Runs captions + fly deploy
fly deploy -a vibee-remotion   # Direct deploy
```

### Player UI (video editor)

```bash
cd player
npm run dev      # Vite dev server at localhost:5174
npm run build    # Production build
npm run lint     # ESLint
fly deploy --app vibee-player  # Deploy to Fly.io
```

## Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                      Video Editor UI                            │
│                    (player/ - React/Vite)                       │
│                                                                 │
│  Canvas/InteractiveCanvas.tsx ─→ SplitTalkingHead composition  │
│  Timeline/Track.tsx ─→ B-roll video items with asset references│
│  store/editorStore.ts ─→ Zustand + immer state management     │
└────────────────────────────┬───────────────────────────────────┘
                             │ HTTP (render, upload)
                             ▼
┌────────────────────────────────────────────────────────────────┐
│              Render Server (render-server.ts)                   │
│                                                                 │
│  GET  /health        - Health check + bundle status            │
│  GET  /compositions  - List available compositions             │
│  POST /render        - Render video/still (returns renderId)   │
│  GET  /renders/:id   - Download rendered file                  │
│  POST /upload        - Upload asset to S3/Tigris               │
│  GET  /assets        - List S3 assets                          │
│  GET  /public/*      - Serve static media files                │
└────────────────────────────┬───────────────────────────────────┘
                             │
                             ▼
┌────────────────────────────────────────────────────────────────┐
│               Remotion Compositions (src/)                      │
│                                                                 │
│  compositions/SplitTalkingHead.tsx                             │
│    - Split horizontal (50/50): Top B-roll, Bottom talking head │
│    - Alternates between 'split' and 'fullscreen' segments      │
│    - LipSync video plays CONTINUOUSLY (segments control layout)│
│                                                                 │
│  components/Captions.tsx                                        │
│    - TikTok-style word-by-word captions                        │
│    - Uses @remotion/captions createTikTokStyleCaptions()       │
│    - Lato font with word highlighting                          │
└────────────────────────────────────────────────────────────────┘
```

## SplitTalkingHead Composition

The primary composition for Instagram Reels-style videos. Key props:

```typescript
interface SplitTalkingHeadProps {
  lipSyncVideo: string;        // Avatar video (plays continuously!)
  segments: Segment[];         // Layout segments (split/fullscreen)
  captions: Caption[];         // From @remotion/captions
  backgroundMusic?: string;    // Audio track
  musicVolume?: number;        // 0-1 (default: 0.15)
  faceOffsetX/Y?: number;      // Face centering (-50 to 50 %)
  faceScale?: number;          // Zoom (default: 1.0)
}

interface Segment {
  type: 'split' | 'fullscreen';
  startFrame: number;
  durationFrames: number;
  bRollUrl?: string;           // B-roll video for split mode
  bRollType?: 'video' | 'image';
}
```

**Important**: B-roll videos use `premountFor={90}` and `pauseWhenBuffering` to prevent freezing during headless rendering.

## Fly.io Storage Architecture

Single persistent volume `vibee_storage` (15GB) mounted at `/app/storage`:
- `/app/public` → symlink to `/app/storage/public` (media files)
- `/app/out` → symlink to `/app/storage/renders` (rendered videos)

**Fast media updates without rebuild**:
```bash
# Upload single file via SFTP (~30 sec)
echo "put local/01.mp4 /app/storage/public/backgrounds/business/01.mp4" | fly ssh sftp shell -a vibee-remotion

# Code-only deploy (~2-3 min, no media in Docker image)
fly deploy -a vibee-remotion
```

## Environment Variables

### Render Server

| Variable | Description | Default |
|----------|-------------|---------|
| `PORT` | Server port | 3333 |
| `OUTPUT_DIR` | Rendered files directory | ./out |
| `AWS_ENDPOINT_URL_S3` | S3/Tigris endpoint | https://fly.storage.tigris.dev |
| `BUCKET_NAME` | S3 bucket name | vibee-assets |

### Player

| Variable | Description | Default |
|----------|-------------|---------|
| `VITE_RENDER_SERVER_URL` | Render server URL | https://vibee-remotion.fly.dev |

## Render API

### POST /render

```json
{
  "type": "video",
  "compositionId": "SplitTalkingHead",
  "inputProps": {
    "lipSyncVideo": "/lipsync/lipsync.mp4",
    "segments": [
      { "type": "split", "startFrame": 0, "durationFrames": 90, "bRollUrl": "/backgrounds/business/01.mp4" },
      { "type": "fullscreen", "startFrame": 90, "durationFrames": 60 }
    ],
    "captions": [/* from /lipsync/captions.json */]
  }
}
```

## Captions Pipeline

1. Place lipsync video at `public/lipsync/lipsync.mp4`
2. Run `npm run captions` - uses Whisper.cpp for transcription
3. Outputs `public/lipsync/captions.json` with word-level timing
4. Captions component uses `@remotion/captions` for TikTok-style display

## Production URLs

- **Render Server**: https://vibee-remotion.fly.dev
- **Player UI**: https://vibee-player.fly.dev
- **S3 Storage**: https://fly.storage.tigris.dev/vibee-assets/

## Player Brand Guidelines

- **VIBEE = Vibe Bee** - yellow/amber colors
- Primary: `#f59e0b` (amber-500)
- NO purple colors
- Deploy to Fly.io only (never Vercel)

## Common Issues

**"Bundle not initialized"**: Wait 30-60s for Remotion bundle at server startup.

**B-roll video freezes**: Ensure composition uses `premountFor={90}` on Sequence and `pauseWhenBuffering` on Video.

**S3 upload fails**: Set credentials on Fly.io:
```bash
fly secrets set AWS_ACCESS_KEY_ID=xxx AWS_SECRET_ACCESS_KEY=xxx -a vibee-remotion
```

**Timeline positions don't match video**: InteractiveCanvas uses actual `TrackItem.startFrame` from timeline, not recalculated values. Check `convertToSplitTalkingHeadProps()` function.

**Video won't play in browser**: Check Range request support in render-server.ts (already implemented).

## Integration with Gleam MCP

Storage tools in `../gleam/src/vibee/mcp/storage_tools.gleam` proxy uploads through this render server:
- `storage_upload` → POST /upload
- `storage_list` → GET /assets
- `storage_config` → Returns S3 configuration

The Erlang FFI at `../gleam/src/vibee_storage_ffi.erl` handles HTTP communication.
