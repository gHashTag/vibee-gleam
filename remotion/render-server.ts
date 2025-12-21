import { createServer, IncomingMessage } from "node:http";
import { WebSocketServer, WebSocket } from "ws";
import { bundle } from "@remotion/bundler";
import { renderMedia, selectComposition, renderStill } from "@remotion/renderer";
import path from "node:path";
import fs from "node:fs";
import { randomUUID } from "node:crypto";
import { execSync } from "node:child_process";
import { S3Client, PutObjectCommand, ListObjectsV2Command } from "@aws-sdk/client-s3";
import { transcribeVideo } from "./src/lib/transcribe";
import { detectFaceInVideo, detectFaceInImage, calculateCropSettings, loadModels } from "./src/lib/faceDetection";

// Get video duration using ffprobe
function getVideoDuration(videoPath: string): number {
  try {
    const result = execSync(
      `ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 "${videoPath}"`,
      { encoding: 'utf-8' }
    ).trim();
    return parseFloat(result);
  } catch (error) {
    console.warn(`⚠️ Could not get video duration for ${videoPath}:`, error);
    return 0;
  }
}

const PORT = process.env.PORT || 3333;
const OUTPUT_DIR = process.env.OUTPUT_DIR || "./out";

// S3/Tigris Configuration
const S3_ENDPOINT = process.env.AWS_ENDPOINT_URL_S3 || "https://fly.storage.tigris.dev";
const S3_BUCKET = process.env.BUCKET_NAME || process.env.S3_BUCKET || "vibee-assets";
const S3_PUBLIC_URL = process.env.S3_PUBLIC_URL || `${S3_ENDPOINT}/${S3_BUCKET}`;

const s3Client = new S3Client({
  region: process.env.AWS_REGION || "auto",
  endpoint: S3_ENDPOINT,
  credentials: process.env.AWS_ACCESS_KEY_ID ? {
    accessKeyId: process.env.AWS_ACCESS_KEY_ID,
    secretAccessKey: process.env.AWS_SECRET_ACCESS_KEY || "",
  } : undefined,
});

// Upload directory for temp files
const UPLOAD_DIR = process.env.UPLOAD_DIR || "./uploads";

// Ensure directories exist
if (!fs.existsSync(UPLOAD_DIR)) {
  fs.mkdirSync(UPLOAD_DIR, { recursive: true });
}

// Ensure output directory exists
if (!fs.existsSync(OUTPUT_DIR)) {
  fs.mkdirSync(OUTPUT_DIR, { recursive: true });
}

// Bundle once at startup for better performance
let bundleLocation: string;

async function initBundle() {
  console.log("📦 Creating Remotion bundle...");
  bundleLocation = await bundle({
    entryPoint: path.resolve("./src/index.ts"),
    webpackOverride: (config) => config,
  });
  console.log("✅ Bundle ready at:", bundleLocation);

  // Preload face detection models
  console.log("👤 Loading face detection models...");
  try {
    await loadModels();
    console.log("✅ Face detection models ready");
  } catch (error) {
    console.warn("⚠️ Face detection models failed to load:", error);
  }
}

// S3 Upload helper
async function uploadToS3(
  fileBuffer: Buffer,
  filename: string,
  contentType: string
): Promise<{ success: boolean; url?: string; key?: string; error?: string }> {
  const key = `assets/${Date.now()}-${filename}`;

  try {
    await s3Client.send(new PutObjectCommand({
      Bucket: S3_BUCKET,
      Key: key,
      Body: fileBuffer,
      ContentType: contentType,
    }));

    const url = `${S3_PUBLIC_URL}/${key}`;
    console.log(`✅ Uploaded to S3: ${url}`);
    return { success: true, url, key };
  } catch (error) {
    console.error("❌ S3 upload failed:", error);
    return {
      success: false,
      error: error instanceof Error ? error.message : "Upload failed"
    };
  }
}

// List S3 assets
async function listS3Assets(prefix: string = "assets/"): Promise<{
  success: boolean;
  assets?: Array<{ key: string; url: string; size: number; lastModified: Date }>;
  error?: string;
}> {
  try {
    const response = await s3Client.send(new ListObjectsV2Command({
      Bucket: S3_BUCKET,
      Prefix: prefix,
    }));

    const assets = (response.Contents || []).map((obj) => ({
      key: obj.Key || "",
      url: `${S3_PUBLIC_URL}/${obj.Key}`,
      size: obj.Size || 0,
      lastModified: obj.LastModified || new Date(),
    }));

    return { success: true, assets };
  } catch (error) {
    console.error("❌ S3 list failed:", error);
    return {
      success: false,
      error: error instanceof Error ? error.message : "List failed",
    };
  }
}

// Get content type from filename
function getContentType(filename: string): string {
  const ext = path.extname(filename).toLowerCase();
  const types: Record<string, string> = {
    ".mp4": "video/mp4",
    ".webm": "video/webm",
    ".mov": "video/quicktime",
    ".mp3": "audio/mpeg",
    ".wav": "audio/wav",
    ".ogg": "audio/ogg",
    ".jpg": "image/jpeg",
    ".jpeg": "image/jpeg",
    ".png": "image/png",
    ".gif": "image/gif",
    ".webp": "image/webp",
  };
  return types[ext] || "application/octet-stream";
}

interface RenderRequest {
  type: "video" | "still";
  compositionId: string;
  inputProps?: Record<string, unknown>;
  codec?: string;
  frame?: number;
}

interface RenderResponse {
  success: boolean;
  renderId?: string;
  outputPath?: string;
  outputUrl?: string;
  error?: string;
}

// Store render jobs for progress tracking
interface RenderJob {
  id: string;
  status: 'pending' | 'rendering' | 'completed' | 'failed';
  progress: number;
  outputUrl?: string;
  error?: string;
  startedAt: Date;
}

const renderJobs = new Map<string, RenderJob>();

// Clean up old jobs after 1 hour
setInterval(() => {
  const oneHourAgo = Date.now() - 60 * 60 * 1000;
  for (const [id, job] of renderJobs) {
    if (job.startedAt.getTime() < oneHourAgo) {
      renderJobs.delete(id);
    }
  }
}, 60 * 1000);

// Resolve media path to absolute file path
function resolveMediaPath(mediaPath: string): string {
  if (mediaPath.startsWith("http://") || mediaPath.startsWith("https://")) {
    return mediaPath; // Remote URL, can't process locally
  }
  if (mediaPath.startsWith("/") && !mediaPath.startsWith("//")) {
    return path.join(process.cwd(), "public", mediaPath);
  }
  return mediaPath;
}

// Start render asynchronously and return immediately
function startRenderAsync(req: RenderRequest): string {
  const renderId = randomUUID();

  // Create job entry
  renderJobs.set(renderId, {
    id: renderId,
    status: 'pending',
    progress: 0,
    startedAt: new Date(),
  });

  // Start render in background
  (async () => {
    const job = renderJobs.get(renderId)!;
    job.status = 'rendering';

    try {
      if (!bundleLocation) {
        throw new Error("Bundle not initialized");
      }

      // Prepare inputProps with auto face detection and dynamic duration
      const inputProps = { ...(req.inputProps || {}) } as Record<string, unknown>;
      const fps = 30;
      let durationInFrames: number | null = null;

      // Log segments if provided (for debugging preview/render sync)
      const segments = inputProps.segments as Array<{ type: string; startFrame: number; durationFrames: number; bRollUrl?: string }> | undefined;
      if (segments && segments.length > 0) {
        console.log(`📊 Received ${segments.length} segments from editor:`);
        segments.forEach((seg, i) => {
          console.log(`   [${i}] ${seg.type} @ frame ${seg.startFrame}, duration ${seg.durationFrames}${seg.bRollUrl ? `, bRoll: ${seg.bRollUrl.split('/').pop()}` : ''}`);
        });
      } else {
        console.log(`⚠️ No segments provided, composition will use default layout`);
      }

      // Get lipSyncVideo path for analysis
      const lipSyncVideo = inputProps.lipSyncVideo as string | undefined;
      if (lipSyncVideo) {
        const videoPath = resolveMediaPath(lipSyncVideo);

        if (fs.existsSync(videoPath)) {
          // 1. Get video duration dynamically
          const duration = getVideoDuration(videoPath);
          if (duration > 0) {
            durationInFrames = Math.ceil(duration * fps);
            console.log(`📏 Video duration: ${duration.toFixed(2)}s = ${durationInFrames} frames`);
          }

          // 2. Auto-load captions if not provided
          const captions = inputProps.captions as unknown[] | undefined;
          if (!captions || captions.length === 0) {
            // Try to find captions.json in the same directory
            const videoDir = path.dirname(videoPath);
            const captionsPath = path.join(videoDir, 'captions.json');
            if (fs.existsSync(captionsPath)) {
              try {
                const captionsData = JSON.parse(fs.readFileSync(captionsPath, 'utf-8'));
                inputProps.captions = captionsData;
                console.log(`📝 Auto-loaded ${captionsData.length} captions from: ${captionsPath}`);
              } catch (captionsError) {
                console.warn(`⚠️ Could not load captions from ${captionsPath}:`, captionsError);
              }
            }
          }

          // 3. Auto face detection (if not already provided)
          if (inputProps.faceOffsetX === undefined || inputProps.faceOffsetY === undefined) {
            console.log(`👤 Auto-detecting face in: ${videoPath}`);
            try {
              const faceBox = await detectFaceInVideo(videoPath);
              if (faceBox) {
                const crop = calculateCropSettings(faceBox, 'portrait');
                inputProps.faceOffsetX = crop.offsetX;
                inputProps.faceOffsetY = crop.offsetY;
                inputProps.faceScale = crop.scale;
                console.log(`✅ Face detected: offsetX=${crop.offsetX.toFixed(1)}, offsetY=${crop.offsetY.toFixed(1)}, scale=${crop.scale.toFixed(2)}`);
              } else {
                console.log(`⚠️ No face detected, using defaults`);
                inputProps.faceOffsetX = 0;
                inputProps.faceOffsetY = 0;
                inputProps.faceScale = 1;
              }
            } catch (faceError) {
              console.warn(`⚠️ Face detection failed:`, faceError);
              inputProps.faceOffsetX = 0;
              inputProps.faceOffsetY = 0;
              inputProps.faceScale = 1;
            }
          }
        } else {
          console.warn(`⚠️ Video file not found for analysis: ${videoPath}`);
        }
      }

      const composition = await selectComposition({
        serveUrl: bundleLocation,
        id: req.compositionId,
        inputProps,
        chromiumOptions: {
          enableMultiProcessOnLinux: true,
          disableWebSecurity: true,
          gl: null,  // Disable WebGL - no X11/display needed
          headless: true,
          args: [
            '--no-sandbox',
            '--disable-setuid-sandbox',
            '--disable-gpu',
            '--disable-software-rasterizer',
            '--disable-dev-shm-usage',
            '--disable-accelerated-2d-canvas',
            '--no-first-run',
          ],
        },
        timeoutInMilliseconds: 120000,
      });

      // Override duration if we detected it from video
      if (durationInFrames && durationInFrames > 0) {
        console.log(`📏 Overriding composition duration: ${composition.durationInFrames} → ${durationInFrames}`);
        (composition as any).durationInFrames = durationInFrames;
      }

      if (req.type === "still") {
        const outputPath = path.join(OUTPUT_DIR, `${renderId}.png`);

        await renderStill({
          composition,
          serveUrl: bundleLocation,
          output: outputPath,
          inputProps,
          frame: req.frame || 0,
          chromiumOptions: {
            enableMultiProcessOnLinux: true,
            disableWebSecurity: true,
            gl: null,  // Disable WebGL - no X11/display needed
            headless: true,
            args: [
              '--no-sandbox',
              '--disable-setuid-sandbox',
              '--disable-gpu',
              '--disable-software-rasterizer',
              '--disable-dev-shm-usage',
              '--disable-accelerated-2d-canvas',
              '--no-first-run',
              '--no-zygote',
              '--single-process',
            ],
          },
          timeoutInMilliseconds: 120000,
        });

        job.status = 'completed';
        job.progress = 100;
        job.outputUrl = `/renders/${renderId}.png`;
        return;
      }

      // Video render
      const codec = (req.codec || "h264") as "h264" | "h265" | "vp8" | "vp9" | "prores" | "gif";
      const ext = codec === "gif" ? "gif" : "mp4";
      const outputPath = path.join(OUTPUT_DIR, `${renderId}.${ext}`);

      await renderMedia({
        composition,
        serveUrl: bundleLocation,
        codec,
        outputLocation: outputPath,
        inputProps,
        concurrency: 4,
        chromiumOptions: {
          enableMultiProcessOnLinux: true,
          disableWebSecurity: true,
          gl: null,  // Disable WebGL - no X11/display needed
          headless: true,
          args: [
            '--no-sandbox',
            '--disable-setuid-sandbox',
            '--disable-gpu',
            '--disable-software-rasterizer',
            '--disable-dev-shm-usage',
            '--disable-accelerated-2d-canvas',
            '--no-first-run',
          ],
        },
        timeoutInMilliseconds: 120000,
        onProgress: ({ progress }) => {
          const percent = Math.round(progress * 100);
          job.progress = percent;
          console.log(`🎬 Render ${renderId}: ${percent}%`);
        },
      });

      job.status = 'completed';
      job.progress = 100;
      job.outputUrl = `/renders/${renderId}.${ext}`;
      console.log(`✅ Render ${renderId} completed: ${job.outputUrl}`);

    } catch (error) {
      console.error(`❌ Render ${renderId} failed:`, error);
      job.status = 'failed';
      job.error = error instanceof Error ? error.message : "Unknown error";
    }
  })();

  return renderId;
}

// Simple HTTP server
const server = createServer(async (req, res) => {
  // Log all requests
  console.log(`📥 ${req.method} ${req.url}`);

  // CORS headers
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type, X-Filename");

  if (req.method === "OPTIONS") {
    res.writeHead(200);
    res.end();
    return;
  }

  // Health check
  if (req.url === "/health" && req.method === "GET") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ status: "ok", bundleReady: !!bundleLocation }));
    return;
  }

  // List compositions
  if (req.url === "/compositions" && req.method === "GET") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(
      JSON.stringify({
        compositions: [
          { id: "TextOverlay", description: "Animated text with title and subtitle" },
          { id: "VideoIntro", description: "Brand intro animation" },
          { id: "DynamicVideo", description: "Data-driven video with message" },
          { id: "LipSyncMain", description: "Avatar lip-sync video template" },
          { id: "LipSyncBusiness", description: "Business theme with lipsync avatar and B-roll" },
        ],
      })
    );
    return;
  }

  // Transcribe video to captions
  if (req.url === "/transcribe" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => { body += chunk; });
    req.on("end", async () => {
      try {
        const { videoUrl, language = "ru", fps = 30 } = JSON.parse(body);

        if (!videoUrl) {
          res.writeHead(400, { "Content-Type": "application/json" });
          res.end(JSON.stringify({ error: "videoUrl is required" }));
          return;
        }

        console.log(`🎤 Transcribing: ${videoUrl} (${language})`);
        const result = await transcribeVideo(videoUrl, language, fps);

        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: true,
          captions: result.captions,
          segments: result.segments,
        }));
      } catch (error) {
        console.error("Transcription error:", error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          error: error instanceof Error ? error.message : "Transcription failed"
        }));
      }
    });
    return;
  }

  // Analyze face in video/image
  if (req.url === "/analyze-face" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => { body += chunk; });
    req.on("end", async () => {
      try {
        const { videoUrl, imageUrl, shape = "portrait" } = JSON.parse(body);
        const mediaUrl = videoUrl || imageUrl;

        if (!mediaUrl) {
          res.writeHead(400, { "Content-Type": "application/json" });
          res.end(JSON.stringify({ error: "videoUrl or imageUrl is required" }));
          return;
        }

        console.log(`👤 Analyzing face in: ${mediaUrl}`);

        // Resolve path for local files
        let filePath = mediaUrl;
        if (mediaUrl.startsWith("/") && !mediaUrl.startsWith("//")) {
          filePath = path.join(process.cwd(), "public", mediaUrl);
        }

        // Detect face
        const isVideo = mediaUrl.endsWith(".mp4") || mediaUrl.endsWith(".webm") || mediaUrl.endsWith(".mov");
        const faceBox = isVideo
          ? await detectFaceInVideo(filePath)
          : await detectFaceInImage(filePath);

        if (!faceBox) {
          res.writeHead(200, { "Content-Type": "application/json" });
          res.end(JSON.stringify({
            success: true,
            faceDetected: false,
            message: "No face detected in media"
          }));
          return;
        }

        // Calculate crop settings
        const cropSettings = calculateCropSettings(faceBox, shape as "square" | "portrait" | "circle");

        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: true,
          faceDetected: true,
          faceBox,
          cropSettings,
        }));
      } catch (error) {
        console.error("Face analysis error:", error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          error: error instanceof Error ? error.message : "Face analysis failed"
        }));
      }
    });
    return;
  }

  // Upload asset to S3
  if (req.url === "/upload" && req.method === "POST") {
    const chunks: Buffer[] = [];

    req.on("data", (chunk: Buffer) => {
      chunks.push(chunk);
    });

    req.on("end", async () => {
      try {
        const fileBuffer = Buffer.concat(chunks);
        const filename = (req.headers["x-filename"] as string) || `file-${Date.now()}`;
        const contentType = (req.headers["content-type"] as string) || getContentType(filename);

        if (fileBuffer.length === 0) {
          res.writeHead(400, { "Content-Type": "application/json" });
          res.end(JSON.stringify({ error: "No file data received" }));
          return;
        }

        // Check file size (max 100MB)
        const maxSize = 100 * 1024 * 1024;
        if (fileBuffer.length > maxSize) {
          res.writeHead(413, { "Content-Type": "application/json" });
          res.end(JSON.stringify({ error: "File too large (max 100MB)" }));
          return;
        }

        const result = await uploadToS3(fileBuffer, filename, contentType);
        const statusCode = result.success ? 200 : 500;
        res.writeHead(statusCode, { "Content-Type": "application/json" });
        res.end(JSON.stringify(result));
      } catch (error) {
        console.error("Upload error:", error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ error: "Upload failed" }));
      }
    });

    req.on("error", (error) => {
      console.error("Request error:", error);
      res.writeHead(500, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ error: "Request error" }));
    });

    return;
  }

  // List S3 assets
  if (req.url === "/assets" && req.method === "GET") {
    const result = await listS3Assets();
    const statusCode = result.success ? 200 : 500;
    res.writeHead(statusCode, { "Content-Type": "application/json" });
    res.end(JSON.stringify(result));
    return;
  }

  // Transcribe video to captions using Whisper
  if (req.url === "/transcribe" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => {
      body += chunk.toString();
    });

    req.on("end", async () => {
      try {
        const { videoUrl, language = "ru", fps = 30 } = JSON.parse(body);

        if (!videoUrl) {
          res.writeHead(400, { "Content-Type": "application/json" });
          res.end(JSON.stringify({ error: "videoUrl is required" }));
          return;
        }

        console.log(`🎤 Starting transcription: ${videoUrl} (${language})`);

        const result = await transcribeVideo(videoUrl, language, fps);

        console.log(`✅ Transcription complete: ${result.captions.length} captions`);

        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: true,
          language,
          captions: result.captions,
          segments: result.segments,
        }));
      } catch (error) {
        console.error("❌ Transcription failed:", error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: false,
          error: error instanceof Error ? error.message : "Transcription failed",
        }));
      }
    });
    return;
  }

  // Serve rendered files
  if (req.url?.startsWith("/renders/") && req.method === "GET") {
    // Strip query string from URL
    const urlPath = req.url.split('?')[0];
    const filename = urlPath.replace("/renders/", "");
    const filePath = path.join(OUTPUT_DIR, filename);

    serveStaticFile(res, filePath);
    return;
  }

  // Serve public assets (with /public/ prefix)
  if (req.url?.startsWith("/public/") && req.method === "GET") {
    // Strip query string from URL
    const urlPath = req.url.split('?')[0];
    const filename = urlPath.replace("/public/", "");
    const filePath = path.join(process.cwd(), "public", filename);
    console.log(`📂 Request for public file: ${req.url} -> ${filePath}`);

    serveStaticFile(res, filePath);
    return;
  }

  // Serve public assets (without /public/ prefix - for editor compatibility)
  // Handles: /covers/*, /backgrounds/*, /lipsync/*, /music/*
  const publicPaths = ["/covers/", "/backgrounds/", "/lipsync/", "/music/", "/audio/"];
  const matchedPath = publicPaths.find(p => req.url?.startsWith(p));
  if (matchedPath && req.method === "GET") {
    // Strip query string from URL before building file path
    const urlPath = req.url!.split('?')[0];
    const filePath = path.join(process.cwd(), "public", urlPath);
    console.log(`📂 Request for public file (no prefix): ${req.url} -> ${filePath}`);

    serveStaticFile(res, filePath);
    return;
  }

  // Cache control by file type - JSON needs frequent updates, video/images can be cached
  function getCacheControl(filePath: string): string {
    // JSON files - no cache (captions.json changes frequently)
    if (filePath.endsWith('.json')) {
      return "public, max-age=0, must-revalidate";
    }
    // Video - 1 hour (balance between performance and freshness)
    if (/\.(mp4|webm|mov|avi)$/i.test(filePath)) {
      return "public, max-age=3600";
    }
    // Images - 1 day
    if (/\.(jpg|jpeg|png|gif|webp|svg)$/i.test(filePath)) {
      return "public, max-age=86400";
    }
    // Default - 1 hour
    return "public, max-age=3600";
  }

  function serveStaticFile(res: any, filePath: string) {
    console.log(`📂 Serving file: ${filePath}`);
    if (fs.existsSync(filePath)) {
      console.log(`✅ File found: ${filePath}`);
      const stat = fs.statSync(filePath);
      const ext = path.extname(filePath).toLowerCase();
      const contentType =
        ext === ".mp4"
          ? "video/mp4"
          : ext === ".mp3"
          ? "audio/mpeg"
          : ext === ".wav"
          ? "audio/wav"
          : ext === ".jpg" || ext === ".jpeg"
          ? "image/jpeg"
          : ext === ".png"
          ? "image/png"
          : ext === ".gif"
          ? "image/gif"
          : ext === ".json"
          ? "application/json"
          : "application/octet-stream";

      // Handle Range requests (essential for video seek)
      const range = req.headers.range;
      if (range) {
        const parts = range.replace(/bytes=/, "").split("-");
        const start = parseInt(parts[0], 10);
        const end = parts[1] ? parseInt(parts[1], 10) : stat.size - 1;
        
        if (start >= stat.size) {
          res.writeHead(416, {
            "Content-Range": `bytes */${stat.size}`,
          });
          return res.end();
        }

        const chunksize = end - start + 1;
        const file = fs.createReadStream(filePath, { start, end });
        
        res.writeHead(206, {
          "Content-Range": `bytes ${start}-${end}/${stat.size}`,
          "Accept-Ranges": "bytes",
          "Content-Length": chunksize,
          "Content-Type": contentType,
          "Cache-Control": getCacheControl(filePath),
        });
        file.pipe(res);
        return;
      }

      res.writeHead(200, {
        "Content-Type": contentType,
        "Content-Length": stat.size,
        "Accept-Ranges": "bytes",
        "Cache-Control": getCacheControl(filePath),
      });
      fs.createReadStream(filePath).pipe(res);
      return;
    }

    console.log(`❌ File NOT found: ${filePath}`);
    res.writeHead(404, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ error: "File not found", path: filePath }));
  }

  // SSE endpoint for render progress streaming
  const sseMatch = req.url?.match(/^\/render\/([^/]+)\/status$/);
  if (sseMatch && req.method === "GET") {
    const renderId = sseMatch[1];
    const job = renderJobs.get(renderId);

    if (!job) {
      res.writeHead(404, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ error: "Render job not found" }));
      return;
    }

    // Set SSE headers
    res.writeHead(200, {
      "Content-Type": "text/event-stream",
      "Cache-Control": "no-cache",
      "Connection": "keep-alive",
      "Access-Control-Allow-Origin": "*",
    });

    console.log(`[SSE] Client subscribed to render ${renderId}`);

    // Send initial status
    res.write(`data: ${JSON.stringify({
      status: job.status,
      progress: job.progress,
      outputUrl: job.outputUrl,
      error: job.error
    })}\n\n`);

    // Poll for updates every 500ms
    const interval = setInterval(() => {
      const currentJob = renderJobs.get(renderId);
      if (!currentJob) {
        res.write(`data: ${JSON.stringify({ status: 'not_found' })}\n\n`);
        clearInterval(interval);
        res.end();
        return;
      }

      res.write(`data: ${JSON.stringify({
        status: currentJob.status,
        progress: currentJob.progress,
        outputUrl: currentJob.outputUrl,
        error: currentJob.error
      })}\n\n`);

      // Close connection when job is done
      if (currentJob.status === 'completed' || currentJob.status === 'failed') {
        console.log(`[SSE] Render ${renderId} finished, closing SSE connection`);
        clearInterval(interval);
        res.end();
      }
    }, 500);

    // Clean up on client disconnect
    req.on("close", () => {
      console.log(`[SSE] Client disconnected from render ${renderId}`);
      clearInterval(interval);
    });

    return;
  }

  // Get render status (polling alternative to SSE)
  const statusMatch = req.url?.match(/^\/render\/([^/]+)$/);
  if (statusMatch && req.method === "GET") {
    const renderId = statusMatch[1];
    const job = renderJobs.get(renderId);

    if (!job) {
      res.writeHead(404, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ error: "Render job not found" }));
      return;
    }

    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({
      id: job.id,
      status: job.status,
      progress: job.progress,
      outputUrl: job.outputUrl,
      error: job.error,
    }));
    return;
  }

  // Render endpoint - starts async render and returns immediately
  if (req.url === "/render" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => {
      body += chunk.toString();
    });

    req.on("end", async () => {
      try {
        const renderReq: RenderRequest = JSON.parse(body);

        if (!renderReq.compositionId) {
          res.writeHead(400, { "Content-Type": "application/json" });
          res.end(JSON.stringify({ error: "compositionId is required" }));
          return;
        }

        // Start render asynchronously and return immediately
        const renderId = startRenderAsync(renderReq);
        console.log(`🎬 Started async render: ${renderId}`);

        res.writeHead(202, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: true,
          renderId,
          statusUrl: `/render/${renderId}`,
          sseUrl: `/render/${renderId}/status`,
          message: "Render started. Use SSE endpoint for progress updates.",
        }));
      } catch (error) {
        res.writeHead(400, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ error: "Invalid JSON" }));
      }
    });
    return;
  }

  // 404 for everything else
  res.writeHead(404, { "Content-Type": "application/json" });
  res.end(JSON.stringify({ error: "Not found" }));
});

// ===============================
// WebSocket Server for Real-Time Sync
// ===============================

const wss = new WebSocketServer({ server });
const wsClients = new Set<WebSocket>();

interface WSMessage {
  type: string;
  payload?: unknown;
  clientId?: string;
}

wss.on("connection", (ws: WebSocket) => {
  const clientId = randomUUID();
  wsClients.add(ws);
  console.log(`[WS] Client connected: ${clientId} (total: ${wsClients.size})`);

  // Send welcome message with client ID
  ws.send(JSON.stringify({ type: "connected", payload: { clientId } }));

  ws.on("message", (data: Buffer) => {
    try {
      const msg: WSMessage = JSON.parse(data.toString());
      msg.clientId = clientId;

      console.log(`[WS] Message from ${clientId}: ${msg.type}`);

      // Broadcast to all OTHER clients (exclude sender)
      broadcastWS(msg, ws);
    } catch (error) {
      console.error("[WS] Failed to parse message:", error);
    }
  });

  ws.on("close", () => {
    wsClients.delete(ws);
    console.log(`[WS] Client disconnected: ${clientId} (total: ${wsClients.size})`);
  });

  ws.on("error", (error) => {
    console.error(`[WS] Client error: ${clientId}`, error);
    wsClients.delete(ws);
  });
});

function broadcastWS(message: WSMessage, exclude?: WebSocket) {
  const data = JSON.stringify(message);
  let sent = 0;

  wsClients.forEach((client) => {
    if (client !== exclude && client.readyState === WebSocket.OPEN) {
      client.send(data);
      sent++;
    }
  });

  if (sent > 0) {
    console.log(`[WS] Broadcast ${message.type} to ${sent} clients`);
  }
}

// Export broadcast for use in handlers
export { broadcastWS };

// Start server
async function main() {
  await initBundle();

  server.listen(Number(PORT), "0.0.0.0", () => {
    console.log(`🚀 Remotion render server running on 0.0.0.0:${PORT}`);
    console.log(`📍 HTTP Endpoints:`);
    console.log(`   GET  /health       - Health check`);
    console.log(`   GET  /compositions - List compositions`);
    console.log(`   POST /render       - Render video/still`);
    console.log(`   GET  /renders/:id  - Download rendered file`);
    console.log(`   POST /upload       - Upload asset to S3`);
    console.log(`   GET  /assets       - List S3 assets`);
    console.log(`   POST /transcribe   - Transcribe audio to captions (RU/EN)`);
    console.log(`🔌 WebSocket: ws://0.0.0.0:${PORT} (real-time sync)`);
    console.log(`📦 S3 Bucket: ${S3_BUCKET}`);
  });
}

main().catch(console.error);
