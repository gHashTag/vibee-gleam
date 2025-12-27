import { createServer, IncomingMessage } from "node:http";
import os from "node:os";
import { WebSocketServer, WebSocket } from "ws";
import { bundle } from "@remotion/bundler";
import { renderMedia, selectComposition, renderStill } from "@remotion/renderer";
import path from "node:path";
import fs from "node:fs";
import { randomUUID, createHmac } from "node:crypto";
import { execSync } from "node:child_process";
import { S3Client, PutObjectCommand, ListObjectsV2Command, GetObjectCommand } from "@aws-sdk/client-s3";
import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
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

// Optimal concurrency based on CPU cores (75% of available cores, min 2)
const OPTIMAL_CONCURRENCY = Math.max(2, Math.floor(os.cpus().length * 0.75));
console.log(`🔧 CPU cores: ${os.cpus().length}, using concurrency: ${OPTIMAL_CONCURRENCY}`);

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

// Telegram Notification Configuration
const TELEGRAM_BOT_TOKEN = process.env.TELEGRAM_BOT_TOKEN;
const TELEGRAM_OWNER_ID = '144022504';
const TELEGRAM_RENDERS_GROUP = '-1002737186844';

// Send text message to Telegram
async function sendTelegramMessage(chatId: string, message: string): Promise<boolean> {
  if (!TELEGRAM_BOT_TOKEN) {
    console.warn('⚠️ TELEGRAM_BOT_TOKEN not set, skipping notification');
    return false;
  }
  try {
    const response = await fetch(`https://api.telegram.org/bot${TELEGRAM_BOT_TOKEN}/sendMessage`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        chat_id: chatId,
        text: message,
        parse_mode: 'HTML'
      })
    });
    const result = await response.json();
    if (!result.ok) {
      console.error('❌ Telegram sendMessage failed:', result);
      return false;
    }
    console.log(`📨 Telegram notification sent to ${chatId}`);
    return true;
  } catch (error) {
    console.error('❌ Telegram sendMessage error:', error);
    return false;
  }
}

// Send video to Telegram
async function sendTelegramVideo(chatId: string, videoUrl: string, caption: string): Promise<boolean> {
  if (!TELEGRAM_BOT_TOKEN) {
    console.warn('⚠️ TELEGRAM_BOT_TOKEN not set, skipping video notification');
    return false;
  }
  try {
    const response = await fetch(`https://api.telegram.org/bot${TELEGRAM_BOT_TOKEN}/sendVideo`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        chat_id: chatId,
        video: videoUrl,
        caption: caption,
        parse_mode: 'HTML'
      })
    });
    const result = await response.json();
    if (!result.ok) {
      console.error('❌ Telegram sendVideo failed:', result);
      // Fallback to message with link
      return sendTelegramMessage(chatId, `${caption}\n\n🔗 ${videoUrl}`);
    }
    console.log(`📹 Telegram video sent to ${chatId}`);
    return true;
  } catch (error) {
    console.error('❌ Telegram sendVideo error:', error);
    return false;
  }
}

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

// Temp directory for pre-downloaded render assets (in public so Remotion can serve)
const RENDER_TEMP_DIR = path.join(process.cwd(), "public", "render-temp");
if (!fs.existsSync(RENDER_TEMP_DIR)) {
  fs.mkdirSync(RENDER_TEMP_DIR, { recursive: true });
}

/**
 * Pre-download S3 asset to temp directory for faster rendering
 * Also transcodes to H.264 if needed (iPhone HEVC videos don't work in Chrome)
 * Returns a file:// URL that Remotion can access directly
 */
async function preDownloadS3Asset(url: string): Promise<string> {
  let s3Key: string | null = null;

  if (url.includes('/s3/')) {
    const match = url.match(/\/s3\/(.+)$/);
    if (match) {
      s3Key = match[1];
    }
  }

  if (!s3Key) {
    return url;
  }

  const baseName = path.basename(s3Key, path.extname(s3Key));
  const downloadFilename = `${Date.now()}-${path.basename(s3Key)}`;
  const downloadPath = path.join(RENDER_TEMP_DIR, downloadFilename);

  // Output will always be .mp4 H.264
  const outputFilename = `${Date.now()}-${baseName}-h264.mp4`;
  const outputPath = path.join(RENDER_TEMP_DIR, outputFilename);

  console.log(`📥 Pre-downloading S3 asset: ${s3Key}`);

  try {
    // Download from S3
    const command = new GetObjectCommand({
      Bucket: S3_BUCKET,
      Key: s3Key,
    });

    const response = await s3Client.send(command);
    const body = response.Body as NodeJS.ReadableStream;

    await new Promise<void>((resolve, reject) => {
      const file = fs.createWriteStream(downloadPath);
      body.pipe(file);
      file.on('finish', () => { file.close(); resolve(); });
      file.on('error', reject);
    });

    const stats = fs.statSync(downloadPath);
    console.log(`✅ Downloaded ${(stats.size / 1024 / 1024).toFixed(2)} MB`);

    // Transcode to H.264 (Chrome-compatible) using ffmpeg
    console.log(`🔄 Transcoding to H.264...`);
    try {
      execSync(
        `ffmpeg -i "${downloadPath}" -c:v libx264 -preset fast -crf 23 -c:a aac -movflags +faststart -y "${outputPath}"`,
        { stdio: 'pipe', timeout: 300000 }
      );

      // Remove original download
      fs.unlinkSync(downloadPath);

      const outputStats = fs.statSync(outputPath);
      console.log(`✅ Transcoded to ${(outputStats.size / 1024 / 1024).toFixed(2)} MB`);

      // Return HTTP URL for Remotion to access via render server
      return `http://0.0.0.0:${PORT}/render-temp/${outputFilename}`;
    } catch (transcodeError) {
      console.warn(`⚠️ Transcode failed, using original:`, transcodeError);
      // If transcode fails, use original via HTTP
      return `http://0.0.0.0:${PORT}/render-temp/${downloadFilename}`;
    }
  } catch (error) {
    console.error(`❌ Failed to pre-download:`, error);
    return url;
  }
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
): Promise<{ success: boolean; url?: string; key?: string; error?: string; signedUrl?: string }> {
  const key = `assets/${Date.now()}-${filename}`;

  try {
    await s3Client.send(new PutObjectCommand({
      Bucket: S3_BUCKET,
      Key: key,
      Body: fileBuffer,
      ContentType: contentType,
    }));

    // Generate presigned URL for public access (7 days)
    const signedUrl = await getSignedUrl(
      s3Client,
      new GetObjectCommand({ Bucket: S3_BUCKET, Key: key }),
      { expiresIn: 604800 } // 7 days
    );

    // Return proxy URL instead of direct S3 URL for browser compatibility (HEVC → H.264)
    const proxyUrl = `/s3/${key}`;
    const directUrl = `${S3_PUBLIC_URL}/${key}`;
    console.log(`✅ Uploaded to S3: ${directUrl} (signed: ${signedUrl.substring(0, 80)}...)`);
    return { success: true, url: proxyUrl, key, directUrl, signedUrl };
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

// Webhook callback with retry
async function sendWebhook(
  url: string,
  payload: WebhookPayload,
  secret?: string,
  retryCount = 0
): Promise<void> {
  const maxRetries = 3;
  const retryDelays = [0, 5000, 15000];

  try {
    const body = JSON.stringify(payload);
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
    };

    if (secret) {
      const signature = createHmac("sha256", secret).update(body).digest("hex");
      headers["X-Vibee-Signature"] = `sha256=${signature}`;
    }

    const response = await fetch(url, {
      method: "POST",
      headers,
      body,
      signal: AbortSignal.timeout(10000),
    });

    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    console.log(`✅ [Webhook] Sent to ${url}`);
  } catch (error) {
    console.error(`❌ [Webhook] Failed attempt ${retryCount + 1}:`, error);
    if (retryCount < maxRetries - 1) {
      const delay = retryDelays[retryCount + 1];
      console.log(`🔄 [Webhook] Retrying in ${delay}ms...`);
      setTimeout(() => sendWebhook(url, payload, secret, retryCount + 1), delay);
    }
  }
}

// Upload rendered file to S3
async function uploadRenderedFile(
  filePath: string,
  prefix: string = "renders/"
): Promise<{ success: boolean; url?: string; error?: string }> {
  try {
    const fileBuffer = fs.readFileSync(filePath);
    const filename = path.basename(filePath);
    const key = `${prefix}${Date.now()}-${filename}`;

    await s3Client.send(new PutObjectCommand({
      Bucket: S3_BUCKET,
      Key: key,
      Body: fileBuffer,
      ContentType: "video/mp4",
    }));

    const url = `${S3_PUBLIC_URL}/${key}`;
    console.log(`✅ [S3] Uploaded rendered video: ${url}`);
    return { success: true, url };
  } catch (error) {
    console.error("❌ [S3] Failed to upload rendered file:", error);
    return {
      success: false,
      error: error instanceof Error ? error.message : "Upload failed",
    };
  }
}

// Convert simplified segments (seconds) to composition segments (frames)
function convertSegmentsToFrames(segments: SimplifiedSegment[], fps = 30) {
  return segments.map((seg) => ({
    type: seg.type,
    startFrame: Math.round(seg.startSeconds * fps),
    durationFrames: Math.round(seg.durationSeconds * fps),
    bRollUrl: seg.bRollUrl,
    bRollType: seg.bRollUrl?.match(/\.(jpg|jpeg|png|gif|webp)$/i) ? 'image' as const : 'video' as const,
    caption: '',
  }));
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
  // User info for notifications
  userInfo?: {
    telegram_id: number;
    username?: string;
    first_name?: string;
    project_name?: string;
  };
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
  publicUrl?: string;  // S3 public URL for notifications
  error?: string;
  startedAt: Date;
  userInfo?: RenderRequest['userInfo'];
}

// Universal template render API types
interface TemplateRenderRequest {
  // Required
  compositionId: string;           // Template name: "SplitTalkingHead", "LipSyncMain", etc.

  // Common props (used by most templates)
  lipSyncVideo?: string;
  segments?: SimplifiedSegment[];
  captions?: Caption[];

  // Template-specific props (passed through as-is)
  props?: Record<string, unknown>;

  // Render options
  uploadToS3?: boolean;            // default: true
  s3Prefix?: string;               // default: "renders/"
  webhookUrl?: string;             // POST on completion
  webhookSecret?: string;          // HMAC signature

  // User info for notifications
  userInfo?: {
    telegram_id: number;
    username?: string;
    first_name?: string;
    project_name?: string;
  };
}

interface SimplifiedSegment {
  type: 'split' | 'fullscreen';
  startSeconds: number;
  durationSeconds: number;
  bRollUrl?: string;
}

interface Caption {
  text: string;
  startMs: number;
  endMs: number;
}

interface WebhookPayload {
  renderId: string;
  status: 'completed' | 'failed';
  publicUrl?: string;
  error?: string;
  renderTimeMs: number;
  timestamp: string;
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

  // Create job entry with userInfo for notifications
  renderJobs.set(renderId, {
    id: renderId,
    status: 'pending',
    progress: 0,
    startedAt: new Date(),
    userInfo: req.userInfo,
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
      let lipSyncVideo = inputProps.lipSyncVideo as string | undefined;

      // Pre-download S3 assets for faster rendering (avoids HTTP timeout in Chrome)
      if (lipSyncVideo && lipSyncVideo.includes('/s3/')) {
        console.log(`📥 Pre-downloading lipSyncVideo for render...`);
        lipSyncVideo = await preDownloadS3Asset(lipSyncVideo);
        inputProps.lipSyncVideo = lipSyncVideo;
      }

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
        timeoutInMilliseconds: 300000, // 5 minutes for slow video loading
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
          timeoutInMilliseconds: 300000, // 5 minutes for slow video loading
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
        concurrency: OPTIMAL_CONCURRENCY,
        audioCodec: 'mp3', // Faster than AAC for concatenation
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
        timeoutInMilliseconds: 300000, // 5 minutes for slow video loading
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

      // Upload to S3 and send Telegram notification
      try {
        const videoBuffer = fs.readFileSync(outputPath);
        const uploadResult = await uploadToS3(videoBuffer, `render-${renderId}.${ext}`, ext === 'gif' ? 'image/gif' : 'video/mp4');

        if (uploadResult.success && uploadResult.signedUrl) {
          job.publicUrl = uploadResult.signedUrl;
          console.log(`📤 Uploaded to S3 with signed URL`);

          // Send Telegram notification with video
          const renderTimeMs = Date.now() - job.startedAt.getTime();
          const renderTimeSec = Math.round(renderTimeMs / 1000);
          const userInfo = job.userInfo;

          const caption = userInfo
            ? `✅ <b>Рендер готов!</b>\n\n👤 ${userInfo.first_name || 'Unknown'} (@${userInfo.username || 'нет'})\n📹 ${userInfo.project_name || 'Untitled'}\n⏱ ${renderTimeSec}s`
            : `✅ <b>Рендер готов!</b>\n\n⏱ ${renderTimeSec}s`;

          await sendTelegramVideo(TELEGRAM_RENDERS_GROUP, uploadResult.signedUrl, caption);
          console.log(`📱 Telegram notification sent for render ${renderId}`);
        }
      } catch (uploadError) {
        console.error(`⚠️ S3 upload or notification failed for ${renderId}:`, uploadError);
        // Don't fail the render if upload/notification fails
      }

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

  // Notify: New lead (user login)
  if (req.url === "/api/notify/lead" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => { body += chunk; });
    req.on("end", async () => {
      try {
        const { telegram_id, username, first_name } = JSON.parse(body);
        const message = `🐝 <b>Новый пользователь VIBEE!</b>\n\n👤 ${first_name || 'Unknown'}\n📱 @${username || 'нет'}\n🆔 ${telegram_id}`;
        await sendTelegramMessage(TELEGRAM_OWNER_ID, message);
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ ok: true }));
      } catch (error) {
        console.error('Lead notification error:', error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ ok: false, error: 'Failed to send notification' }));
      }
    });
    return;
  }

  // Notify: Render started
  if (req.url === "/api/notify/render-start" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => { body += chunk; });
    req.on("end", async () => {
      try {
        const { telegram_id, username, first_name, project_name } = JSON.parse(body);
        const message = `🎬 <b>Рендер запущен</b>\n\n👤 ${first_name || 'Unknown'} (@${username || 'нет'})\n🆔 ${telegram_id}\n📹 ${project_name || 'Untitled'}`;
        await sendTelegramMessage(TELEGRAM_RENDERS_GROUP, message);
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ ok: true }));
      } catch (error) {
        console.error('Render start notification error:', error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ ok: false, error: 'Failed to send notification' }));
      }
    });
    return;
  }

  // ==============================================
  // AI Generation Endpoints
  // ==============================================
  const MCP_URL = process.env.MCP_URL || "https://vibee-mcp.fly.dev";
  const FAL_KEY = process.env.FAL_KEY;

  // Supported fal.ai image models
  const FAL_IMAGE_MODELS: Record<string, string> = {
    'fal-ai/flux-pro/v1.1-ultra': 'fal-ai/flux-pro/v1.1-ultra',
    'fal-ai/flux/dev': 'fal-ai/flux/dev',
    'fal-ai/nano-banana-pro': 'fal-ai/nano-banana-pro',
    'fal-ai/reve/text-to-image': 'fal-ai/reve/text-to-image',
  };

  // POST /api/generate/image - Generate image using FAL.ai (multiple models)
  if (req.url === "/api/generate/image" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => { body += chunk; });
    req.on("end", async () => {
      try {
        if (!FAL_KEY) throw new Error("FAL_KEY not configured");

        const { model, prompt, width, height } = JSON.parse(body);
        console.log(`📷 [Generate] Photo: ${model}, prompt: "${prompt.substring(0, 50)}..."`);

        // Convert width/height to aspect ratio for FAL
        const getAspectRatio = (w: number, h: number): string => {
          if (w === h) return "1:1";
          if (w > h) return w / h >= 1.7 ? "16:9" : "4:3";
          return h / w >= 1.7 ? "9:16" : "3:4";
        };
        const aspectRatio = getAspectRatio(width || 1024, height || 1024);

        // Get model endpoint (default to nano-banana-pro)
        const modelEndpoint = FAL_IMAGE_MODELS[model] || 'fal-ai/nano-banana-pro';

        // Submit job to FAL queue
        const submitResponse = await fetch(`https://queue.fal.run/${modelEndpoint}`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            "Authorization": `Key ${FAL_KEY}`,
          },
          body: JSON.stringify({
            prompt,
            aspect_ratio: aspectRatio,
            num_images: 1,
          }),
        });

        if (!submitResponse.ok) {
          const errorText = await submitResponse.text();
          throw new Error(`FAL submit failed: ${submitResponse.status} - ${errorText}`);
        }

        const submitResult = await submitResponse.json();
        const requestId = submitResult.request_id;
        console.log(`📷 [Generate] FAL request submitted: ${requestId}`);

        if (!requestId) {
          // Synchronous response - image is already ready
          const imageUrl = submitResult.images?.[0]?.url || submitResult.image?.url;
          if (imageUrl) {
            res.writeHead(200, { "Content-Type": "application/json" });
            res.end(JSON.stringify({ success: true, url: imageUrl, id: Date.now().toString() }));
            return;
          }
          throw new Error("No image URL in response");
        }

        // Poll for completion (async queue mode)
        let imageUrl = null;
        for (let i = 0; i < 120; i++) { // Max 6 minutes (120 * 3s)
          await new Promise(r => setTimeout(r, 3000));

          // Check status
          const statusResponse = await fetch(
            `https://queue.fal.run/${modelEndpoint}/requests/${requestId}/status`,
            { headers: { "Authorization": `Key ${FAL_KEY}` } }
          );

          if (!statusResponse.ok) continue;
          const statusData = await statusResponse.json();
          const status = statusData.status;

          console.log(`📷 [Generate] FAL status: ${status}`);

          if (status === "COMPLETED") {
            // Get result
            const resultResponse = await fetch(
              `https://queue.fal.run/${modelEndpoint}/requests/${requestId}`,
              { headers: { "Authorization": `Key ${FAL_KEY}` } }
            );

            if (resultResponse.ok) {
              const resultData = await resultResponse.json();
              imageUrl = resultData.images?.[0]?.url || resultData.image?.url;
            }
            break;
          } else if (status === "FAILED") {
            throw new Error("FAL generation failed: " + (statusData.error || "Unknown error"));
          }
        }

        if (imageUrl) {
          res.writeHead(200, { "Content-Type": "application/json" });
          res.end(JSON.stringify({ success: true, url: imageUrl, id: requestId }));
          return;
        }

        throw new Error("Image generation timeout");
      } catch (error) {
        console.error("❌ [Generate] Image error:", error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ success: false, error: error instanceof Error ? error.message : "Generation failed" }));
      }
    });
    return;
  }

  // POST /api/generate/video - Generate video using Kling/Veo3
  if (req.url === "/api/generate/video" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => { body += chunk; });
    req.on("end", async () => {
      try {
        const { model, prompt, duration, aspect_ratio } = JSON.parse(body);
        console.log(`🎬 [Generate] Video: ${model}, duration: ${duration}`);

        // Determine which API to use based on model
        const isKling = model.startsWith("kling");
        const toolName = isKling ? "ai_kling_create_video" : "ai_kie_create_video";
        const mode = model.includes("pro") ? "pro" : "std";

        const mcpResponse = await fetch(`${MCP_URL}/mcp`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            jsonrpc: "2.0",
            method: "tools/call",
            params: {
              name: toolName,
              arguments: { prompt, mode, duration: duration.replace("s", ""), aspect_ratio },
            },
            id: Date.now(),
          }),
        });

        const mcpResult = await mcpResponse.json();
        if (mcpResult.error) throw new Error(mcpResult.error.message);

        const content = mcpResult.result?.content?.[0]?.text;
        if (!content) throw new Error("No result from MCP");

        const data = JSON.parse(content);
        if (!data.success) throw new Error(data.error || "Generation failed");

        // Poll for result (video generation is async)
        const taskId = data.data?.task_id;
        if (taskId) {
          let videoUrl = null;
          const pollTool = isKling ? "ai_kling_get_task" : "ai_kie_get_task";
          for (let i = 0; i < 100; i++) { // Max 5 minutes
            await new Promise(r => setTimeout(r, 3000));
            const statusResp = await fetch(`${MCP_URL}/mcp`, {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify({
                jsonrpc: "2.0",
                method: "tools/call",
                params: {
                  name: pollTool,
                  arguments: { task_id: taskId },
                },
                id: Date.now(),
              }),
            });
            const statusResult = await statusResp.json();
            const statusContent = statusResult.result?.content?.[0]?.text;
            if (statusContent) {
              const statusData = JSON.parse(statusContent);
              if (statusData.success && statusData.data?.status === "completed") {
                videoUrl = statusData.data.video_url || statusData.data.works?.[0]?.resource?.resource;
                break;
              }
            }
          }
          if (videoUrl) {
            res.writeHead(200, { "Content-Type": "application/json" });
            res.end(JSON.stringify({ success: true, url: videoUrl, id: taskId }));
            return;
          }
        }

        throw new Error("Video generation timeout");
      } catch (error) {
        console.error("❌ [Generate] Video error:", error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ success: false, error: error instanceof Error ? error.message : "Generation failed" }));
      }
    });
    return;
  }

  // POST /api/generate/audio - Generate TTS using ElevenLabs (direct API call)
  if (req.url === "/api/generate/audio" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => { body += chunk; });
    req.on("end", async () => {
      try {
        const { text, voice_id, speed } = JSON.parse(body);
        const ELEVENLABS_API_KEY = process.env.ELEVENLABS_API_KEY;

        if (!ELEVENLABS_API_KEY) {
          throw new Error("ELEVENLABS_API_KEY not configured");
        }

        console.log(`🎤 [Generate] Audio: voice=${voice_id}, text="${text.substring(0, 50)}..."`);

        // Call ElevenLabs TTS API directly
        const ttsResponse = await fetch(
          `https://api.elevenlabs.io/v1/text-to-speech/${voice_id}`,
          {
            method: "POST",
            headers: {
              "xi-api-key": ELEVENLABS_API_KEY,
              "Content-Type": "application/json",
              "Accept": "audio/mpeg",
            },
            body: JSON.stringify({
              text,
              model_id: "eleven_multilingual_v2",
              voice_settings: {
                stability: 0.5,
                similarity_boost: 0.75,
              },
            }),
          }
        );

        if (!ttsResponse.ok) {
          const errorText = await ttsResponse.text();
          throw new Error(`ElevenLabs TTS error: ${ttsResponse.status} - ${errorText}`);
        }

        // Get audio buffer
        const audioBuffer = Buffer.from(await ttsResponse.arrayBuffer());
        console.log(`✅ [Generate] Audio received: ${audioBuffer.length} bytes`);

        // Upload to S3
        const filename = `tts-${Date.now()}.mp3`;
        const uploadResult = await uploadToS3(audioBuffer, filename, "audio/mpeg");

        if (!uploadResult.success) {
          throw new Error(uploadResult.error || "Failed to upload audio to S3");
        }

        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: true,
          url: uploadResult.url,
          id: Date.now().toString(),
        }));
      } catch (error) {
        console.error("❌ [Generate] Audio error:", error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: false,
          error: error instanceof Error ? error.message : "Generation failed",
        }));
      }
    });
    return;
  }

  // GET /api/voices - Get available ElevenLabs voices
  if (req.url === "/api/voices" && req.method === "GET") {
    try {
      const ELEVENLABS_API_KEY = process.env.ELEVENLABS_API_KEY;
      if (!ELEVENLABS_API_KEY) {
        throw new Error("ELEVENLABS_API_KEY not configured");
      }

      console.log(`🎤 [Voices] Fetching ElevenLabs voices...`);

      const response = await fetch("https://api.elevenlabs.io/v1/voices", {
        method: "GET",
        headers: {
          "xi-api-key": ELEVENLABS_API_KEY,
          "Content-Type": "application/json",
        },
      });

      if (!response.ok) {
        throw new Error(`ElevenLabs API error: ${response.status}`);
      }

      const data = await response.json();
      const voices = data.voices || [];

      console.log(`✅ [Voices] Found ${voices.length} voices`);

      // Filter out voices that are not fine-tuned (professional clones need fine-tuning)
      const readyVoices = voices.filter((voice: any) => {
        // Premade voices always work
        if (voice.category === "premade") {
          return true;
        }

        // Check fine_tuning status for cloned voices
        if (voice.fine_tuning) {
          const state = voice.fine_tuning.fine_tuning_state;
          const isAllowed = voice.fine_tuning.is_allowed_to_fine_tune;

          // Log for debugging
          console.log(`🔍 [Voices] ${voice.name} (${voice.voice_id}): category=${voice.category}, state=${state}, isAllowed=${isAllowed}`);

          // Skip if fine-tuning is required but not complete
          if (state && state !== "fine_tuned" && state !== "not_started") {
            console.log(`⚠️ [Voices] Skipping ${voice.name}: not fine-tuned (state=${state})`);
            return false;
          }

          // Skip if not allowed to use (professional voices that need fine-tuning)
          if (isAllowed === false && state !== "fine_tuned") {
            console.log(`⚠️ [Voices] Skipping ${voice.name}: not allowed and not fine-tuned`);
            return false;
          }
        }
        return true;
      });

      console.log(`✅ [Voices] ${readyVoices.length} ready voices (filtered from ${voices.length})`);

      // Map to simplified format
      const simplifiedVoices = readyVoices.map((voice: any) => ({
        id: voice.voice_id,
        name: voice.name,
        category: voice.category || "custom",
        labels: voice.labels || {},
        preview_url: voice.preview_url,
      }));

      res.writeHead(200, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ success: true, voices: simplifiedVoices }));
    } catch (error) {
      console.error("❌ [Voices] Error:", error);
      res.writeHead(500, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ success: false, error: error instanceof Error ? error.message : "Failed to fetch voices" }));
    }
    return;
  }

  // POST /api/generate/lipsync - Generate lipsync video using fal.ai VEED Fabric
  if (req.url === "/api/generate/lipsync" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => { body += chunk; });
    req.on("end", async () => {
      try {
        const { audio_url, image_url, resolution } = JSON.parse(body);
        console.log(`👄 [Generate] Lipsync via fal.ai VEED Fabric: resolution=${resolution || "720p"}`);

        const FAL_KEY = process.env.FAL_KEY;
        if (!FAL_KEY) throw new Error("FAL_KEY not configured");

        // Submit job to fal.ai queue
        const queueResponse = await fetch("https://queue.fal.run/veed/fabric-1.0", {
          method: "POST",
          headers: {
            "Authorization": `Key ${FAL_KEY}`,
            "Content-Type": "application/json",
          },
          body: JSON.stringify({
            image_url,
            audio_url,
            resolution: resolution || "720p",
          }),
        });

        if (!queueResponse.ok) {
          const errorText = await queueResponse.text();
          throw new Error(`fal.ai queue error: ${queueResponse.status} ${errorText}`);
        }

        const queueResult = await queueResponse.json();
        const requestId = queueResult.request_id;
        console.log(`👄 [fal.ai] Job queued: ${requestId}`);

        // Poll for result
        let videoUrl = null;
        for (let i = 0; i < 120; i++) { // Max 10 minutes (5s intervals)
          await new Promise(r => setTimeout(r, 5000));

          const statusResponse = await fetch(`https://queue.fal.run/veed/fabric-1.0/requests/${requestId}/status`, {
            headers: { "Authorization": `Key ${FAL_KEY}` },
          });
          const statusData = await statusResponse.json();
          console.log(`👄 [fal.ai] Status: ${statusData.status}`);

          if (statusData.status === "COMPLETED") {
            // Get the result
            const resultResponse = await fetch(`https://queue.fal.run/veed/fabric-1.0/requests/${requestId}`, {
              headers: { "Authorization": `Key ${FAL_KEY}` },
            });
            const resultData = await resultResponse.json();
            videoUrl = resultData.video?.url;
            break;
          } else if (statusData.status === "FAILED") {
            throw new Error(`fal.ai job failed: ${statusData.error || "Unknown error"}`);
          }
        }

        if (videoUrl) {
          console.log(`👄 [fal.ai] Video ready: ${videoUrl}`);
          res.writeHead(200, { "Content-Type": "application/json" });
          res.end(JSON.stringify({ success: true, url: videoUrl, id: requestId }));
          return;
        }

        throw new Error("Lipsync generation timeout");
      } catch (error) {
        console.error("❌ [Generate] Lipsync error:", error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ success: false, error: error instanceof Error ? error.message : "Generation failed" }));
      }
    });
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
          { id: "SplitTalkingHead", description: "Split layout with talking head, B-roll and TikTok-style captions" },
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

        console.log(`✅ Transcription complete: ${result.captions.length} captions`);

        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: true,
          captions: result.captions,
          segments: result.segments,
        }));
      } catch (error) {
        console.error("❌ Transcription error:", error);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: false,
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
  const publicPaths = ["/covers/", "/backgrounds/", "/lipsync/", "/music/", "/audio/", "/render-temp/"];
  const matchedPath = publicPaths.find(p => req.url?.startsWith(p));
  if (matchedPath && req.method === "GET") {
    // Strip query string from URL before building file path
    const urlPath = req.url!.split('?')[0];
    const filePath = path.join(process.cwd(), "public", urlPath);
    console.log(`📂 Request for public file (no prefix): ${req.url} -> ${filePath}`);

    serveStaticFile(res, filePath);
    return;
  }

  // S3 video proxy with HEVC → H.264 conversion for browser compatibility
  if (req.url?.startsWith("/s3/") && req.method === "GET") {
    const s3Key = decodeURIComponent(req.url.slice(4).split('?')[0]); // Remove /s3/ and query string, decode URL
    const ext = path.extname(s3Key).toLowerCase();
    const isImage = ['.jpg', '.jpeg', '.png', '.gif', '.webp', '.svg', '.bmp'].includes(ext);
    const isAudio = ['.mp3', '.wav', '.ogg', '.m4a', '.aac', '.webm'].includes(ext);
    const isJson = ext === '.json';

    console.log(`🎬 S3 proxy request: ${s3Key} (image: ${isImage}, audio: ${isAudio}, json: ${isJson})`);

    // For images, audio, and JSON - serve directly from S3 without conversion
    if (isImage || isAudio || isJson) {
      try {
        const command = new GetObjectCommand({
          Bucket: S3_BUCKET,
          Key: s3Key,
        });
        const response = await s3Client.send(command);
        const contentType = response.ContentType || (isImage ? 'image/jpeg' : isAudio ? 'audio/mpeg' : 'application/json');

        res.writeHead(200, {
          "Content-Type": contentType,
          "Content-Length": response.ContentLength?.toString() || '',
          "Cache-Control": "public, max-age=31536000",
          "Access-Control-Allow-Origin": "*",
        });

        const body = response.Body as NodeJS.ReadableStream;
        body.pipe(res);
      } catch (error) {
        console.error(`❌ S3 proxy error for ${s3Key}:`, error);
        res.writeHead(404, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ error: "File not found" }));
      }
      return;
    }

    // For videos - download, convert to H.264, cache
    const cacheDir = path.join(process.cwd(), "public", "video-cache");
    const cacheFilename = `${s3Key.replace(/\//g, '-').replace(/[^a-zA-Z0-9.-]/g, '_')}-h264.mp4`;
    const cachePath = path.join(cacheDir, cacheFilename);

    // Check cache first
    if (fs.existsSync(cachePath)) {
      console.log(`✅ Serving cached H.264 video: ${cachePath}`);
      serveStaticFile(res, cachePath);
      return;
    }

    // Download and convert
    try {
      if (!fs.existsSync(cacheDir)) {
        fs.mkdirSync(cacheDir, { recursive: true });
      }

      const tempPath = path.join(cacheDir, `temp-${Date.now()}.mp4`);

      // Download from S3
      console.log(`📥 Downloading from S3: ${s3Key}`);
      const command = new GetObjectCommand({
        Bucket: S3_BUCKET,
        Key: s3Key,
      });
      const response = await s3Client.send(command);
      const body = response.Body as NodeJS.ReadableStream;

      await new Promise<void>((resolve, reject) => {
        const file = fs.createWriteStream(tempPath);
        body.pipe(file);
        file.on('finish', () => { file.close(); resolve(); });
        file.on('error', reject);
      });

      // Convert to H.264
      console.log(`🔄 Converting to H.264: ${cachePath}`);
      execSync(
        `ffmpeg -i "${tempPath}" -c:v libx264 -preset fast -crf 23 -c:a aac -movflags +faststart -y "${cachePath}"`,
        { stdio: 'pipe', timeout: 300000 }
      );

      // Remove temp file
      fs.unlinkSync(tempPath);

      console.log(`✅ Converted and cached: ${cachePath}`);
      serveStaticFile(res, cachePath);
    } catch (error) {
      console.error(`❌ S3 video proxy error:`, error);
      res.writeHead(500, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ error: "Failed to process video" }));
    }
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
          "Access-Control-Allow-Origin": "*",
        });
        file.pipe(res);
        return;
      }

      res.writeHead(200, {
        "Content-Type": contentType,
        "Content-Length": stat.size,
        "Accept-Ranges": "bytes",
        "Cache-Control": getCacheControl(filePath),
        "Access-Control-Allow-Origin": "*",
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

  // Universal template render endpoint
  if (req.url === "/render/template" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => {
      body += chunk.toString();
    });

    req.on("end", async () => {
      try {
        const request: TemplateRenderRequest = JSON.parse(body);

        if (!request.compositionId) {
          res.writeHead(400, { "Content-Type": "application/json" });
          res.end(JSON.stringify({ error: "compositionId is required" }));
          return;
        }

        const renderId = randomUUID();
        const fps = 30;
        const startTime = Date.now();

        // Create job entry
        renderJobs.set(renderId, {
          id: renderId,
          status: 'pending',
          progress: 0,
          startedAt: new Date(),
        });

        // Return immediately with job info
        res.writeHead(202, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          success: true,
          renderId,
          compositionId: request.compositionId,
          statusUrl: `/render/${renderId}`,
          sseUrl: `/render/${renderId}/status`,
          message: "Render started. Use SSE endpoint for progress updates.",
        }));

        // Start render in background
        (async () => {
          const job = renderJobs.get(renderId)!;
          job.status = 'rendering';

          try {
            if (!bundleLocation) {
              throw new Error("Bundle not initialized");
            }

            // Build input props from request
            const inputProps: Record<string, unknown> = {
              ...(request.props || {}),
            };

            let durationInFrames = 900; // 30 seconds default

            // Handle lipSyncVideo if provided (common for talking head templates)
            let lipSyncVideoPath = request.lipSyncVideo;

            // Pre-download S3 assets for faster rendering
            if (lipSyncVideoPath && lipSyncVideoPath.includes('/s3/')) {
              console.log(`📥 Pre-downloading lipSyncVideo for template render...`);
              lipSyncVideoPath = await preDownloadS3Asset(lipSyncVideoPath);
            }

            if (lipSyncVideoPath) {
              inputProps.lipSyncVideo = lipSyncVideoPath;

              const videoPath = resolveMediaPath(lipSyncVideoPath);
              if (fs.existsSync(videoPath)) {
                // Get video duration
                const duration = getVideoDuration(videoPath);
                if (duration > 0) {
                  durationInFrames = Math.ceil(duration * fps);
                  console.log(`📏 Video duration: ${duration.toFixed(2)}s = ${durationInFrames} frames`);
                }

                // Auto face detection (if not already provided)
                if (inputProps.faceOffsetX === undefined) {
                  try {
                    console.log(`👤 Auto-detecting face in: ${videoPath}`);
                    const faceBox = await detectFaceInVideo(videoPath);
                    if (faceBox) {
                      const crop = calculateCropSettings(faceBox, 'portrait');
                      inputProps.faceOffsetX = crop.offsetX;
                      inputProps.faceOffsetY = crop.offsetY;
                      inputProps.faceScale = crop.scale;
                      console.log(`✅ Face detected: offsetX=${crop.offsetX.toFixed(1)}, offsetY=${crop.offsetY.toFixed(1)}, scale=${crop.scale.toFixed(2)}`);
                    }
                  } catch (e) {
                    console.warn("⚠️ Face detection failed:", e);
                  }
                }
              }
            }

            // Handle segments (convert from seconds to frames)
            if (request.segments) {
              inputProps.segments = convertSegmentsToFrames(request.segments, fps);
            } else if (lipSyncVideoPath && !inputProps.segments) {
              // Default fullscreen segment for lipsync videos
              inputProps.segments = [{ type: 'fullscreen', startFrame: 0, durationFrames: durationInFrames, caption: '' }];
            }

            // Handle captions
            if (request.captions) {
              inputProps.captions = request.captions;
              inputProps.showCaptions = request.captions.length > 0;
            }

            // Apply template-specific defaults
            const templateDefaults: Record<string, Record<string, unknown>> = {
              SplitTalkingHead: {
                splitRatio: 0.5,
                musicVolume: 0.06,
                videoVolume: 1,  // LipSync video audio volume
                captionColor: '#FFFF00',
                captionStyle: {},
                faceOffsetX: 0,
                faceOffsetY: 0,
                faceScale: 1,
              },
              LipSyncMain: {
                musicVolume: 0.06,
              },
              LipSyncBusiness: {
                musicVolume: 0.06,
              },
            };

            const defaults = templateDefaults[request.compositionId] || {};
            for (const [key, value] of Object.entries(defaults)) {
              if (inputProps[key] === undefined) {
                inputProps[key] = value;
              }
            }

            console.log(`🎬 Rendering ${request.compositionId} with props:`, Object.keys(inputProps));

            // Select composition
            const composition = await selectComposition({
              serveUrl: bundleLocation,
              id: request.compositionId,
              inputProps,
              chromiumOptions: {
                enableMultiProcessOnLinux: true,
                disableWebSecurity: true,
                gl: null,
                headless: true,
                args: [
                  '--no-sandbox',
                  '--disable-setuid-sandbox',
                  '--disable-gpu',
                  '--disable-software-rasterizer',
                  '--disable-dev-shm-usage',
                ],
              },
              timeoutInMilliseconds: 300000, // 5 minutes for slow video loading
            });

            // Override duration if we detected it
            if (durationInFrames > 0) {
              (composition as any).durationInFrames = durationInFrames;
            }

            // Render video
            const outputPath = path.join(OUTPUT_DIR, `${renderId}.mp4`);

            await renderMedia({
              composition,
              serveUrl: bundleLocation,
              codec: "h264",
              outputLocation: outputPath,
              inputProps,
              concurrency: OPTIMAL_CONCURRENCY,
              audioCodec: 'mp3',
              chromiumOptions: {
                enableMultiProcessOnLinux: true,
                disableWebSecurity: true,
                gl: null,
                headless: true,
                args: [
                  '--no-sandbox',
                  '--disable-setuid-sandbox',
                  '--disable-gpu',
                ],
              },
              timeoutInMilliseconds: 300000, // 5 minutes for slow video loading
              onProgress: ({ progress }) => {
                job.progress = Math.round(progress * 100);
              },
            });

            // Upload to S3 if enabled (default: true)
            let publicUrl: string | undefined;
            if (request.uploadToS3 !== false) {
              const uploadResult = await uploadRenderedFile(
                outputPath,
                request.s3Prefix || "renders/"
              );
              if (uploadResult.success) {
                publicUrl = uploadResult.url;
              }
            }

            // Update job status
            job.status = 'completed';
            job.progress = 100;
            job.outputUrl = publicUrl || `/renders/${renderId}.mp4`;

            const renderTimeMs = Date.now() - startTime;
            console.log(`✅ [Render] ${renderId} completed in ${renderTimeMs}ms`);

            // Send webhook if configured
            if (request.webhookUrl) {
              sendWebhook(
                request.webhookUrl,
                {
                  renderId,
                  status: 'completed',
                  publicUrl,
                  renderTimeMs,
                  timestamp: new Date().toISOString(),
                },
                request.webhookSecret
              );
            }

            // Send Telegram notification with video
            if (publicUrl) {
              const renderTimeSec = Math.round(renderTimeMs / 1000);
              const userInfo = request.userInfo;
              const caption = userInfo
                ? `✅ <b>Рендер готов!</b>\n\n👤 ${userInfo.first_name || 'Unknown'} (@${userInfo.username || 'нет'})\n📹 ${userInfo.project_name || 'Untitled'}\n⏱ ${renderTimeSec}s`
                : `✅ <b>Рендер готов!</b>\n\n⏱ ${renderTimeSec}s`;
              await sendTelegramVideo(TELEGRAM_RENDERS_GROUP, publicUrl, caption);
            }

          } catch (error) {
            console.error(`❌ [Render] ${renderId} failed:`, error);
            job.status = 'failed';
            job.error = error instanceof Error ? error.message : "Unknown error";

            // Send failure webhook
            if (request.webhookUrl) {
              sendWebhook(
                request.webhookUrl,
                {
                  renderId,
                  status: 'failed',
                  error: job.error,
                  renderTimeMs: Date.now() - startTime,
                  timestamp: new Date().toISOString(),
                },
                request.webhookSecret
              );
            }
          }
        })();

      } catch (error) {
        res.writeHead(400, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ error: "Invalid JSON" }));
      }
    });
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
    console.log(`   POST /render/template - Universal template API (S3 + webhook)`);
    console.log(`   GET  /renders/:id  - Download rendered file`);
    console.log(`   POST /upload       - Upload asset to S3`);
    console.log(`   GET  /assets       - List S3 assets`);
    console.log(`   POST /transcribe   - Transcribe audio to captions (RU/EN)`);
    console.log(`🔌 WebSocket: ws://0.0.0.0:${PORT} (real-time sync)`);
    console.log(`📦 S3 Bucket: ${S3_BUCKET}`);
  });
}

main().catch(console.error);
