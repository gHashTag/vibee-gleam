// MCP AI Tools - AI service integrations for agents
// Provides MCP tools for ElevenLabs, Hedra, BFL, Kling, HeyGen

import gleam/json
import gleam/list
import vibee/mcp/types.{type Tool, Tool}

// ============================================================
// ElevenLabs TTS Tools
// ============================================================

/// Tool for text-to-speech conversion using ElevenLabs
pub fn elevenlabs_tts_tool() -> Tool {
  Tool(
    name: "ai_elevenlabs_tts",
    description: "Convert text to speech using ElevenLabs. Returns audio URL or request details for execution.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "text",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Text to convert to speech")),
            ]),
          ),
          #(
            "voice_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Voice ID to use (optional, default: Sarah)")),
            ]),
          ),
          #(
            "model",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Model to use (optional, default: eleven_multilingual_v2)")),
              #("enum", json.array(["eleven_multilingual_v2", "eleven_turbo_v2", "eleven_monolingual_v1"], json.string)),
            ]),
          ),
          #(
            "stability",
            json.object([
              #("type", json.string("number")),
              #("description", json.string("Voice stability (0.0-1.0, optional)")),
            ]),
          ),
          #(
            "similarity_boost",
            json.object([
              #("type", json.string("number")),
              #("description", json.string("Similarity boost (0.0-1.0, optional)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["text"], json.string)),
    ]),
  )
}

/// Tool to list available ElevenLabs voices
pub fn elevenlabs_list_voices_tool() -> Tool {
  Tool(
    name: "ai_elevenlabs_list_voices",
    description: "List all available voices from ElevenLabs",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Tool to clone a voice from audio using ElevenLabs Instant Voice Cloning
pub fn elevenlabs_clone_voice_tool() -> Tool {
  Tool(
    name: "ai_elevenlabs_clone_voice",
    description: "Clone a voice from audio using ElevenLabs Instant Voice Cloning. Downloads audio from URL and creates a new cloned voice.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Name for the cloned voice")),
            ]),
          ),
          #(
            "audio_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the audio file to clone voice from (mp3, wav, ogg)")),
            ]),
          ),
          #(
            "description",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Description for the cloned voice (optional)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["name", "audio_url"], json.string)),
    ]),
  )
}

/// Tool to delete a cloned voice from ElevenLabs
pub fn elevenlabs_delete_voice_tool() -> Tool {
  Tool(
    name: "ai_elevenlabs_delete_voice",
    description: "Delete a cloned voice from ElevenLabs by voice_id",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "voice_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Voice ID to delete")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["voice_id"], json.string)),
    ]),
  )
}

// ============================================================
// Hedra Avatar Tools
// ============================================================

/// Tool to create a talking avatar video with Hedra
pub fn hedra_create_avatar_tool() -> Tool {
  Tool(
    name: "ai_hedra_create_avatar",
    description: "Create a talking avatar video using Hedra. Combines audio and image to generate a video.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "audio_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the audio file (mp3, wav)")),
            ]),
          ),
          #(
            "image_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the image file (jpg, png)")),
            ]),
          ),
          #(
            "aspect_ratio",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Video aspect ratio (optional)")),
              #("enum", json.array(["1:1", "16:9", "9:16", "4:3", "3:4"], json.string)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["audio_url", "image_url"], json.string)),
    ]),
  )
}

/// Tool to check Hedra job status
pub fn hedra_get_status_tool() -> Tool {
  Tool(
    name: "ai_hedra_get_status",
    description: "Check the status of a Hedra avatar generation job",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "job_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("The job ID returned from create_avatar")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["job_id"], json.string)),
    ]),
  )
}

/// Tool to list all Hedra jobs
pub fn hedra_list_jobs_tool() -> Tool {
  Tool(
    name: "ai_hedra_list_jobs",
    description: "List all Hedra avatar generation jobs",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
      #("required", json.array([], json.string)),
    ]),
  )
}

// ============================================================
// BFL Image Generation Tools
// ============================================================

/// Tool to generate images using BFL FLUX models
pub fn bfl_generate_image_tool() -> Tool {
  Tool(
    name: "ai_bfl_generate_image",
    description: "Generate an image using BFL FLUX models (flux-pro-1.1, flux-dev, etc.)",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Text prompt describing the image to generate")),
            ]),
          ),
          #(
            "model",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("FLUX model to use")),
              #("enum", json.array(["flux-pro-1.1", "flux-pro", "flux-dev", "flux-pro-1.1-ultra"], json.string)),
              #("default", json.string("flux-pro-1.1")),
            ]),
          ),
          #(
            "width",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Image width in pixels (optional)")),
            ]),
          ),
          #(
            "height",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Image height in pixels (optional)")),
            ]),
          ),
          #(
            "steps",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Number of inference steps (optional)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["prompt"], json.string)),
    ]),
  )
}

/// Tool to get BFL task result
pub fn bfl_get_result_tool() -> Tool {
  Tool(
    name: "ai_bfl_get_result",
    description: "Get the result of a BFL image generation task",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("The task ID returned from generate_image")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id"], json.string)),
    ]),
  )
}

// ============================================================
// Kling AI Video Tools
// ============================================================

/// Tool to generate video from text using Kling AI
pub fn kling_create_video_tool() -> Tool {
  Tool(
    name: "ai_kling_create_video",
    description: "Generate a video from text prompt using Kling AI",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Text prompt describing the video to generate")),
            ]),
          ),
          #(
            "mode",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Generation mode: std (faster) or pro (higher quality)")),
              #("enum", json.array(["std", "pro"], json.string)),
              #("default", json.string("std")),
            ]),
          ),
          #(
            "duration",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Video duration in seconds")),
              #("enum", json.array(["5", "10"], json.string)),
              #("default", json.string("5")),
            ]),
          ),
          #(
            "aspect_ratio",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Video aspect ratio (optional)")),
              #("enum", json.array(["16:9", "9:16", "1:1"], json.string)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["prompt"], json.string)),
    ]),
  )
}

/// Tool to generate video from image using Kling AI
pub fn kling_image_to_video_tool() -> Tool {
  Tool(
    name: "ai_kling_image_to_video",
    description: "Animate an image into a video using Kling AI",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "image_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the image to animate")),
            ]),
          ),
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Optional prompt to guide the animation")),
            ]),
          ),
          #(
            "duration",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Video duration in seconds")),
              #("enum", json.array(["5", "10"], json.string)),
              #("default", json.string("5")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["image_url"], json.string)),
    ]),
  )
}

/// Tool to get Kling task status
pub fn kling_get_task_tool() -> Tool {
  Tool(
    name: "ai_kling_get_task",
    description: "Get the status and result of a Kling AI video task",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("The task ID returned from create_video or image_to_video")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id"], json.string)),
    ]),
  )
}

/// Tool to list all Kling tasks
pub fn kling_list_tasks_tool() -> Tool {
  Tool(
    name: "ai_kling_list_tasks",
    description: "List all Kling AI video generation tasks",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
      #("required", json.array([], json.string)),
    ]),
  )
}

// ============================================================
// HeyGen Avatar Tools
// ============================================================

/// Tool to create a video with HeyGen avatar
pub fn heygen_create_video_tool() -> Tool {
  Tool(
    name: "ai_heygen_create_video",
    description: "Create a video with an AI avatar speaking the provided script using HeyGen",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "avatar_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("HeyGen avatar ID to use")),
            ]),
          ),
          #(
            "script",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Text script for the avatar to speak")),
            ]),
          ),
          #(
            "voice_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Voice ID to use (optional, default: avatar's default voice)")),
            ]),
          ),
          #(
            "background_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of background image (optional)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["avatar_id", "script"], json.string)),
    ]),
  )
}

/// Tool to list available HeyGen avatars
pub fn heygen_list_avatars_tool() -> Tool {
  Tool(
    name: "ai_heygen_list_avatars",
    description: "List all available HeyGen avatars",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Tool to get HeyGen video status
pub fn heygen_get_video_status_tool() -> Tool {
  Tool(
    name: "ai_heygen_get_video_status",
    description: "Check the status of a HeyGen video generation",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("The video ID returned from create_video")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_id"], json.string)),
    ]),
  )
}

/// Tool to list available HeyGen voices
pub fn heygen_list_voices_tool() -> Tool {
  Tool(
    name: "ai_heygen_list_voices",
    description: "List all available HeyGen voices for avatars",
    input_schema: json.object([
      #("type", json.string("object")),
      #("properties", json.object([])),
      #("required", json.array([], json.string)),
    ]),
  )
}

// ============================================================
// KIE.ai (Veo3) Video Tools
// ============================================================

/// Tool to generate video using KIE.ai (Veo3)
pub fn kieai_create_video_tool() -> Tool {
  Tool(
    name: "ai_kieai_create_video",
    description: "Generate a video using KIE.ai (Veo3). Supports text-to-video with multiple models including veo3_fast, veo3, sora-2, wan-2.5-t2v, runway-aleph",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Text prompt describing the video to generate")),
            ]),
          ),
          #(
            "model",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Video model to use")),
              #("enum", json.array(["veo3_fast", "veo3", "sora-2", "wan-2.5-t2v", "runway-aleph"], json.string)),
              #("default", json.string("veo3_fast")),
            ]),
          ),
          #(
            "aspect_ratio",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Video aspect ratio")),
              #("enum", json.array(["9:16", "16:9", "1:1"], json.string)),
              #("default", json.string("9:16")),
            ]),
          ),
          #(
            "seeds",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Random seed for reproducibility (10000-99999)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["prompt"], json.string)),
    ]),
  )
}

/// Tool to check KIE.ai task status
pub fn kieai_get_status_tool() -> Tool {
  Tool(
    name: "ai_kieai_get_status",
    description: "Check the status of a KIE.ai video generation task. Returns status, progress, and video URLs when complete.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "task_id",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("The task ID returned from create_video")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["task_id"], json.string)),
    ]),
  )
}

/// Tool to list KIE.ai videos
pub fn kieai_list_videos_tool() -> Tool {
  Tool(
    name: "ai_kieai_list_videos",
    description: "List all KIE.ai video generation records with pagination",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "limit",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Number of records to return (default: 20)")),
            ]),
          ),
          #(
            "offset",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Offset for pagination (default: 0)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

// ============================================================
// FAL.ai Tools (NeuroPhoto, Nano Banana, Flux Kontext)
// ============================================================

/// Tool to generate NeuroPhoto with LoRA
pub fn fal_neuro_photo_tool() -> Tool {
  Tool(
    name: "ai_fal_neuro_photo",
    description: "Generate a NeuroPhoto image using FAL.ai FLUX with LoRA. Uses NEURO_SAGE trigger word for best results.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Text prompt describing the image (NEURO_SAGE trigger added automatically)")),
            ]),
          ),
          #(
            "lora_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL to the LoRA model weights")),
            ]),
          ),
          #(
            "num_images",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Number of images to generate (default: 1)")),
              #("default", json.int(1)),
            ]),
          ),
          #(
            "aspect_ratio",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Aspect ratio")),
              #("enum", json.array(["9:16", "16:9", "1:1", "4:3"], json.string)),
              #("default", json.string("9:16")),
            ]),
          ),
          #(
            "seed",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Random seed for reproducibility")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["prompt", "lora_url"], json.string)),
    ]),
  )
}

/// Tool to generate images with Nano Banana Pro
pub fn fal_nano_banana_tool() -> Tool {
  Tool(
    name: "ai_fal_nano_banana",
    description: "Generate images using Google Nano Banana Pro via FAL.ai. Fast and cost-effective.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Text prompt describing the image")),
            ]),
          ),
          #(
            "num_images",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Number of images (default: 1)")),
              #("default", json.int(1)),
            ]),
          ),
          #(
            "aspect_ratio",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Aspect ratio")),
              #("enum", json.array(["1:1", "16:9", "9:16", "4:3", "3:4"], json.string)),
              #("default", json.string("9:16")),
            ]),
          ),
          #(
            "resolution",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Output resolution")),
              #("enum", json.array(["1K", "2K", "4K"], json.string)),
              #("default", json.string("1K")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["prompt"], json.string)),
    ]),
  )
}

/// Tool to generate images with Flux Kontext
pub fn fal_flux_kontext_tool() -> Tool {
  Tool(
    name: "ai_fal_flux_kontext",
    description: "Generate or transform images using FLUX Kontext (Pro or Max). Supports image-to-image and various modes.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Text prompt describing the desired transformation")),
            ]),
          ),
          #(
            "input_image_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the input image to transform")),
            ]),
          ),
          #(
            "model_type",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Model version to use")),
              #("enum", json.array(["pro", "max"], json.string)),
              #("default", json.string("pro")),
            ]),
          ),
          #(
            "aspect_ratio",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Output aspect ratio")),
              #("enum", json.array(["1:1", "16:9", "9:16", "4:3", "3:4", "21:9"], json.string)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["prompt", "input_image_url"], json.string)),
    ]),
  )
}

// ============================================================
// Replicate LipSync Tool (Kling LipSync)
// ============================================================

/// Tool to generate lip-sync video using Kling model via Replicate
pub fn replicate_lipsync_tool() -> Tool {
  Tool(
    name: "ai_replicate_lipsync",
    description: "Generate lip-sync video by synchronizing audio to a video. Uses Kling LipSync model via Replicate.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the video to process")),
            ]),
          ),
          #(
            "audio_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the audio to synchronize")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_url", "audio_url"], json.string)),
    ]),
  )
}

// ============================================================
// Replicate Morphing Tool (Kling Morphing)
// ============================================================

/// Tool to create morphing video between two images
pub fn replicate_morphing_tool() -> Tool {
  Tool(
    name: "ai_replicate_morphing",
    description: "Create smooth morphing video transition between two images. Uses Kling model via Replicate.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "start_image_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the starting image")),
            ]),
          ),
          #(
            "end_image_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the ending image")),
            ]),
          ),
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string(
                  "Optional prompt describing the transition style",
                ),
              ),
            ]),
          ),
          #(
            "duration",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Video duration in seconds")),
              #("enum", json.array(["5", "10"], json.string)),
              #("default", json.string("5")),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array(["start_image_url", "end_image_url"], json.string),
      ),
    ]),
  )
}

// ============================================================
// Replicate Face Swap Tool
// ============================================================

/// Tool to swap faces between two images
pub fn replicate_faceswap_tool() -> Tool {
  Tool(
    name: "ai_replicate_faceswap",
    description: "Swap face from one image to another. Uses codeplugtech/face-swap model via Replicate.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "target_image_url",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("URL of the target image (person whose face will be replaced)"),
              ),
            ]),
          ),
          #(
            "swap_image_url",
            json.object([
              #("type", json.string("string")),
              #(
                "description",
                json.string("URL of the source image (face to swap in)"),
              ),
            ]),
          ),
        ]),
      ),
      #(
        "required",
        json.array(["target_image_url", "swap_image_url"], json.string),
      ),
    ]),
  )
}

// ============================================================
// OpenAI Tools (Whisper, Vision, Improve Prompt)
// ============================================================

/// Tool to transcribe audio using OpenAI Whisper
pub fn openai_transcribe_tool() -> Tool {
  Tool(
    name: "ai_openai_transcribe",
    description: "Transcribe audio to text using OpenAI Whisper. Supports multiple languages and formats.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "audio_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the audio file to transcribe")),
            ]),
          ),
          #(
            "language",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Language code (optional, auto-detect if not specified)")),
              #("enum", json.array(["en", "ru", "es", "fr", "de", "ja", "zh", "ko"], json.string)),
            ]),
          ),
          #(
            "response_format",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Output format")),
              #("enum", json.array(["json", "text", "srt", "vtt", "verbose_json"], json.string)),
              #("default", json.string("json")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["audio_url"], json.string)),
    ]),
  )
}

/// Tool to describe an image using GPT-4 Vision
pub fn openai_vision_tool() -> Tool {
  Tool(
    name: "ai_openai_vision",
    description: "Analyze and describe an image using GPT-4 Vision. Can generate prompts for image recreation.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "image_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the image to analyze")),
            ]),
          ),
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Custom prompt for image analysis (optional)")),
            ]),
          ),
          #(
            "mode",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Analysis mode")),
              #("enum", json.array(["describe", "image_to_prompt"], json.string)),
              #("default", json.string("describe")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["image_url"], json.string)),
    ]),
  )
}

/// Tool to improve prompts using GPT
pub fn openai_improve_prompt_tool() -> Tool {
  Tool(
    name: "ai_openai_improve_prompt",
    description: "Improve and enhance a prompt for AI image/video generation. Makes prompts more detailed and effective.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Original prompt to improve")),
            ]),
          ),
          #(
            "style",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Target style for optimization")),
              #("enum", json.array(["image", "video", "text"], json.string)),
              #("default", json.string("image")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["prompt"], json.string)),
    ]),
  )
}

// ============================================================
// Replicate Upscaler Tool (Clarity Upscaler)
// ============================================================

/// Tool to upscale images using Clarity Upscaler via Replicate
pub fn replicate_upscale_tool() -> Tool {
  Tool(
    name: "ai_replicate_upscale",
    description: "Upscale and enhance images using Clarity Upscaler (philz1337x/clarity-upscaler). Increases resolution while preserving details.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "image_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL of the image to upscale")),
            ]),
          ),
          #(
            "scale",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Upscale factor")),
              #("enum", json.array([json.int(2), json.int(4)], fn(x) { x })),
              #("default", json.int(2)),
            ]),
          ),
          #(
            "creativity",
            json.object([
              #("type", json.string("number")),
              #("description", json.string("Creativity level 0.0-1.0 (higher = more creative details)")),
              #("default", json.float(0.35)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["image_url"], json.string)),
    ]),
  )
}

// ============================================================
// Replicate LoRA Training Tool
// ============================================================

/// Tool to train LoRA models using Replicate
pub fn replicate_train_lora_tool() -> Tool {
  Tool(
    name: "ai_replicate_train_lora",
    description: "Train a LoRA model using Replicate Training API. Upload images and train a custom FLUX LoRA.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "images_zip_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("URL to a ZIP archive containing training images")),
            ]),
          ),
          #(
            "trigger_word",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Trigger word to activate the LoRA style")),
            ]),
          ),
          #(
            "model_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Name for the trained model")),
            ]),
          ),
          #(
            "steps",
            json.object([
              #("type", json.string("integer")),
              #("description", json.string("Number of training steps (default: 1000)")),
              #("default", json.int(1000)),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["images_zip_url", "trigger_word", "model_name"], json.string)),
    ]),
  )
}

// ============================================================
// Video Editing Tools (FFmpeg)
// ============================================================

/// Tool to concatenate multiple videos
pub fn video_concat_tool() -> Tool {
  Tool(
    name: "video_concat",
    description: "Concatenate multiple video files into one. Returns path to output video.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_urls",
            json.object([
              #("type", json.string("array")),
              #("items", json.object([#("type", json.string("string"))])),
              #("description", json.string("Array of video file paths or URLs to concatenate")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_urls"], json.string)),
    ]),
  )
}

/// Tool to add audio track to video
pub fn video_add_audio_tool() -> Tool {
  Tool(
    name: "video_add_audio",
    description: "Add or replace audio track in a video. Can mix with existing audio or replace completely.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path or URL to the video file")),
            ]),
          ),
          #(
            "audio_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path or URL to the audio file")),
            ]),
          ),
          #(
            "volume",
            json.object([
              #("type", json.string("number")),
              #("description", json.string("Audio volume (0.0-2.0, default: 1.0)")),
            ]),
          ),
          #(
            "replace_audio",
            json.object([
              #("type", json.string("boolean")),
              #("description", json.string("Replace original audio completely (default: false, mix audio tracks)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_url", "audio_url"], json.string)),
    ]),
  )
}

/// Tool to add watermark to video
pub fn video_watermark_tool() -> Tool {
  Tool(
    name: "video_watermark",
    description: "Add a watermark image to a video. Supports position, opacity, and scale settings.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path or URL to the video file")),
            ]),
          ),
          #(
            "watermark_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path or URL to the watermark image (PNG recommended)")),
            ]),
          ),
          #(
            "position",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Watermark position")),
              #("enum", json.array(["topleft", "topright", "bottomleft", "bottomright", "center"], json.string)),
            ]),
          ),
          #(
            "opacity",
            json.object([
              #("type", json.string("number")),
              #("description", json.string("Watermark opacity (0.0-1.0, default: 0.7)")),
            ]),
          ),
          #(
            "scale",
            json.object([
              #("type", json.string("number")),
              #("description", json.string("Watermark scale relative to video (0.05-1.0, default: 0.15)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_url", "watermark_url"], json.string)),
    ]),
  )
}

/// Tool to trim video
pub fn video_trim_tool() -> Tool {
  Tool(
    name: "video_trim",
    description: "Trim a video to a specific duration, starting from a given time.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path or URL to the video file")),
            ]),
          ),
          #(
            "start_time",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Start time (seconds or HH:MM:SS format)")),
            ]),
          ),
          #(
            "duration",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Duration to keep (seconds or HH:MM:SS format)")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_url", "start_time", "duration"], json.string)),
    ]),
  )
}

/// Tool to get video information
pub fn video_info_tool() -> Tool {
  Tool(
    name: "video_info",
    description: "Get detailed information about a video file (duration, resolution, codec, etc.)",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path or URL to the video file")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_url"], json.string)),
    ]),
  )
}

/// Tool to extract audio from video
pub fn video_extract_audio_tool() -> Tool {
  Tool(
    name: "video_extract_audio",
    description: "Extract audio track from a video file. Returns MP3 file path.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "video_url",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Path or URL to the video file")),
            ]),
          ),
        ]),
      ),
      #("required", json.array(["video_url"], json.string)),
    ]),
  )
}

// ============================================================
// B-Roll Generation Tools
// ============================================================

/// Tool to generate B-Roll video from templates
pub fn broll_generate_tool() -> Tool {
  Tool(
    name: "broll_generate",
    description: "Generate B-Roll background video using AI. Choose from preset templates by category or provide custom prompt. Uses Kling or Veo3 for generation.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "category",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("B-Roll category: nature, city, abstract, business, technology, lifestyle, space, water, fire, smoke, particles, light")),
              #("enum", json.array([
                "nature", "city", "abstract", "business", "technology",
                "lifestyle", "space", "water", "fire", "smoke", "particles", "light"
              ], json.string)),
            ]),
          ),
          #(
            "template_name",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Specific template name (e.g., 'ocean_waves', 'particle_flow'). Use broll_list_templates to see available templates.")),
            ]),
          ),
          #(
            "custom_prompt",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Custom prompt to use instead of template. Overrides category/template if provided.")),
            ]),
          ),
          #(
            "model",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("AI model to use: 'kling' (default) or 'veo3'")),
              #("enum", json.array(["kling", "veo3"], json.string)),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

/// Tool to list available B-Roll templates
pub fn broll_list_templates_tool() -> Tool {
  Tool(
    name: "broll_list_templates",
    description: "List available B-Roll video templates. Can filter by category. Returns template names, prompts, styles, and whether they are loop-friendly.",
    input_schema: json.object([
      #("type", json.string("object")),
      #(
        "properties",
        json.object([
          #(
            "category",
            json.object([
              #("type", json.string("string")),
              #("description", json.string("Filter by category (optional). Leave empty to list all.")),
              #("enum", json.array([
                "nature", "city", "abstract", "business", "technology",
                "lifestyle", "space", "water", "fire", "smoke", "particles", "light"
              ], json.string)),
            ]),
          ),
        ]),
      ),
      #("required", json.array([], json.string)),
    ]),
  )
}

// ============================================================
// All AI Tools Registry
// ============================================================

/// Get all AI tools for registration in the MCP server
pub fn all_ai_tools() -> List(Tool) {
  [
    // ElevenLabs
    elevenlabs_tts_tool(),
    elevenlabs_list_voices_tool(),
    elevenlabs_clone_voice_tool(),
    elevenlabs_delete_voice_tool(),
    // Hedra
    hedra_create_avatar_tool(),
    hedra_get_status_tool(),
    hedra_list_jobs_tool(),
    // BFL
    bfl_generate_image_tool(),
    bfl_get_result_tool(),
    // Kling
    kling_create_video_tool(),
    kling_image_to_video_tool(),
    kling_get_task_tool(),
    kling_list_tasks_tool(),
    // HeyGen
    heygen_create_video_tool(),
    heygen_list_avatars_tool(),
    heygen_get_video_status_tool(),
    heygen_list_voices_tool(),
    // KIE.ai (Veo3)
    kieai_create_video_tool(),
    kieai_get_status_tool(),
    kieai_list_videos_tool(),
    // FAL.ai
    fal_neuro_photo_tool(),
    fal_nano_banana_tool(),
    fal_flux_kontext_tool(),
    // Replicate LipSync, Morphing & Face Swap
    replicate_lipsync_tool(),
    replicate_morphing_tool(),
    replicate_faceswap_tool(),
    // OpenAI (Whisper, Vision, Improve Prompt)
    openai_transcribe_tool(),
    openai_vision_tool(),
    openai_improve_prompt_tool(),
    // Replicate Upscaler & LoRA Training
    replicate_upscale_tool(),
    replicate_train_lora_tool(),
    // Video Editing (FFmpeg)
    video_concat_tool(),
    video_add_audio_tool(),
    video_watermark_tool(),
    video_trim_tool(),
    video_info_tool(),
    video_extract_audio_tool(),
    // B-Roll Generation
    broll_generate_tool(),
    broll_list_templates_tool(),
  ]
}
