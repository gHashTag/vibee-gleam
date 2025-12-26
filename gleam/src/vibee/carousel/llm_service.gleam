// Carousel LLM Service
// Uses Gemini 3 Pro via OpenRouter for text parsing and viral rewriting
// Includes vision capabilities for combo mode (photo analysis)

import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/ai/client as ai_client
import vibee/ai/openrouter
import vibee/carousel/types.{
  type ComboSlide, type Language, type ParsedSlide, type PhotoAnalysis,
  type SlideSource, type SlideType, type UploadedPhoto,
}
import vibee/mcp/config

// ============================================================
// Constants
// ============================================================

const model = "google/gemini-2.0-flash-001"

const vision_model = "google/gemini-2.0-flash-001"

// ============================================================
// Parse Topic to Slides
// ============================================================

/// Parse a topic/text into carousel slides
pub fn parse_topic_to_slides(
  topic: String,
  language: Language,
) -> Result(List(ParsedSlide), String) {
  let api_key = config.get_env_or("OPENROUTER_API_KEY", "")
  case api_key {
    "" -> Error("OPENROUTER_API_KEY not set")
    key -> {
      let or_config = openrouter.default_config(key)
      let system_prompt = build_carousel_system_prompt(language)

      let request =
        openrouter.ChatRequest(
          model: model,
          messages: [
            openrouter.system_message(system_prompt),
            openrouter.user_message("Topic: " <> topic),
          ],
          temperature: Some(0.7),
          max_tokens: Some(4000),
          top_p: None,
          frequency_penalty: None,
          presence_penalty: None,
          stop: None,
        )

      let or_request = openrouter.create_chat_request(or_config, request)
      let http_request = convert_request(or_request)

      io.println(
        "[Carousel LLM] Parsing topic: " <> string.slice(topic, 0, 50) <> "...",
      )

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> parse_slides_from_response(response_body)
        Error(e) -> Error("LLM request failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

/// Convert openrouter.Request to ai_client.Request
fn convert_request(req: openrouter.Request) -> ai_client.Request {
  ai_client.Request(
    url: req.url,
    method: req.method,
    headers: req.headers,
    body: req.body,
  )
}

/// Build system prompt for carousel generation
fn build_carousel_system_prompt(language: Language) -> String {
  let lang_name = case language {
    types.En -> "ENGLISH"
    types.Ru -> "RUSSIAN"
  }

  "You are an expert viral content creator for Instagram and LinkedIn carousels.

OUTPUT LANGUAGE: "
  <> lang_name
  <> " (all slide text MUST be in "
  <> lang_name
  <> ")

TASK: Convert the provided topic into a high-engagement carousel structure.

GUIDELINES:
1. Create 5-10 slides following this structure:
   - Slide 1 (HOOK): Short, punchy headline that triggers curiosity or fear. Under 10 words.
   - Middle Slides (BODY): ONE distinct thought per slide. Clear, actionable, memorable.
   - Last Slide (CTA): Strong call-to-action. Encourage saves, shares, follows.

2. TEXT FORMATTING:
   - Wrap 1-3 key impactful words per slide in <b> tags for highlighting
   - Keep text concise - each slide should be readable in 2-3 seconds
   - Use power words: \"секрет\", \"ошибка\", \"лучший\", \"бесплатно\", \"сейчас\"

3. IMAGE PROMPTS:
   - Provide an English image prompt for each slide background
   - Be specific and visual: describe scene, lighting, mood, composition
   - Avoid text in images - backgrounds should complement text, not duplicate it
   - Style hints: \"cinematic\", \"dramatic lighting\", \"professional photography\"

OUTPUT FORMAT (strict JSON array):
[
  {\"text\": \"<b>Вы делаете</b> эту ошибку каждый день\", \"imagePrompt\": \"Dark mysterious office scene with dramatic spotlight, silhouette of person at desk, cinematic noir style\"},
  {\"text\": \"Секрет успешных людей - <b>они встают в 5 утра</b>\", \"imagePrompt\": \"Golden sunrise over city skyline, warm light streaming through window, motivational atmosphere\"},
  ...
]

IMPORTANT:
- Output ONLY the JSON array, no markdown code blocks
- All text in "
  <> lang_name
  <> "
- All imagePrompt in English
- 5-10 slides total"
}

/// Parse LLM response into slides
fn parse_slides_from_response(
  response_body: String,
) -> Result(List(ParsedSlide), String) {
  // Extract content from OpenRouter response
  // Structure: { choices: [{ message: { content: "..." } }] }
  case extract_content_from_response(response_body) {
    Ok(content) -> {
      io.println("[Carousel LLM] Got response, parsing JSON...")
      // Clean up response - remove markdown code blocks if present
      let cleaned =
        content
        |> string.replace("```json", "")
        |> string.replace("```", "")
        |> string.trim

      case json.parse(cleaned, slides_decoder()) {
        Ok(slides) -> {
          io.println(
            "[Carousel LLM] Parsed "
            <> string.inspect(list.length(slides))
            <> " slides",
          )
          Ok(slides)
        }
        Error(e) -> {
          io.println("[Carousel LLM] JSON parse error: " <> string.inspect(e))
          Error("Failed to parse slides JSON: " <> string.inspect(e))
        }
      }
    }
    Error(e) -> Error(e)
  }
}

fn extract_content_from_response(response_body: String) -> Result(String, String) {
  // Decoder for choices[0].message.content
  let content_decoder = {
    use choices <- decode.field(
      "choices",
      decode.list({
        use message <- decode.field("message", {
          use content <- decode.field("content", decode.string)
          decode.success(content)
        })
        decode.success(message)
      }),
    )
    decode.success(choices)
  }

  case json.parse(response_body, content_decoder) {
    Ok([content, ..]) -> Ok(content)
    Ok([]) -> Error("Empty choices array")
    Error(_) -> Error("Failed to decode response structure")
  }
}

fn slides_decoder() -> decode.Decoder(List(ParsedSlide)) {
  decode.list({
    use text <- decode.field("text", decode.string)
    use image_prompt <- decode.field("imagePrompt", decode.string)
    decode.success(types.ParsedSlide(text: text, image_prompt: image_prompt))
  })
}

// ============================================================
// Make Text Viral
// ============================================================

/// Rewrite text to be more viral/engaging
pub fn make_text_viral(
  text: String,
  slide_type: SlideType,
  language: Language,
) -> Result(String, String) {
  let api_key = config.get_env_or("OPENROUTER_API_KEY", "")
  case api_key {
    "" -> Error("OPENROUTER_API_KEY not set")
    key -> {
      let or_config = openrouter.default_config(key)
      let instruction = get_viral_instruction(slide_type, language)

      let request =
        openrouter.ChatRequest(
          model: model,
          messages: [
            openrouter.system_message(viral_system_prompt(language)),
            openrouter.user_message(
              "Original: \"" <> text <> "\"\n\n" <> instruction,
            ),
          ],
          temperature: Some(0.8),
          max_tokens: Some(500),
          top_p: None,
          frequency_penalty: None,
          presence_penalty: None,
          stop: None,
        )

      let or_request = openrouter.create_chat_request(or_config, request)
      let http_request = convert_request(or_request)

      io.println(
        "[Carousel LLM] Making text viral: "
        <> string.slice(text, 0, 30)
        <> "...",
      )

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> extract_content_from_response(response_body)
        Error(e) -> Error("LLM request failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

fn viral_system_prompt(language: Language) -> String {
  let lang = case language {
    types.En -> "English"
    types.Ru -> "Russian"
  }

  "You are a viral content expert. Rewrite text to maximize engagement on Instagram and LinkedIn.

RULES:
1. Output in "
  <> lang
  <> "
2. Wrap 1-2 key trigger words in <b> tags
3. Keep the core message but make it punchy
4. Use power words and emotional triggers
5. Output ONLY the rewritten text, nothing else"
}

fn get_viral_instruction(slide_type: SlideType, language: Language) -> String {
  let lang = case language {
    types.En -> "English"
    types.Ru -> "Russian"
  }

  case slide_type {
    types.Hook ->
      "Rewrite as a viral HOOK headline in "
      <> lang
      <> ".
       Use fear, curiosity, or FOMO triggers.
       Under 10 words.
       Wrap the main trigger word in <b> tags.
       Examples: '<b>Ошибка</b> которую делают 90% людей', 'Почему <b>никто</b> об этом не говорит'"

    types.Body ->
      "Rewrite to be clearer and punchier in "
      <> lang
      <> ".
       ONE main thought.
       Make it actionable and memorable.
       Wrap the key concept in <b> tags.
       Keep it scannable in 2-3 seconds."

    types.CTA ->
      "Rewrite as a powerful CALL TO ACTION in "
      <> lang
      <> ".
       Encourage saves, shares, comments, follows.
       Create urgency.
       Wrap action word in <b> tags.
       Examples: '<b>Сохрани</b> чтобы не потерять', 'Поделись с тем кто <b>должен</b> это увидеть'"
  }
}

// ============================================================
// Generate Image Prompt
// ============================================================

/// Generate or improve an image prompt for a slide
pub fn generate_image_prompt(
  slide_text: String,
  art_style: String,
) -> Result(String, String) {
  let api_key = config.get_env_or("OPENROUTER_API_KEY", "")
  case api_key {
    "" -> Error("OPENROUTER_API_KEY not set")
    key -> {
      let or_config = openrouter.default_config(key)

      let request =
        openrouter.ChatRequest(
          model: model,
          messages: [
            openrouter.system_message(image_prompt_system()),
            openrouter.user_message(
              "Slide text: \""
              <> slide_text
              <> "\"\n"
              <> "Art style: "
              <> art_style
              <> "\n\n"
              <> "Generate an image prompt for this slide background.",
            ),
          ],
          temperature: Some(0.7),
          max_tokens: Some(300),
          top_p: None,
          frequency_penalty: None,
          presence_penalty: None,
          stop: None,
        )

      let or_request = openrouter.create_chat_request(or_config, request)
      let http_request = convert_request(or_request)

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> extract_content_from_response(response_body)
        Error(e) -> Error("LLM request failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

fn image_prompt_system() -> String {
  "You are an expert at creating image prompts for AI image generation (FLUX, Midjourney, DALL-E).

TASK: Generate an image prompt for a carousel slide background.

RULES:
1. Output in ENGLISH only
2. Be specific: describe scene, lighting, mood, composition
3. Include style keywords matching the requested art style
4. NO text should appear in the image
5. Image should complement the slide text, not duplicate it
6. Keep it under 100 words
7. Output ONLY the prompt, nothing else

STYLE KEYWORDS:
- PHOTOREALISM: ultra-realistic, 8k photography, professional lighting, sharp focus
- CYBERPUNK: neon lights, futuristic, rain, dark atmosphere, holographic
- DARK_NOIR: high contrast, dramatic shadows, cinematic, moody
- 3D_ABSTRACT: 3D render, abstract shapes, gradient colors, depth
- MINIMALIST: clean, simple, negative space, geometric, calm"
}

// ============================================================
// Suggest Improvements
// ============================================================

/// Analyze carousel and suggest improvements
pub fn suggest_improvements(
  slides: List(ParsedSlide),
  language: Language,
) -> Result(String, String) {
  let api_key = config.get_env_or("OPENROUTER_API_KEY", "")
  case api_key {
    "" -> Error("OPENROUTER_API_KEY not set")
    key -> {
      let or_config = openrouter.default_config(key)

      let slides_text =
        slides
        |> list.index_map(fn(slide, i) {
          "Slide " <> string.inspect(i + 1) <> ": " <> slide.text
        })
        |> string.join("\n")

      let lang = case language {
        types.En -> "English"
        types.Ru -> "Russian"
      }

      let request =
        openrouter.ChatRequest(
          model: model,
          messages: [
            openrouter.system_message(
              "You are a viral content expert. Analyze this carousel and suggest improvements. Output in "
              <> lang
              <> ".",
            ),
            openrouter.user_message(
              "Carousel slides:\n"
              <> slides_text
              <> "\n\n"
              <> "Provide 3-5 specific suggestions to make this carousel more viral.",
            ),
          ],
          temperature: Some(0.7),
          max_tokens: Some(1000),
          top_p: None,
          frequency_penalty: None,
          presence_penalty: None,
          stop: None,
        )

      let or_request = openrouter.create_chat_request(or_config, request)
      let http_request = convert_request(or_request)

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> extract_content_from_response(response_body)
        Error(e) -> Error("LLM request failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

// ============================================================
// Vision Analysis (Combo Mode)
// ============================================================

/// Analyze a single photo using vision LLM
pub fn analyze_photo(photo_url: String) -> Result(PhotoAnalysis, String) {
  let api_key = config.get_env_or("OPENROUTER_API_KEY", "")
  case api_key {
    "" -> Error("OPENROUTER_API_KEY not set")
    key -> {
      let or_config = openrouter.default_config(key)

      let vision_req = openrouter.analyze_image_url(photo_url, photo_analysis_prompt())
      let or_request = openrouter.create_vision_request(or_config, vision_req)
      let http_request = convert_request(or_request)

      io.println("[Carousel LLM] Analyzing photo: " <> string.slice(photo_url, 0, 50) <> "...")

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> {
          case extract_content_from_response(response_body) {
            Ok(content) -> parse_photo_analysis(content)
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error("Vision analysis failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

fn photo_analysis_prompt() -> String {
  "Analyze this image and provide a JSON response with the following structure:
{
  \"description\": \"Brief description of what's in the image\",
  \"mood\": \"Emotional tone/atmosphere (e.g., energetic, calm, professional)\",
  \"style\": \"Visual style (e.g., minimalist, vibrant, corporate)\",
  \"colors\": \"Dominant colors in the image\",
  \"suggested_text\": \"A short caption that would work well with this image\"
}

Output ONLY the JSON, no markdown code blocks."
}

fn parse_photo_analysis(content: String) -> Result(PhotoAnalysis, String) {
  let cleaned = content
    |> string.replace("```json", "")
    |> string.replace("```", "")
    |> string.trim

  let decoder = {
    use description <- decode.field("description", decode.string)
    use mood <- decode.field("mood", decode.string)
    use style <- decode.field("style", decode.string)
    use colors <- decode.field("colors", decode.string)
    use suggested_text <- decode.field("suggested_text", decode.string)
    decode.success(types.PhotoAnalysis(
      description: description,
      mood: mood,
      style: style,
      colors: colors,
      suggested_text: suggested_text,
    ))
  }

  case json.parse(cleaned, decoder) {
    Ok(analysis) -> Ok(analysis)
    Error(e) -> Error("Failed to parse photo analysis: " <> string.inspect(e))
  }
}

/// Analyze photos and generate combo storyline
pub fn analyze_photos_and_generate_storyline(
  photos: List(UploadedPhoto),
  topic: String,
  total_slides: Int,
  language: Language,
) -> Result(List(ComboSlide), String) {
  let api_key = config.get_env_or("OPENROUTER_API_KEY", "")
  case api_key {
    "" -> Error("OPENROUTER_API_KEY not set")
    key -> {
      let or_config = openrouter.default_config(key)

      // Build photo URLs list
      let photo_urls = list.map(photos, fn(p) { p.photo_url })

      // Create vision request with all photos
      let prompt = build_combo_storyline_prompt(
        list.length(photos),
        topic,
        total_slides,
        language,
      )

      let vision_req = openrouter.analyze_multiple_images(photo_urls, prompt)
      let or_request = openrouter.create_vision_request(or_config, vision_req)
      let http_request = convert_request(or_request)

      io.println(
        "[Carousel LLM] Generating combo storyline for "
        <> string.inspect(list.length(photos))
        <> " photos, "
        <> string.inspect(total_slides)
        <> " slides...",
      )

      case ai_client.execute_json(http_request) {
        Ok(response_body) -> {
          case extract_content_from_response(response_body) {
            Ok(content) -> parse_combo_slides(content, photos)
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error("Combo storyline generation failed: " <> ai_client.error_to_string(e))
      }
    }
  }
}

fn build_combo_storyline_prompt(
  num_photos: Int,
  topic: String,
  total_slides: Int,
  language: Language,
) -> String {
  let lang_name = case language {
    types.En -> "ENGLISH"
    types.Ru -> "RUSSIAN"
  }

  "You are analyzing "
  <> string.inspect(num_photos)
  <> " user-uploaded photos to create a coherent Instagram carousel storyline.

TOPIC: "
  <> topic
  <> "
TOTAL SLIDES NEEDED: "
  <> string.inspect(total_slides)
  <> "
OUTPUT LANGUAGE: "
  <> lang_name
  <> "

YOUR TASK:
1. Analyze each uploaded photo - describe what you see
2. Create a narrative arc across ALL "
  <> string.inspect(total_slides)
  <> " slides
3. For slides with photos: write text that complements the photo
4. For slides needing AI generation: write image prompts matching the photo style

OUTPUT FORMAT (strict JSON array):
[
  {\"position\": 0, \"text\": \"...\", \"source\": \"uploaded\", \"photo_index\": 0},
  {\"position\": 1, \"text\": \"...\", \"source\": \"generate\", \"image_prompt\": \"...\"},
  ...
]

RULES:
- Slide 0 (HOOK) should grab attention - use the most striking photo or generate one
- Last slide (CTA) should drive action
- Maintain visual consistency with uploaded photos' style
- All \"text\" in "
  <> lang_name
  <> "
- All \"image_prompt\" in English
- If source is \"uploaded\", include \"photo_index\" (0-based index of the photo)
- If source is \"generate\", include \"image_prompt\"
- Total slides MUST be exactly "
  <> string.inspect(total_slides)
  <> "
- Use <b> tags to highlight 1-2 key words per slide

Output ONLY the JSON array, no markdown."
}

fn parse_combo_slides(
  content: String,
  photos: List(UploadedPhoto),
) -> Result(List(ComboSlide), String) {
  let cleaned = content
    |> string.replace("```json", "")
    |> string.replace("```", "")
    |> string.trim

  // Parse raw JSON first
  let raw_decoder = decode.list({
    use position <- decode.field("position", decode.int)
    use text <- decode.field("text", decode.string)
    use source <- decode.field("source", decode.string)
    use photo_index <- decode.optional_field("photo_index", -1, decode.int)
    use image_prompt <- decode.optional_field("image_prompt", "", decode.string)
    decode.success(#(position, text, source, photo_index, image_prompt))
  })

  case json.parse(cleaned, raw_decoder) {
    Ok(raw_slides) -> {
      let combo_slides = list.map(raw_slides, fn(raw) {
        let #(position, text, source, photo_index, image_prompt) = raw

        case source {
          "uploaded" -> {
            // Get photo URL from index
            let photo_url = case get_at(photos, photo_index) {
              Some(photo) -> Some(photo.photo_url)
              None -> None
            }
            types.ComboSlide(
              position: position,
              text: text,
              source: types.UserUploaded,
              photo_url: photo_url,
              image_prompt: None,
            )
          }
          _ -> {
            // "generate" - AI will create this image
            types.ComboSlide(
              position: position,
              text: text,
              source: types.LoraGenerated,
              photo_url: None,
              image_prompt: Some(image_prompt),
            )
          }
        }
      })
      Ok(combo_slides)
    }
    Error(e) -> Error("Failed to parse combo slides: " <> string.inspect(e))
  }
}

/// Helper to get element at index
fn get_at(items: List(a), index: Int) -> Option(a) {
  case index < 0 {
    True -> None
    False -> {
      case list.drop(items, index) {
        [item, ..] -> Some(item)
        [] -> None
      }
    }
  }
}
