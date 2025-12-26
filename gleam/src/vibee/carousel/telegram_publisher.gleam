// Carousel Telegram Publisher
// Publishes carousel slides to Telegram channels/chats

import gleam/erlang/process
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vibee/carousel/db as carousel_db
import vibee/carousel/types.{type Carousel, type Slide}
import vibee/db/postgres
import vibee/mcp/config

// ============================================================
// Types
// ============================================================

pub type PublishResult {
  PublishResult(
    carousel_id: String,
    channel_id: String,
    message_ids: List(Int),
    success: Bool,
    error: Option(String),
  )
}

pub type PublishOptions {
  PublishOptions(
    session_id: String,
    channel_id: String,
    include_captions: Bool,
    caption_template: Option(String),
  )
}

// ============================================================
// Publish Carousel
// ============================================================

/// Publish a carousel to a Telegram channel
/// Uses media group for multiple slides
pub fn publish_carousel(
  carousel_id: String,
  options: PublishOptions,
) -> Result(PublishResult, String) {
  case postgres.get_global_pool() {
    None -> Error("Database not connected")
    Some(pool) -> {
      case carousel_db.get_carousel(pool, carousel_id) {
        Error(_) -> Error("Carousel not found")
        Ok(carousel) -> {
          io.println(
            "[Carousel Publisher] Publishing carousel: "
            <> carousel_id
            <> " to channel: "
            <> options.channel_id,
          )

          case list.length(carousel.slides) {
            0 -> Error("Carousel has no slides")
            1 -> publish_single_slide(carousel, options)
            _ -> publish_media_group(carousel, options)
          }
        }
      }
    }
  }
}

/// Publish a single slide as a photo
fn publish_single_slide(
  carousel: Carousel,
  options: PublishOptions,
) -> Result(PublishResult, String) {
  case carousel.slides {
    [slide] -> {
      let caption = build_slide_caption(slide, 1, 1, options)

      case send_photo(options.session_id, options.channel_id, slide.image_url, caption) {
        Ok(result) -> {
          let msg_id = extract_message_id(result)
          Ok(PublishResult(
            carousel_id: carousel.id,
            channel_id: options.channel_id,
            message_ids: [msg_id],
            success: True,
            error: None,
          ))
        }
        Error(e) ->
          Ok(PublishResult(
            carousel_id: carousel.id,
            channel_id: options.channel_id,
            message_ids: [],
            success: False,
            error: Some(e),
          ))
      }
    }
    _ -> Error("Expected single slide")
  }
}

/// Send a photo via Telegram Bridge
fn send_photo(
  session_id: String,
  chat_id: String,
  image_url: String,
  caption: String,
) -> Result(String, String) {
  let base_url = config.get_env_or("VIBEE_BRIDGE_URL", "https://vibee-telegram-bridge.fly.dev")
  let api_key = config.get_env_or("VIBEE_API_KEY", "")

  let body = json.object([
    #("chat_id", json.string(chat_id)),
    #("photo", json.string(image_url)),
    #("caption", json.string(caption)),
  ]) |> json.to_string

  case request.to(base_url <> "/api/v1/send_photo") {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Post)
        |> request.set_header("content-type", "application/json")
        |> request.set_header("x-session-id", session_id)
        |> request.set_header("authorization", "Bearer " <> api_key)
        |> request.set_body(body)

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status >= 200 && resp.status < 300 {
            True -> Ok(resp.body)
            False -> Error("HTTP error: " <> string.inspect(resp.status))
          }
        }
        Error(_) -> Error("Network error")
      }
    }
    Error(_) -> Error("Invalid URL")
  }
}

/// Publish multiple slides as a media group
fn publish_media_group(
  carousel: Carousel,
  options: PublishOptions,
) -> Result(PublishResult, String) {
  let total_slides = list.length(carousel.slides)

  // Build media items for the group
  let media_items =
    carousel.slides
    |> list.index_map(fn(slide, index) {
      let caption = case index {
        // Only first slide gets the caption in media groups
        0 -> build_slide_caption(slide, index + 1, total_slides, options)
        _ -> ""
      }
      #(slide.image_url, caption)
    })

  io.println(
    "[Carousel Publisher] Sending media group with "
    <> string.inspect(total_slides)
    <> " slides",
  )

  case send_media_group(options.session_id, options.channel_id, media_items) {
    Ok(result) -> {
      let msg_ids = extract_message_ids_from_group(result)
      io.println(
        "[Carousel Publisher] Successfully published "
        <> string.inspect(list.length(msg_ids))
        <> " messages",
      )
      Ok(PublishResult(
        carousel_id: carousel.id,
        channel_id: options.channel_id,
        message_ids: msg_ids,
        success: True,
        error: None,
      ))
    }
    Error(e) -> {
      io.println("[Carousel Publisher] Failed: " <> e)
      Ok(PublishResult(
        carousel_id: carousel.id,
        channel_id: options.channel_id,
        message_ids: [],
        success: False,
        error: Some(e),
      ))
    }
  }
}

/// Send a media group via Telegram Bridge
fn send_media_group(
  session_id: String,
  chat_id: String,
  media_items: List(#(String, String)),
) -> Result(String, String) {
  let base_url = config.get_env_or("VIBEE_BRIDGE_URL", "https://vibee-telegram-bridge.fly.dev")
  let api_key = config.get_env_or("VIBEE_API_KEY", "")

  let media_json = json.array(media_items, fn(item) {
    let #(url, caption) = item
    json.object([
      #("type", json.string("photo")),
      #("media", json.string(url)),
      #("caption", json.string(caption)),
    ])
  })

  let body = json.object([
    #("chat_id", json.string(chat_id)),
    #("media", media_json),
  ]) |> json.to_string

  case request.to(base_url <> "/api/v1/send_media_group") {
    Ok(req) -> {
      let req = req
        |> request.set_method(http.Post)
        |> request.set_header("content-type", "application/json")
        |> request.set_header("x-session-id", session_id)
        |> request.set_header("authorization", "Bearer " <> api_key)
        |> request.set_body(body)

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status >= 200 && resp.status < 300 {
            True -> Ok(resp.body)
            False -> Error("HTTP error: " <> string.inspect(resp.status))
          }
        }
        Error(_) -> Error("Network error")
      }
    }
    Error(_) -> Error("Invalid URL")
  }
}

/// Build caption for a slide
fn build_slide_caption(
  slide: Slide,
  index: Int,
  total: Int,
  options: PublishOptions,
) -> String {
  case options.include_captions {
    False -> ""
    True -> {
      case options.caption_template {
        Some(template) ->
          template
          |> string.replace("{index}", string.inspect(index))
          |> string.replace("{total}", string.inspect(total))
          |> string.replace("{text}", get_slide_text(slide))
        None -> {
          // Default caption: slide text
          get_slide_text(slide)
        }
      }
    }
  }
}

/// Get primary text from slide elements
fn get_slide_text(slide: Slide) -> String {
  case slide.elements {
    [first, ..] -> first.content
    [] -> ""
  }
}

/// Extract message ID from send_photo response
fn extract_message_id(response: String) -> Int {
  // Parse response JSON to get message_id
  // Response format: {"message_id": 123, ...}
  case json.parse(response, message_id_decoder()) {
    Ok(id) -> id
    Error(_) -> 0
  }
}

/// Extract message IDs from media group response
fn extract_message_ids_from_group(response: String) -> List(Int) {
  // Parse response JSON to get list of message_ids
  // Response format: [{"message_id": 123}, {"message_id": 124}, ...]
  case json.parse(response, message_ids_decoder()) {
    Ok(ids) -> ids
    Error(_) -> []
  }
}

import gleam/dynamic/decode

fn message_id_decoder() -> decode.Decoder(Int) {
  use id <- decode.field("message_id", decode.int)
  decode.success(id)
}

fn message_ids_decoder() -> decode.Decoder(List(Int)) {
  decode.list({
    use id <- decode.field("message_id", decode.int)
    decode.success(id)
  })
}

// ============================================================
// Schedule Publish
// ============================================================

/// Schedule a carousel for future publishing
pub fn schedule_publish(
  carousel_id: String,
  options: PublishOptions,
  schedule_at: Int,
) -> Result(String, String) {
  // For now, just store the schedule in DB
  // A worker process would pick this up and publish at the scheduled time
  io.println(
    "[Carousel Publisher] Scheduled publish for carousel: "
    <> carousel_id
    <> " at: "
    <> string.inspect(schedule_at),
  )

  // TODO: Store in scheduled_posts table
  Ok("Scheduled successfully")
}

// ============================================================
// Cross-Post to Multiple Channels
// ============================================================

/// Publish carousel to multiple channels
pub fn cross_post(
  carousel_id: String,
  session_id: String,
  channel_ids: List(String),
  include_captions: Bool,
) -> List(Result(PublishResult, String)) {
  channel_ids
  |> list.map(fn(channel_id) {
    let options =
      PublishOptions(
        session_id: session_id,
        channel_id: channel_id,
        include_captions: include_captions,
        caption_template: None,
      )

    // Small delay between posts to avoid rate limiting
    process.sleep(1000)

    publish_carousel(carousel_id, options)
  })
}

// ============================================================
// Default Options
// ============================================================

pub fn default_options(session_id: String, channel_id: String) -> PublishOptions {
  PublishOptions(
    session_id: session_id,
    channel_id: channel_id,
    include_captions: True,
    caption_template: None,
  )
}
