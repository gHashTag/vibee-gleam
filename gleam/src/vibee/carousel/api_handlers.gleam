// Carousel API Handlers
// REST endpoints for carousel CRUD and AI generation

import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mist.{type ResponseData}
import vibee/carousel/bfl_service
import vibee/carousel/db as carousel_db
import vibee/carousel/fal_service
import vibee/carousel/llm_service
import vibee/carousel/types
import vibee/db/postgres

// ============================================================
// Create Carousel
// ============================================================

/// POST /api/carousel/create
/// Body: {"topic": "...", "aspect_ratio": "4:5", "language": "ru"}
pub fn create_handler(body: String) -> Response(ResponseData) {
  case parse_create_request(body) {
    Error(e) -> json_error(400, "Invalid request: " <> e)
    Ok(req) -> {
      case postgres.get_global_pool() {
        None -> json_error(500, "Database not connected")
        Some(pool) -> {
          io.println("[Carousel API] Creating carousel: " <> string.slice(req.topic, 0, 50))

          // Create carousel in DB
          case carousel_db.create_carousel(
            pool,
            req.user_id,
            req.topic,
            req.aspect_ratio,
            req.language,
            types.default_settings(),
          ) {
            Ok(id) -> {
              let response_body = json.object([
                #("status", json.string("ok")),
                #("carousel_id", json.string(id)),
                #("message", json.string("Carousel created. Use /generate to add slides.")),
              ])
              |> json.to_string

              response.new(201)
              |> response.set_header("content-type", "application/json")
              |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
            }
            Error(e) -> json_error(500, "Failed to create carousel: " <> db_error_to_string(e))
          }
        }
      }
    }
  }
}

type CreateRequest {
  CreateRequest(
    topic: String,
    aspect_ratio: types.AspectRatio,
    language: types.Language,
    user_id: Option(Int),
  )
}

fn parse_create_request(body: String) -> Result(CreateRequest, String) {
  let decoder = {
    use topic <- decode.field("topic", decode.string)
    use ratio_str <- decode.optional_field("aspect_ratio", "", decode.string)
    use lang_str <- decode.optional_field("language", "", decode.string)
    use user_id_val <- decode.optional_field("user_id", -1, decode.int)

    let aspect_ratio = case ratio_str {
      "1:1" -> types.Square
      "9:16" -> types.Story
      _ -> types.Portrait
    }

    let language = case lang_str {
      "en" -> types.En
      _ -> types.Ru
    }

    let user_id = case user_id_val {
      -1 -> None
      id -> Some(id)
    }

    decode.success(CreateRequest(
      topic: topic,
      aspect_ratio: aspect_ratio,
      language: language,
      user_id: user_id,
    ))
  }

  case json.parse(body, decoder) {
    Ok(req) -> Ok(req)
    Error(_) -> Error("Invalid JSON or missing 'topic' field")
  }
}

// ============================================================
// Generate Carousel (AI)
// ============================================================

/// POST /api/carousel/:id/generate
/// Parses topic into slides and generates images
pub fn generate_handler(carousel_id: String, body: String) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      io.println("[Carousel API] Generating slides for: " <> carousel_id)

      // Get carousel
      case carousel_db.get_carousel(pool, carousel_id) {
        Error(postgres.DbNotFound) -> json_error(404, "Carousel not found")
        Error(e) -> json_error(500, "Database error: " <> db_error_to_string(e))
        Ok(carousel) -> {
          // Parse topic into slides using LLM
          case llm_service.parse_topic_to_slides(carousel.topic, carousel.language) {
            Error(e) -> json_error(500, "LLM error: " <> e)
            Ok(parsed_slides) -> {
              io.println("[Carousel API] Got " <> string.inspect(list.length(parsed_slides)) <> " slides from LLM")

              // Create slides in DB and start image generation
              let slide_results = create_slides_with_images(
                pool,
                carousel_id,
                parsed_slides,
                carousel.settings.art_style,
                carousel.aspect_ratio,
              )

              let response_body = json.object([
                #("status", json.string("ok")),
                #("carousel_id", json.string(carousel_id)),
                #("slides_count", json.int(list.length(parsed_slides))),
                #("slides", json.array(slide_results, fn(r) {
                  case r {
                    Ok(slide) -> json.object([
                      #("id", json.string(slide.id)),
                      #("text", json.string(get_first_element_text(slide.elements))),
                      #("image_url", json.string(slide.image_url)),
                      #("status", json.string("ready")),
                    ])
                    Error(e) -> json.object([
                      #("status", json.string("error")),
                      #("error", json.string(e)),
                    ])
                  }
                })),
              ])
              |> json.to_string

              response.new(200)
              |> response.set_header("content-type", "application/json")
              |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
            }
          }
        }
      }
    }
  }
}

fn get_first_element_text(elements: List(types.SlideElement)) -> String {
  case elements {
    [first, ..] -> first.content
    [] -> ""
  }
}

fn create_slides_with_images(
  pool: postgres.DbPool,
  carousel_id: String,
  parsed_slides: List(types.ParsedSlide),
  art_style: String,
  aspect_ratio: types.AspectRatio,
) -> List(Result(types.Slide, String)) {
  list.index_map(parsed_slides, fn(parsed, index) {
    io.println("[Carousel API] Creating slide " <> string.inspect(index + 1))

    // Generate image
    case bfl_service.generate_and_wait(parsed.image_prompt, art_style, aspect_ratio) {
      Error(e) -> {
        io.println("[Carousel API] Image generation failed: " <> e)
        Error(e)
      }
      Ok(image_url) -> {
        // Create slide in DB
        case carousel_db.create_slide(
          pool,
          carousel_id,
          index,
          image_url,
          parsed.image_prompt,
          types.NoFrame,
        ) {
          Error(e) -> Error("DB error: " <> db_error_to_string(e))
          Ok(slide_id) -> {
            // Create text element
            let element = types.SlideElement(
              id: "",
              element_type: case index {
                0 -> types.Heading
                _ -> types.Text
              },
              content: parsed.text,
              position: types.default_position(),
              scale: 1.0,
              rotation: 0.0,
              z_index: 1,
              width: None,
              locked: False,
              style: None,
            )

            case carousel_db.create_element(pool, slide_id, element) {
              Error(_) -> Error("Failed to create element")
              Ok(element_id) -> {
                Ok(types.Slide(
                  id: slide_id,
                  image_url: image_url,
                  image_prompt: parsed.image_prompt,
                  elements: [types.SlideElement(..element, id: element_id)],
                  frame: types.NoFrame,
                ))
              }
            }
          }
        }
      }
    }
  })
}

// ============================================================
// Get Carousel
// ============================================================

/// GET /api/carousel/:id
pub fn get_handler(carousel_id: String) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      case carousel_db.get_carousel(pool, carousel_id) {
        Error(postgres.DbNotFound) -> json_error(404, "Carousel not found")
        Error(e) -> json_error(500, "Database error: " <> db_error_to_string(e))
        Ok(carousel) -> {
          let response_body = carousel_to_json(carousel) |> json.to_string

          response.new(200)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
        }
      }
    }
  }
}

fn carousel_to_json(c: types.Carousel) -> json.Json {
  json.object([
    #("id", json.string(c.id)),
    #("topic", json.string(c.topic)),
    #("aspect_ratio", json.string(aspect_ratio_to_string(c.aspect_ratio))),
    #("language", json.string(language_to_string(c.language))),
    #("created_at", json.string(c.created_at)),
    #("slides", json.array(c.slides, slide_to_json)),
    #("settings", settings_to_json(c.settings)),
  ])
}

fn slide_to_json(s: types.Slide) -> json.Json {
  json.object([
    #("id", json.string(s.id)),
    #("image_url", json.string(s.image_url)),
    #("image_prompt", json.string(s.image_prompt)),
    #("frame", json.string(frame_to_string(s.frame))),
    #("elements", json.array(s.elements, element_to_json)),
  ])
}

fn element_to_json(e: types.SlideElement) -> json.Json {
  json.object([
    #("id", json.string(e.id)),
    #("type", json.string(element_type_to_string(e.element_type))),
    #("content", json.string(e.content)),
    #("position", json.object([
      #("x", json.float(e.position.x)),
      #("y", json.float(e.position.y)),
    ])),
    #("scale", json.float(e.scale)),
    #("rotation", json.float(e.rotation)),
    #("z_index", json.int(e.z_index)),
    #("locked", json.bool(e.locked)),
  ])
}

fn settings_to_json(s: types.GlobalSettings) -> json.Json {
  json.object([
    #("font_family", json.string(s.font_family)),
    #("font_size", json.int(s.font_size)),
    #("text_color", json.string(s.text_color)),
    #("highlight_color", json.string(s.highlight_color)),
    #("overlay_mode", json.string(overlay_to_string(s.overlay_mode))),
    #("overlay_color", json.string(s.overlay_color)),
    #("overlay_opacity", json.int(s.overlay_opacity)),
    #("art_style", json.string(s.art_style)),
  ])
}

// ============================================================
// List Carousels
// ============================================================

/// GET /api/carousel/list
pub fn list_handler(user_id: Option(Int)) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      case carousel_db.list_carousels(pool, user_id, 50, 0) {
        Error(e) -> json_error(500, "Database error: " <> db_error_to_string(e))
        Ok(carousels) -> {
          let response_body = json.object([
            #("status", json.string("ok")),
            #("count", json.int(list.length(carousels))),
            #("carousels", json.array(carousels, fn(c) {
              json.object([
                #("id", json.string(c.id)),
                #("topic", json.string(c.topic)),
                #("slides_count", json.int(list.length(c.slides))),
                #("created_at", json.string(c.created_at)),
              ])
            })),
          ])
          |> json.to_string

          response.new(200)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
        }
      }
    }
  }
}

// ============================================================
// Update Element
// ============================================================

/// PUT /api/carousel/:id/element/:element_id
/// Body: {"content": "...", "position": {"x": 50, "y": 60}, "scale": 1.0, "rotation": 0}
pub fn update_element_handler(
  carousel_id: String,
  element_id: String,
  body: String,
) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      // Parse element update request
      case parse_element_update(body) {
        Error(_) -> json_error(400, "Invalid JSON")
        Ok(update) -> {
          // Update content if provided
          case update.content {
            Some(content) -> {
              case carousel_db.update_element_content(pool, element_id, content) {
                Error(e) -> {
                  io.println("[Carousel API] Update content error: " <> db_error_to_string(e))
                }
                Ok(_) -> Nil
              }
            }
            None -> Nil
          }

          // Update transform if all values provided
          case update.position, update.scale, update.rotation {
            Some(position), Some(scale), Some(rotation) -> {
              case carousel_db.update_element_transform(pool, element_id, position, scale, rotation) {
                Error(e) -> {
                  io.println("[Carousel API] Update transform error: " <> db_error_to_string(e))
                }
                Ok(_) -> Nil
              }
            }
            _, _, _ -> Nil
          }

          json_ok("Element updated")
        }
      }
    }
  }
}

type ElementUpdate {
  ElementUpdate(
    content: Option(String),
    position: Option(types.Position),
    scale: Option(Float),
    rotation: Option(Float),
  )
}

fn parse_element_update(body: String) -> Result(ElementUpdate, String) {
  let position_decoder = {
    use x <- decode.field("x", decode.float)
    use y <- decode.field("y", decode.float)
    decode.success(types.Position(x: x, y: y))
  }

  let decoder = {
    use content_str <- decode.optional_field("content", "", decode.string)
    use scale_val <- decode.optional_field("scale", -1.0, decode.float)
    use rotation_val <- decode.optional_field("rotation", -999.0, decode.float)
    use position_opt <- decode.optional_field("position", types.Position(x: -1.0, y: -1.0), position_decoder)

    let content = case content_str {
      "" -> None
      s -> Some(s)
    }

    let scale = case scale_val {
      -1.0 -> None
      s -> Some(s)
    }

    let rotation = case rotation_val {
      -999.0 -> None
      r -> Some(r)
    }

    let position = case position_opt.x, position_opt.y {
      -1.0, -1.0 -> None
      _, _ -> Some(position_opt)
    }

    decode.success(ElementUpdate(
      content: content,
      position: position,
      scale: scale,
      rotation: rotation,
    ))
  }

  case json.parse(body, decoder) {
    Ok(update) -> Ok(update)
    Error(_) -> Error("Invalid JSON")
  }
}

// ============================================================
// Regenerate Image
// ============================================================

/// POST /api/carousel/:id/image/:slide_id
/// Regenerates the background image for a slide
pub fn regenerate_image_handler(
  carousel_id: String,
  slide_id: String,
  body: String,
) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      // Get carousel for settings
      case carousel_db.get_carousel(pool, carousel_id) {
        Error(_) -> json_error(404, "Carousel not found")
        Ok(carousel) -> {
          // Parse optional new prompt
          let new_prompt = parse_prompt_from_body(body, carousel.slides, slide_id)

          io.println("[Carousel API] Regenerating image for slide: " <> slide_id)

          case bfl_service.regenerate_image(new_prompt, carousel.settings.art_style, carousel.aspect_ratio) {
            Error(e) -> json_error(500, "Image generation failed: " <> e)
            Ok(result) -> {
              // Update slide with new image
              case carousel_db.update_slide_image(pool, slide_id, result.image_url) {
                Error(e) -> json_error(500, "Failed to update slide: " <> db_error_to_string(e))
                Ok(_) -> {
                  let response_body = json.object([
                    #("status", json.string("ok")),
                    #("slide_id", json.string(slide_id)),
                    #("image_url", json.string(result.image_url)),
                  ])
                  |> json.to_string

                  response.new(200)
                  |> response.set_header("content-type", "application/json")
                  |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
                }
              }
            }
          }
        }
      }
    }
  }
}

fn parse_prompt_from_body(body: String, slides: List(types.Slide), slide_id: String) -> String {
  let prompt_decoder = {
    use prompt <- decode.optional_field("prompt", "", decode.string)
    decode.success(prompt)
  }

  case json.parse(body, prompt_decoder) {
    Ok("") -> get_slide_prompt(slides, slide_id)
    Ok(prompt) -> prompt
    Error(_) -> get_slide_prompt(slides, slide_id)
  }
}

fn get_slide_prompt(slides: List(types.Slide), slide_id: String) -> String {
  case list.find(slides, fn(s) { s.id == slide_id }) {
    Ok(slide) -> slide.image_prompt
    Error(_) -> "Abstract background"
  }
}

// ============================================================
// Make Text Viral
// ============================================================

/// POST /api/carousel/:id/viral/:slide_id
/// Rewrites text to be more viral
pub fn make_viral_handler(
  carousel_id: String,
  slide_id: String,
  body: String,
) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      case carousel_db.get_carousel(pool, carousel_id) {
        Error(_) -> json_error(404, "Carousel not found")
        Ok(carousel) -> {
          // Find slide and its first text element
          case list.find(carousel.slides, fn(s) { s.id == slide_id }) {
            Error(_) -> json_error(404, "Slide not found")
            Ok(slide) -> {
              case slide.elements {
                [] -> json_error(400, "Slide has no elements")
                [first, ..] -> {
                  // Determine slide type based on position
                  let slide_type = determine_slide_type(carousel.slides, slide_id)

                  io.println("[Carousel API] Making text viral: " <> string.slice(first.content, 0, 30))

                  case llm_service.make_text_viral(first.content, slide_type, carousel.language) {
                    Error(e) -> json_error(500, "LLM error: " <> e)
                    Ok(viral_text) -> {
                      // Update element content
                      case carousel_db.update_element_content(pool, first.id, viral_text) {
                        Error(e) -> json_error(500, "Failed to update: " <> db_error_to_string(e))
                        Ok(_) -> {
                          let response_body = json.object([
                            #("status", json.string("ok")),
                            #("original", json.string(first.content)),
                            #("viral", json.string(viral_text)),
                          ])
                          |> json.to_string

                          response.new(200)
                          |> response.set_header("content-type", "application/json")
                          |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

// ============================================================
// Upload Photos (Combo Mode)
// ============================================================

/// POST /api/carousel/:id/photos
/// Body: {"photos": ["url1", "url2", ...]}
/// Uploads photos and optionally analyzes them with Vision LLM
pub fn photos_handler(carousel_id: String, body: String) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      // Verify carousel exists
      case carousel_db.get_carousel(pool, carousel_id) {
        Error(postgres.DbNotFound) -> json_error(404, "Carousel not found")
        Error(e) -> json_error(500, "Database error: " <> db_error_to_string(e))
        Ok(_carousel) -> {
          // Parse photo URLs from request
          case parse_photos_request(body) {
            Error(e) -> json_error(400, "Invalid request: " <> e)
            Ok(photo_urls) -> {
              io.println("[Carousel API] Uploading " <> int.to_string(list.length(photo_urls)) <> " photos")

              // Save photos and optionally analyze them
              let results = list.index_map(photo_urls, fn(url, index) {
                io.println("[Carousel API] Processing photo " <> int.to_string(index + 1) <> ": " <> string.slice(url, 0, 50))

                // Analyze photo with Vision LLM
                let analysis = case llm_service.analyze_photo(url) {
                  Ok(a) -> Some(a)
                  Error(e) -> {
                    io.println("[Carousel API] Photo analysis failed: " <> e)
                    None
                  }
                }

                // Save to DB
                case carousel_db.save_uploaded_photo(pool, carousel_id, url, None, analysis) {
                  Ok(photo_id) -> Ok(#(photo_id, url, analysis))
                  Error(e) -> Error(db_error_to_string(e))
                }
              })

              let saved_photos = result.values(results)

              let response_body = json.object([
                #("status", json.string("ok")),
                #("carousel_id", json.string(carousel_id)),
                #("photos_count", json.int(list.length(saved_photos))),
                #("photos", json.array(saved_photos, fn(photo) {
                  let #(id, url, analysis) = photo
                  json.object([
                    #("id", json.string(id)),
                    #("url", json.string(url)),
                    #("analysis", photo_analysis_to_json(analysis)),
                  ])
                })),
              ])
              |> json.to_string

              response.new(201)
              |> response.set_header("content-type", "application/json")
              |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
            }
          }
        }
      }
    }
  }
}

fn parse_photos_request(body: String) -> Result(List(String), String) {
  let decoder = {
    use photos <- decode.field("photos", decode.list(decode.string))
    decode.success(photos)
  }

  case json.parse(body, decoder) {
    Ok(urls) -> {
      case urls {
        [] -> Error("At least one photo URL is required")
        _ -> Ok(urls)
      }
    }
    Error(_) -> Error("Invalid JSON or missing 'photos' array")
  }
}

// ============================================================
// Get Uploaded Photos
// ============================================================

/// GET /api/carousel/:id/photos
pub fn get_photos_handler(carousel_id: String) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      case carousel_db.get_uploaded_photos(pool, carousel_id) {
        Error(e) -> json_error(500, "Database error: " <> db_error_to_string(e))
        Ok(photos) -> {
          let response_body = json.object([
            #("status", json.string("ok")),
            #("carousel_id", json.string(carousel_id)),
            #("photos_count", json.int(list.length(photos))),
            #("photos", json.array(photos, uploaded_photo_to_json)),
          ])
          |> json.to_string

          response.new(200)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
        }
      }
    }
  }
}

fn uploaded_photo_to_json(photo: types.UploadedPhoto) -> json.Json {
  json.object([
    #("id", json.string(photo.id)),
    #("photo_url", json.string(photo.photo_url)),
    #("slide_position", case photo.slide_position {
      Some(pos) -> json.int(pos)
      None -> json.null()
    }),
    #("analysis", case photo.analysis {
      Some(a) -> json.object([
        #("description", json.string(a.description)),
        #("mood", json.string(a.mood)),
        #("style", json.string(a.style)),
        #("colors", json.string(a.colors)),
        #("suggested_text", json.string(a.suggested_text)),
      ])
      None -> json.null()
    }),
    #("created_at", json.string(photo.created_at)),
  ])
}

// ============================================================
// Generate Combo (Photos + LoRA)
// ============================================================

/// POST /api/carousel/:id/generate-combo
/// Body: {"total_slides": 5, "lora_url": "optional custom lora"}
/// Creates storyline combining uploaded photos with AI-generated images
pub fn combo_generate_handler(carousel_id: String, body: String) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      io.println("[Carousel API] Combo generation for: " <> carousel_id)

      // Get carousel
      case carousel_db.get_carousel(pool, carousel_id) {
        Error(postgres.DbNotFound) -> json_error(404, "Carousel not found")
        Error(e) -> json_error(500, "Database error: " <> db_error_to_string(e))
        Ok(carousel) -> {
          // Get uploaded photos
          case carousel_db.get_uploaded_photos(pool, carousel_id) {
            Error(e) -> json_error(500, "Failed to get photos: " <> db_error_to_string(e))
            Ok(photos) -> {
              case photos {
                [] -> json_error(400, "No photos uploaded. Use POST /photos first.")
                _ -> {
                  // Parse combo request
                  case parse_combo_request(body, list.length(photos)) {
                    Error(e) -> json_error(400, e)
                    Ok(req) -> {
                      io.println("[Carousel API] Creating storyline: " <> int.to_string(req.total_slides) <> " slides from " <> int.to_string(list.length(photos)) <> " photos")

                      // Generate storyline using Vision LLM
                      case llm_service.analyze_photos_and_generate_storyline(
                        photos,
                        carousel.topic,
                        req.total_slides,
                        carousel.language,
                      ) {
                        Error(e) -> json_error(500, "LLM error: " <> e)
                        Ok(combo_slides) -> {
                          io.println("[Carousel API] Got " <> int.to_string(list.length(combo_slides)) <> " combo slides")

                          // Create slides - photos use their URL, AI slides get generated
                          let slide_results = create_combo_slides(
                            pool,
                            carousel_id,
                            combo_slides,
                            carousel.settings.art_style,
                            carousel.aspect_ratio,
                            req.lora_url,
                          )

                          // Update carousel's lora_url if custom one provided
                          case req.lora_url {
                            Some(url) -> {
                              let _ = carousel_db.update_carousel_lora_url(pool, carousel_id, Some(url))
                              Nil
                            }
                            None -> Nil
                          }

                          let response_body = json.object([
                            #("status", json.string("ok")),
                            #("carousel_id", json.string(carousel_id)),
                            #("slides_count", json.int(list.length(combo_slides))),
                            #("photos_used", json.int(list.length(photos))),
                            #("slides", json.array(slide_results, fn(r) {
                              case r {
                                Ok(slide) -> json.object([
                                  #("id", json.string(slide.id)),
                                  #("text", json.string(get_first_element_text(slide.elements))),
                                  #("image_url", json.string(slide.image_url)),
                                  #("source", json.string("ready")),
                                ])
                                Error(e) -> json.object([
                                  #("status", json.string("error")),
                                  #("error", json.string(e)),
                                ])
                              }
                            })),
                          ])
                          |> json.to_string

                          response.new(200)
                          |> response.set_header("content-type", "application/json")
                          |> response.set_body(mist.Bytes(bytes_tree.from_string(response_body)))
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

type ComboRequest {
  ComboRequest(
    total_slides: Int,
    lora_url: Option(String),
  )
}

fn parse_combo_request(body: String, photo_count: Int) -> Result(ComboRequest, String) {
  let decoder = {
    use total <- decode.optional_field("total_slides", -1, decode.int)
    use lora <- decode.optional_field("lora_url", "", decode.string)

    let total_slides = case total {
      -1 -> photo_count + 2  // Default: photos + 2 extra AI slides
      n -> n
    }

    let lora_url = case lora {
      "" -> None
      url -> Some(url)
    }

    decode.success(ComboRequest(
      total_slides: total_slides,
      lora_url: lora_url,
    ))
  }

  case json.parse(body, decoder) {
    Ok(req) -> {
      case req.total_slides < photo_count {
        True -> Error("total_slides must be >= number of photos (" <> int.to_string(photo_count) <> ")")
        False -> Ok(req)
      }
    }
    Error(_) -> Ok(ComboRequest(
      total_slides: photo_count + 2,
      lora_url: None,
    ))
  }
}

fn create_combo_slides(
  pool: postgres.DbPool,
  carousel_id: String,
  combo_slides: List(types.ComboSlide),
  art_style: String,
  aspect_ratio: types.AspectRatio,
  lora_url: Option(String),
) -> List(Result(types.Slide, String)) {
  list.map(combo_slides, fn(combo) {
    io.println("[Carousel API] Creating combo slide " <> int.to_string(combo.position))

    let #(image_url, source_type, original_url) = case combo.source {
      types.UserUploaded -> {
        // Use the photo URL directly
        let url = option.unwrap(combo.photo_url, "")
        #(url, types.UserUploaded, combo.photo_url)
      }
      types.LoraGenerated -> {
        // Generate with FAL.ai LoRA
        let prompt = option.unwrap(combo.image_prompt, "Abstract background")
        case fal_service.generate_with_lora(prompt, art_style, aspect_ratio, lora_url) {
          Ok(result) -> #(result.image_url, types.LoraGenerated, None)
          Error(e) -> {
            io.println("[Carousel API] FAL generation failed: " <> e)
            #("", types.LoraGenerated, None)
          }
        }
      }
    }

    case image_url {
      "" -> Error("Image generation failed")
      url -> {
        // Create slide with source tracking
        case carousel_db.create_slide_with_source(
          pool,
          carousel_id,
          combo.position,
          url,
          option.unwrap(combo.image_prompt, ""),
          types.NoFrame,
          source_type,
          original_url,
        ) {
          Error(e) -> Error("DB error: " <> db_error_to_string(e))
          Ok(slide_id) -> {
            // Create text element
            let element = types.SlideElement(
              id: "",
              element_type: case combo.position {
                0 -> types.Heading
                _ -> types.Text
              },
              content: combo.text,
              position: types.default_position(),
              scale: 1.0,
              rotation: 0.0,
              z_index: 1,
              width: None,
              locked: False,
              style: None,
            )

            case carousel_db.create_element(pool, slide_id, element) {
              Error(_) -> Error("Failed to create element")
              Ok(element_id) -> {
                Ok(types.Slide(
                  id: slide_id,
                  image_url: url,
                  image_prompt: option.unwrap(combo.image_prompt, ""),
                  elements: [types.SlideElement(..element, id: element_id)],
                  frame: types.NoFrame,
                ))
              }
            }
          }
        }
      }
    }
  })
}

// ============================================================
// Delete Carousel
// ============================================================

/// DELETE /api/carousel/:id
pub fn delete_handler(carousel_id: String) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error(500, "Database not connected")
    Some(pool) -> {
      case carousel_db.delete_carousel(pool, carousel_id) {
        Error(e) -> json_error(500, "Failed to delete: " <> db_error_to_string(e))
        Ok(_) -> json_ok("Carousel deleted")
      }
    }
  }
}

// ============================================================
// Helpers
// ============================================================

fn determine_slide_type(slides: List(types.Slide), slide_id: String) -> types.SlideType {
  let total = list.length(slides)
  let indexed = list.index_map(slides, fn(s, i) { #(s.id, i) })
  case list.find(indexed, fn(item) { item.0 == slide_id }) {
    Ok(#(_, 0)) -> types.Hook
    Ok(#(_, i)) -> {
      case i == total - 1 {
        True -> types.CTA
        False -> types.Body
      }
    }
    Error(_) -> types.Body
  }
}

fn json_ok(message: String) -> Response(ResponseData) {
  let body = json.object([
    #("status", json.string("ok")),
    #("message", json.string(message)),
  ])
  |> json.to_string

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

fn json_error(code: Int, message: String) -> Response(ResponseData) {
  let body = json.object([
    #("status", json.string("error")),
    #("error", json.string(message)),
  ])
  |> json.to_string

  response.new(code)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

fn db_error_to_string(e: postgres.DbError) -> String {
  case e {
    postgres.DbConnectionError(msg) -> "Connection error: " <> msg
    postgres.DbQueryError(msg) -> "Query error: " <> msg
    postgres.DbNotFound -> "Not found"
  }
}

fn aspect_ratio_to_string(ar: types.AspectRatio) -> String {
  case ar {
    types.Square -> "1:1"
    types.Portrait -> "4:5"
    types.Story -> "9:16"
  }
}

fn language_to_string(l: types.Language) -> String {
  case l {
    types.En -> "en"
    types.Ru -> "ru"
  }
}

fn frame_to_string(f: types.FrameType) -> String {
  case f {
    types.NoFrame -> "none"
    types.BrowserLight -> "browser-light"
    types.BrowserDark -> "browser-dark"
    types.Film -> "film"
    types.Polaroid -> "polaroid"
    types.Paper -> "paper"
    types.PhotoBooth -> "photo-booth"
  }
}

fn element_type_to_string(et: types.ElementType) -> String {
  case et {
    types.Heading -> "heading"
    types.Text -> "text"
    types.Image -> "image"
    types.Sticker -> "sticker"
  }
}

fn overlay_to_string(om: types.OverlayMode) -> String {
  case om {
    types.Gradient -> "gradient"
    types.Solid -> "solid"
    types.NoOverlay -> "none"
  }
}

fn photo_analysis_to_json(analysis: Option(types.PhotoAnalysis)) -> json.Json {
  case analysis {
    Some(a) -> json.object([
      #("description", json.string(a.description)),
      #("mood", json.string(a.mood)),
      #("style", json.string(a.style)),
      #("colors", json.string(a.colors)),
      #("suggested_text", json.string(a.suggested_text)),
    ])
    None -> json.null()
  }
}
