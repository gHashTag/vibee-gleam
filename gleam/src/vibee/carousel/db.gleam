// Carousel Database Operations
// CRUD operations for carousels, slides, and elements

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import pog
import vibee/carousel/types.{
  type AspectRatio, type Carousel, type ElementType, type FrameType,
  type GenerationJob, type GenerationStatus, type GlobalSettings,
  type Language, type OverlayMode, type PhotoAnalysis, type Position,
  type Slide, type SlideElement, type SlideSource, type TextAlign,
  type UploadedPhoto, type VerticalPosition,
}
import vibee/db/postgres.{type DbError, type DbPool, DbNotFound, DbQueryError}

// ============================================================
// Helper: Add parameters to query
// ============================================================

fn add_parameters(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, p) { pog.parameter(q, p) })
}

fn pog_error_to_string(err: pog.QueryError) -> String {
  case err {
    pog.ConnectionUnavailable -> "Connection unavailable"
    pog.ConstraintViolated(msg, constraint, _) ->
      "Constraint violated: " <> msg <> " (" <> constraint <> ")"
    pog.PostgresqlError(code, name, msg) ->
      "PostgreSQL error " <> code <> " [" <> name <> "]: " <> msg
    pog.UnexpectedArgumentCount(expected, got) ->
      "Unexpected argument count: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected, got) ->
      "Unexpected argument type: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(_) -> "Unexpected result type"
    pog.QueryTimeout -> "Query timeout"
  }
}

// ============================================================
// Carousel CRUD
// ============================================================

/// Create a new carousel
pub fn create_carousel(
  pool: DbPool,
  user_id: Option(Int),
  topic: String,
  aspect_ratio: AspectRatio,
  language: Language,
  settings: GlobalSettings,
) -> Result(String, DbError) {
  let sql =
    "INSERT INTO carousels (user_id, topic, aspect_ratio, language, settings)
     VALUES ($1, $2, $3, $4, $5::jsonb)
     RETURNING id::text"

  let id_decoder = {
    use id <- decode.field(0, decode.string)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.nullable(pog.int, user_id),
      pog.text(topic),
      pog.text(aspect_ratio_to_string(aspect_ratio)),
      pog.text(language_to_string(language)),
      pog.text(settings_to_json(settings)),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to create carousel"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get carousel by ID (without slides)
pub fn get_carousel(pool: DbPool, id: String) -> Result(Carousel, DbError) {
  let sql =
    "SELECT id::text, user_id, topic, aspect_ratio, language, settings::text,
            created_at::text, updated_at::text
     FROM carousels WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(id)])
    |> pog.returning(decode_carousel_row())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [carousel])) -> {
      // Load slides for this carousel
      case get_slides_for_carousel(pool, id) {
        Ok(slides) -> Ok(types.Carousel(..carousel, slides: slides))
        Error(e) -> Error(e)
      }
    }
    Ok(pog.Returned(_, [])) -> Error(DbNotFound)
    Ok(_) -> Error(DbNotFound)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// List carousels for a user
pub fn list_carousels(
  pool: DbPool,
  user_id: Option(Int),
  limit: Int,
  offset: Int,
) -> Result(List(Carousel), DbError) {
  let #(sql, params) = case user_id {
    Some(uid) -> #(
      "SELECT id::text, user_id, topic, aspect_ratio, language, settings::text,
              created_at::text, updated_at::text
       FROM carousels WHERE user_id = $1
       ORDER BY created_at DESC LIMIT $2 OFFSET $3",
      [pog.int(uid), pog.int(limit), pog.int(offset)],
    )
    None -> #(
      "SELECT id::text, user_id, topic, aspect_ratio, language, settings::text,
              created_at::text, updated_at::text
       FROM carousels
       ORDER BY created_at DESC LIMIT $1 OFFSET $2",
      [pog.int(limit), pog.int(offset)],
    )
  }

  case
    pog.query(sql)
    |> add_parameters(params)
    |> pog.returning(decode_carousel_row())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, carousels)) -> Ok(carousels)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update carousel settings
pub fn update_carousel_settings(
  pool: DbPool,
  id: String,
  settings: GlobalSettings,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE carousels SET settings = $2::jsonb, updated_at = NOW()
     WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(id), pog.text(settings_to_json(settings))])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Delete carousel
pub fn delete_carousel(pool: DbPool, id: String) -> Result(Nil, DbError) {
  let sql = "DELETE FROM carousels WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(id)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// ============================================================
// Slide CRUD
// ============================================================

/// Create a new slide
pub fn create_slide(
  pool: DbPool,
  carousel_id: String,
  position: Int,
  image_url: String,
  image_prompt: String,
  frame: FrameType,
) -> Result(String, DbError) {
  let sql =
    "INSERT INTO carousel_slides (carousel_id, position, image_url, image_prompt, frame_type)
     VALUES ($1::uuid, $2, $3, $4, $5)
     RETURNING id::text"

  let id_decoder = {
    use id <- decode.field(0, decode.string)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(carousel_id),
      pog.int(position),
      pog.text(image_url),
      pog.text(image_prompt),
      pog.text(frame_type_to_string(frame)),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to create slide"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get slides for a carousel
pub fn get_slides_for_carousel(
  pool: DbPool,
  carousel_id: String,
) -> Result(List(Slide), DbError) {
  let sql =
    "SELECT id::text, image_url, image_prompt, frame_type
     FROM carousel_slides
     WHERE carousel_id = $1::uuid
     ORDER BY position ASC"

  case
    pog.query(sql)
    |> add_parameters([pog.text(carousel_id)])
    |> pog.returning(decode_slide_row())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, slides)) -> {
      // Load elements for each slide
      list.try_map(slides, fn(slide) {
        case get_elements_for_slide(pool, slide.id) {
          Ok(elements) -> Ok(types.Slide(..slide, elements: elements))
          Error(e) -> Error(e)
        }
      })
    }
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update slide image
pub fn update_slide_image(
  pool: DbPool,
  slide_id: String,
  image_url: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE carousel_slides SET image_url = $2 WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(slide_id), pog.text(image_url)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Delete slide
pub fn delete_slide(pool: DbPool, slide_id: String) -> Result(Nil, DbError) {
  let sql = "DELETE FROM carousel_slides WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(slide_id)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// ============================================================
// Element CRUD
// ============================================================

/// Create a new element
pub fn create_element(
  pool: DbPool,
  slide_id: String,
  element: SlideElement,
) -> Result(String, DbError) {
  let sql =
    "INSERT INTO slide_elements (slide_id, element_type, content, position_x, position_y,
                                  scale, rotation, z_index, width, locked, style_overrides)
     VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11::jsonb)
     RETURNING id::text"

  let id_decoder = {
    use id <- decode.field(0, decode.string)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(slide_id),
      pog.text(element_type_to_string(element.element_type)),
      pog.text(element.content),
      pog.float(element.position.x),
      pog.float(element.position.y),
      pog.float(element.scale),
      pog.float(element.rotation),
      pog.int(element.z_index),
      pog.nullable(pog.int, element.width),
      pog.bool(element.locked),
      pog.text(element_style_to_json(element.style)),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to create element"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get elements for a slide
pub fn get_elements_for_slide(
  pool: DbPool,
  slide_id: String,
) -> Result(List(SlideElement), DbError) {
  let sql =
    "SELECT id::text, element_type, content, position_x, position_y,
            scale, rotation, z_index, width, locked, style_overrides::text
     FROM slide_elements
     WHERE slide_id = $1::uuid
     ORDER BY z_index ASC"

  case
    pog.query(sql)
    |> add_parameters([pog.text(slide_id)])
    |> pog.returning(decode_element_row())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, elements)) -> Ok(elements)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update element position and transform
pub fn update_element_transform(
  pool: DbPool,
  element_id: String,
  position: Position,
  scale: Float,
  rotation: Float,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE slide_elements
     SET position_x = $2, position_y = $3, scale = $4, rotation = $5
     WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(element_id),
      pog.float(position.x),
      pog.float(position.y),
      pog.float(scale),
      pog.float(rotation),
    ])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update element content
pub fn update_element_content(
  pool: DbPool,
  element_id: String,
  content: String,
) -> Result(Nil, DbError) {
  let sql = "UPDATE slide_elements SET content = $2 WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(element_id), pog.text(content)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Delete element
pub fn delete_element(
  pool: DbPool,
  element_id: String,
) -> Result(Nil, DbError) {
  let sql = "DELETE FROM slide_elements WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(element_id)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// ============================================================
// Generation Job CRUD
// ============================================================

/// Create a generation job
pub fn create_generation_job(
  pool: DbPool,
  carousel_id: String,
  slide_id: String,
) -> Result(String, DbError) {
  let sql =
    "INSERT INTO carousel_generation_jobs (carousel_id, slide_id, status)
     VALUES ($1::uuid, $2::uuid, 'pending')
     RETURNING id::text"

  let id_decoder = {
    use id <- decode.field(0, decode.string)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([pog.text(carousel_id), pog.text(slide_id)])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to create job"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update job with BFL task ID
pub fn update_job_bfl_task(
  pool: DbPool,
  job_id: String,
  bfl_task_id: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE carousel_generation_jobs
     SET bfl_task_id = $2, status = 'processing'
     WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(job_id), pog.text(bfl_task_id)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Complete job with result
pub fn complete_job(
  pool: DbPool,
  job_id: String,
  result_url: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE carousel_generation_jobs
     SET status = 'completed', result_url = $2, completed_at = NOW()
     WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(job_id), pog.text(result_url)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Fail job with error
pub fn fail_job(
  pool: DbPool,
  job_id: String,
  error: String,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE carousel_generation_jobs
     SET status = 'failed', error_message = $2, completed_at = NOW()
     WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(job_id), pog.text(error)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get pending jobs
pub fn get_pending_jobs(
  pool: DbPool,
  limit: Int,
) -> Result(List(GenerationJob), DbError) {
  let sql =
    "SELECT id::text, carousel_id::text, slide_id::text, bfl_task_id, status, result_url
     FROM carousel_generation_jobs
     WHERE status IN ('pending', 'processing')
     ORDER BY created_at ASC
     LIMIT $1"

  case
    pog.query(sql)
    |> add_parameters([pog.int(limit)])
    |> pog.returning(decode_job_row())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, jobs)) -> Ok(jobs)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// ============================================================
// Type Converters
// ============================================================

fn aspect_ratio_to_string(ar: AspectRatio) -> String {
  case ar {
    types.Square -> "1:1"
    types.Portrait -> "4:5"
    types.Story -> "9:16"
  }
}

fn string_to_aspect_ratio(s: String) -> AspectRatio {
  case s {
    "1:1" -> types.Square
    "4:5" -> types.Portrait
    "9:16" -> types.Story
    _ -> types.Portrait
  }
}

fn language_to_string(l: Language) -> String {
  case l {
    types.En -> "en"
    types.Ru -> "ru"
  }
}

fn string_to_language(s: String) -> Language {
  case s {
    "en" -> types.En
    "ru" -> types.Ru
    _ -> types.Ru
  }
}

fn frame_type_to_string(f: FrameType) -> String {
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

fn string_to_frame_type(s: String) -> FrameType {
  case s {
    "none" -> types.NoFrame
    "browser-light" -> types.BrowserLight
    "browser-dark" -> types.BrowserDark
    "film" -> types.Film
    "polaroid" -> types.Polaroid
    "paper" -> types.Paper
    "photo-booth" -> types.PhotoBooth
    _ -> types.NoFrame
  }
}

fn element_type_to_string(e: ElementType) -> String {
  case e {
    types.Heading -> "heading"
    types.Text -> "text"
    types.Image -> "image"
    types.Sticker -> "sticker"
  }
}

fn string_to_element_type(s: String) -> ElementType {
  case s {
    "heading" -> types.Heading
    "text" -> types.Text
    "image" -> types.Image
    "sticker" -> types.Sticker
    _ -> types.Text
  }
}

fn string_to_status(s: String) -> GenerationStatus {
  case s {
    "pending" -> types.Pending
    "processing" -> types.Processing
    "completed" -> types.Completed
    "failed" -> types.Failed("")
    _ -> types.Pending
  }
}

// ============================================================
// JSON Serialization
// ============================================================

fn settings_to_json(s: GlobalSettings) -> String {
  json.object([
    #("font_family", json.string(s.font_family)),
    #("text_align", json.string(text_align_to_string(s.text_align))),
    #("font_size", json.int(s.font_size)),
    #("line_height", json.float(s.line_height)),
    #("text_padding", json.int(s.text_padding)),
    #("is_uppercase", json.bool(s.is_uppercase)),
    #("text_color", json.string(s.text_color)),
    #("highlight_color", json.string(s.highlight_color)),
    #("text_background_color", json.string(s.text_background_color)),
    #("text_background_opacity", json.int(s.text_background_opacity)),
    #("overlay_mode", json.string(overlay_mode_to_string(s.overlay_mode))),
    #("overlay_color", json.string(s.overlay_color)),
    #("overlay_opacity", json.int(s.overlay_opacity)),
    #("fade_height", json.int(s.fade_height)),
    #("corner_radius", json.int(s.corner_radius)),
    #("shadow_intensity", json.int(s.shadow_intensity)),
    #("show_swipe_arrow", json.bool(s.show_swipe_arrow)),
    #("swipe_indicator_position", json.string(vertical_position_to_string(s.swipe_indicator_position))),
    #("art_style", json.string(s.art_style)),
    #("branding_text", json.string(s.branding_text)),
    #("branding_position", json.string(corner_position_to_string(s.branding_position))),
  ])
  |> json.to_string
}

fn element_style_to_json(style: Option(types.ElementStyle)) -> String {
  case style {
    None -> "{}"
    Some(s) -> {
      let pairs = []
      let pairs = case s.font_size {
        Some(v) -> [#("font_size", json.int(v)), ..pairs]
        None -> pairs
      }
      let pairs = case s.font_family {
        Some(v) -> [#("font_family", json.string(v)), ..pairs]
        None -> pairs
      }
      let pairs = case s.text_color {
        Some(v) -> [#("text_color", json.string(v)), ..pairs]
        None -> pairs
      }
      let pairs = case s.text_align {
        Some(v) -> [#("text_align", json.string(text_align_to_string(v))), ..pairs]
        None -> pairs
      }
      let pairs = case s.is_uppercase {
        Some(v) -> [#("is_uppercase", json.bool(v)), ..pairs]
        None -> pairs
      }
      let pairs = case s.opacity {
        Some(v) -> [#("opacity", json.float(v)), ..pairs]
        None -> pairs
      }
      let pairs = case s.filter {
        Some(v) -> [#("filter", json.string(v)), ..pairs]
        None -> pairs
      }
      json.object(pairs) |> json.to_string
    }
  }
}

fn text_align_to_string(ta: TextAlign) -> String {
  case ta {
    types.Left -> "left"
    types.Center -> "center"
    types.Right -> "right"
  }
}

fn overlay_mode_to_string(om: OverlayMode) -> String {
  case om {
    types.Gradient -> "gradient"
    types.Solid -> "solid"
    types.NoOverlay -> "none"
  }
}

fn vertical_position_to_string(vp: VerticalPosition) -> String {
  case vp {
    types.Top -> "top"
    types.Bottom -> "bottom"
  }
}

fn corner_position_to_string(cp: types.CornerPosition) -> String {
  case cp {
    types.TopLeft -> "top-left"
    types.TopRight -> "top-right"
    types.BottomLeft -> "bottom-left"
    types.BottomRight -> "bottom-right"
  }
}

// ============================================================
// Decoders
// ============================================================

fn decode_carousel_row() -> Decoder(Carousel) {
  use id <- decode.field(0, decode.string)
  use user_id <- decode.field(1, decode.optional(decode.int))
  use topic <- decode.field(2, decode.string)
  use aspect_ratio <- decode.field(3, decode.string)
  use language <- decode.field(4, decode.string)
  use _settings_json <- decode.field(5, decode.string)
  use created_at <- decode.field(6, decode.string)
  use updated_at <- decode.field(7, decode.string)

  decode.success(types.Carousel(
    id: id,
    user_id: user_id,
    topic: topic,
    slides: [],  // Loaded separately
    settings: types.default_settings(),  // TODO: parse from JSON
    aspect_ratio: string_to_aspect_ratio(aspect_ratio),
    language: string_to_language(language),
    created_at: created_at,
    updated_at: updated_at,
  ))
}

fn decode_slide_row() -> Decoder(Slide) {
  use id <- decode.field(0, decode.string)
  use image_url <- decode.field(1, decode.optional(decode.string))
  use image_prompt <- decode.field(2, decode.optional(decode.string))
  use frame_type <- decode.field(3, decode.optional(decode.string))

  decode.success(types.Slide(
    id: id,
    image_url: option.unwrap(image_url, ""),
    image_prompt: option.unwrap(image_prompt, ""),
    elements: [],  // Loaded separately
    frame: string_to_frame_type(option.unwrap(frame_type, "none")),
  ))
}

fn decode_element_row() -> Decoder(SlideElement) {
  use id <- decode.field(0, decode.string)
  use element_type <- decode.field(1, decode.string)
  use content <- decode.field(2, decode.string)
  use position_x <- decode.field(3, decode.float)
  use position_y <- decode.field(4, decode.float)
  use scale <- decode.field(5, decode.float)
  use rotation <- decode.field(6, decode.float)
  use z_index <- decode.field(7, decode.int)
  use width <- decode.field(8, decode.optional(decode.int))
  use locked <- decode.field(9, decode.bool)
  use _style_json <- decode.field(10, decode.string)

  decode.success(types.SlideElement(
    id: id,
    element_type: string_to_element_type(element_type),
    content: content,
    position: types.Position(x: position_x, y: position_y),
    scale: scale,
    rotation: rotation,
    z_index: z_index,
    width: width,
    locked: locked,
    style: None,  // TODO: parse from JSON
  ))
}

fn decode_job_row() -> Decoder(GenerationJob) {
  use id <- decode.field(0, decode.string)
  use carousel_id <- decode.field(1, decode.string)
  use slide_id <- decode.field(2, decode.string)
  use bfl_task_id <- decode.field(3, decode.optional(decode.string))
  use status <- decode.field(4, decode.string)
  use result_url <- decode.field(5, decode.optional(decode.string))

  decode.success(types.GenerationJob(
    id: id,
    carousel_id: carousel_id,
    slide_id: slide_id,
    bfl_task_id: bfl_task_id,
    status: string_to_status(status),
    result_url: result_url,
  ))
}

// ============================================================
// Uploaded Photos CRUD (Combo Mode)
// ============================================================

/// Save an uploaded photo for combo mode
pub fn save_uploaded_photo(
  pool: DbPool,
  carousel_id: String,
  photo_url: String,
  slide_position: Option(Int),
  analysis: Option(PhotoAnalysis),
) -> Result(String, DbError) {
  let sql =
    "INSERT INTO carousel_uploaded_photos (carousel_id, photo_url, slide_position, analysis_json)
     VALUES ($1::uuid, $2, $3, $4::jsonb)
     RETURNING id::text"

  let id_decoder = {
    use id <- decode.field(0, decode.string)
    decode.success(id)
  }

  let analysis_json = case analysis {
    Some(a) -> photo_analysis_to_json(a)
    None -> "null"
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(carousel_id),
      pog.text(photo_url),
      pog.nullable(pog.int, slide_position),
      pog.text(analysis_json),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to save uploaded photo"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Get all uploaded photos for a carousel
pub fn get_uploaded_photos(
  pool: DbPool,
  carousel_id: String,
) -> Result(List(UploadedPhoto), DbError) {
  let sql =
    "SELECT id::text, carousel_id::text, photo_url, slide_position,
            analysis_json::text, created_at::text
     FROM carousel_uploaded_photos
     WHERE carousel_id = $1::uuid
     ORDER BY created_at ASC"

  case
    pog.query(sql)
    |> add_parameters([pog.text(carousel_id)])
    |> pog.returning(decode_uploaded_photo_row())
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, photos)) -> Ok(photos)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Update uploaded photo with analysis
pub fn update_photo_analysis(
  pool: DbPool,
  photo_id: String,
  analysis: PhotoAnalysis,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE carousel_uploaded_photos SET analysis_json = $2::jsonb
     WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(photo_id), pog.text(photo_analysis_to_json(analysis))])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Assign photo to slide position
pub fn assign_photo_to_slide(
  pool: DbPool,
  photo_id: String,
  slide_position: Int,
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE carousel_uploaded_photos SET slide_position = $2
     WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(photo_id), pog.int(slide_position)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

/// Delete uploaded photo
pub fn delete_uploaded_photo(
  pool: DbPool,
  photo_id: String,
) -> Result(Nil, DbError) {
  let sql = "DELETE FROM carousel_uploaded_photos WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(photo_id)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// ============================================================
// Slide with Source (Combo Mode)
// ============================================================

/// Create slide with source tracking for combo mode
pub fn create_slide_with_source(
  pool: DbPool,
  carousel_id: String,
  position: Int,
  image_url: String,
  image_prompt: String,
  frame: FrameType,
  source_type: SlideSource,
  original_photo_url: Option(String),
) -> Result(String, DbError) {
  let sql =
    "INSERT INTO carousel_slides
     (carousel_id, position, image_url, image_prompt, frame_type, source_type, original_photo_url)
     VALUES ($1::uuid, $2, $3, $4, $5, $6, $7)
     RETURNING id::text"

  let id_decoder = {
    use id <- decode.field(0, decode.string)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> add_parameters([
      pog.text(carousel_id),
      pog.int(position),
      pog.text(image_url),
      pog.text(image_prompt),
      pog.text(frame_type_to_string(frame)),
      pog.text(slide_source_to_string(source_type)),
      pog.nullable(pog.text, original_photo_url),
    ])
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error(DbQueryError("Failed to create slide with source"))
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// ============================================================
// Carousel LoRA URL
// ============================================================

/// Update carousel's custom LoRA URL
pub fn update_carousel_lora_url(
  pool: DbPool,
  carousel_id: String,
  lora_url: Option(String),
) -> Result(Nil, DbError) {
  let sql =
    "UPDATE carousels SET lora_url = $2, updated_at = NOW()
     WHERE id = $1::uuid"

  case
    pog.query(sql)
    |> add_parameters([pog.text(carousel_id), pog.nullable(pog.text, lora_url)])
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(DbQueryError(pog_error_to_string(e)))
  }
}

// ============================================================
// Photo Analysis JSON
// ============================================================

fn photo_analysis_to_json(a: PhotoAnalysis) -> String {
  json.object([
    #("description", json.string(a.description)),
    #("mood", json.string(a.mood)),
    #("style", json.string(a.style)),
    #("colors", json.string(a.colors)),
    #("suggested_text", json.string(a.suggested_text)),
  ])
  |> json.to_string
}

fn parse_photo_analysis(json_str: String) -> Option(PhotoAnalysis) {
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

  case json.parse(json_str, decoder) {
    Ok(analysis) -> Some(analysis)
    Error(_) -> None
  }
}

fn decode_uploaded_photo_row() -> Decoder(UploadedPhoto) {
  use id <- decode.field(0, decode.string)
  use carousel_id <- decode.field(1, decode.string)
  use photo_url <- decode.field(2, decode.string)
  use slide_position <- decode.field(3, decode.optional(decode.int))
  use analysis_json <- decode.field(4, decode.optional(decode.string))
  use created_at <- decode.field(5, decode.string)

  let analysis = case analysis_json {
    Some(json_str) -> parse_photo_analysis(json_str)
    None -> None
  }

  decode.success(types.UploadedPhoto(
    id: id,
    carousel_id: carousel_id,
    photo_url: photo_url,
    slide_position: slide_position,
    analysis: analysis,
    created_at: created_at,
  ))
}

fn slide_source_to_string(source: SlideSource) -> String {
  case source {
    types.UserUploaded -> "user_uploaded"
    types.LoraGenerated -> "lora_generated"
  }
}
