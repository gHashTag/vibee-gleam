// Feed API Handlers
// Public templates feed for social sharing and template reuse

import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import mist.{type Connection, type ResponseData}
import pog
import vibee/db/postgres
import vibee/logging

// ============================================================
// GET /api/feed?page=0&limit=20&sort=recent|popular
// Get public templates feed
// ============================================================

pub fn get_feed_handler(req: Request(Connection)) -> Response(ResponseData) {
  let query = request.get_query(req)

  // Parse query params
  let page = case query {
    Ok(params) -> {
      case find_param(params, "page") {
        Some(p) -> int.parse(p) |> result_or(0)
        None -> 0
      }
    }
    Error(_) -> 0
  }

  let limit = case query {
    Ok(params) -> {
      case find_param(params, "limit") {
        Some(l) -> int.parse(l) |> result_or(20) |> int.min(50)
        None -> 20
      }
    }
    Error(_) -> 20
  }

  let sort = case query {
    Ok(params) -> {
      case find_param(params, "sort") {
        Some("popular") -> "popular"
        _ -> "recent"
      }
    }
    Error(_) -> "recent"
  }

  // Optional: user_id to check if liked
  let user_id = case query {
    Ok(params) -> {
      case find_param(params, "user_id") {
        Some(id_str) -> case int.parse(id_str) {
          Ok(id) -> Some(id)
          Error(_) -> None
        }
        None -> None
      }
    }
    Error(_) -> None
  }

  case postgres.get_global_pool() {
    None -> json_error_response(500, "Database not connected")
    Some(pool) -> {
      case get_feed(pool, page, limit, sort, user_id) {
        Ok(templates) -> {
          json_success_response(json.object([
            #("templates", json.array(templates, template_to_json)),
            #("page", json.int(page)),
            #("limit", json.int(limit)),
            #("sort", json.string(sort)),
          ]))
        }
        Error(err) -> {
          logging.quick_error("Failed to get feed: " <> err)
          json_error_response(500, "Failed to get feed")
        }
      }
    }
  }
}

// ============================================================
// POST /api/feed/publish
// Publish a template to the feed
// ============================================================

pub fn publish_to_feed_handler(req: Request(Connection)) -> Response(ResponseData) {
  case mist.read_body(req, 10 * 1024 * 1024) {
    // 10MB limit
    Ok(body_req) -> {
      let body_str = case bit_array.to_string(body_req.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      let publish_decoder = {
        use telegram_id <- decode.field("telegram_id", decode.int)
        use creator_name <- decode.field("creator_name", decode.optional(decode.string))
        use creator_avatar <- decode.field("creator_avatar", decode.optional(decode.string))
        use name <- decode.field("name", decode.string)
        use description <- decode.field("description", decode.optional(decode.string))
        use thumbnail_url <- decode.field("thumbnail_url", decode.optional(decode.string))
        use video_url <- decode.field("video_url", decode.string)
        use template_settings <- decode.field("template_settings", decode.string)
        use assets <- decode.field("assets", decode.string)
        use tracks <- decode.field("tracks", decode.string)
        decode.success(PublishData(
          telegram_id: telegram_id,
          creator_name: creator_name,
          creator_avatar: creator_avatar,
          name: name,
          description: description,
          thumbnail_url: thumbnail_url,
          video_url: video_url,
          template_settings: template_settings,
          assets: assets,
          tracks: tracks,
        ))
      }

      case json.parse(body_str, publish_decoder) {
        Ok(data) -> {
          case postgres.get_global_pool() {
            None -> json_error_response(500, "Database not connected")
            Some(pool) -> {
              case publish_template(pool, data) {
                Ok(id) -> {
                  // Return full template data for frontend
                  case get_template_by_id(pool, id) {
                    Ok(Some(template)) -> json_success_response(template_to_json(template))
                    _ -> {
                      // Fallback to basic response if fetch fails
                      json_success_response(json.object([
                        #("success", json.bool(True)),
                        #("id", json.int(id)),
                      ]))
                    }
                  }
                }
                Error(err) -> {
                  logging.quick_error("Failed to publish template: " <> err)
                  json_error_response(500, "Failed to publish: " <> err)
                }
              }
            }
          }
        }
        Error(decode_err) -> {
          logging.quick_error("JSON decode error: " <> string.inspect(decode_err))
          json_error_response(400, "Invalid JSON body")
        }
      }
    }
    Error(_) -> {
      json_error_response(400, "Failed to read request body")
    }
  }
}

// ============================================================
// POST /api/feed/:id/like
// Like or unlike a template
// ============================================================

pub fn like_template_handler(
  req: Request(Connection),
  template_id: Int,
) -> Response(ResponseData) {
  case mist.read_body(req, 1024 * 1024) {
    Ok(body_req) -> {
      let body_str = case bit_array.to_string(body_req.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      let like_decoder = {
        use user_id <- decode.field("user_id", decode.int)
        decode.success(user_id)
      }

      case json.parse(body_str, like_decoder) {
        Ok(user_id) -> {
          case postgres.get_global_pool() {
            None -> json_error_response(500, "Database not connected")
            Some(pool) -> {
              case toggle_like(pool, template_id, user_id) {
                Ok(liked) -> {
                  json_success_response(json.object([
                    #("success", json.bool(True)),
                    #("liked", json.bool(liked)),
                  ]))
                }
                Error(err) -> {
                  logging.quick_error("Failed to toggle like: " <> err)
                  json_error_response(500, "Failed to toggle like")
                }
              }
            }
          }
        }
        Error(_) -> {
          json_error_response(400, "Invalid JSON body. Expected: {\"user_id\": 123}")
        }
      }
    }
    Error(_) -> {
      json_error_response(400, "Failed to read request body")
    }
  }
}

// ============================================================
// GET /api/feed/:id
// Get single template details
// ============================================================

pub fn get_template_handler(template_id: Int) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error_response(500, "Database not connected")
    Some(pool) -> {
      // Increment views
      let _ = increment_views(pool, template_id)

      case get_template_by_id(pool, template_id) {
        Ok(Some(template)) -> {
          json_success_response(template_to_json(template))
        }
        Ok(None) -> {
          json_error_response(404, "Template not found")
        }
        Error(err) -> {
          logging.quick_error("Failed to get template: " <> err)
          json_error_response(500, "Failed to get template")
        }
      }
    }
  }
}

// ============================================================
// POST /api/feed/:id/use
// Record template usage and return full data
// ============================================================

pub fn use_template_handler(template_id: Int) -> Response(ResponseData) {
  case postgres.get_global_pool() {
    None -> json_error_response(500, "Database not connected")
    Some(pool) -> {
      // Increment uses count
      let _ = increment_uses(pool, template_id)

      case get_template_by_id(pool, template_id) {
        Ok(Some(template)) -> {
          json_success_response(json.object([
            #("success", json.bool(True)),
            #("template", template_to_json(template)),
          ]))
        }
        Ok(None) -> {
          json_error_response(404, "Template not found")
        }
        Error(err) -> {
          logging.quick_error("Failed to use template: " <> err)
          json_error_response(500, "Failed to use template")
        }
      }
    }
  }
}

// ============================================================
// Types
// ============================================================

pub type PublishData {
  PublishData(
    telegram_id: Int,
    creator_name: Option(String),
    creator_avatar: Option(String),
    name: String,
    description: Option(String),
    thumbnail_url: Option(String),
    video_url: String,
    template_settings: String,
    assets: String,
    tracks: String,
  )
}

pub type FeedTemplate {
  FeedTemplate(
    id: Int,
    telegram_id: Int,
    creator_name: Option(String),
    creator_avatar: Option(String),
    creator_username: Option(String),
    name: String,
    description: Option(String),
    thumbnail_url: Option(String),
    video_url: String,
    template_settings: String,
    assets: String,
    tracks: String,
    likes_count: Int,
    views_count: Int,
    uses_count: Int,
    is_liked: Bool,
    created_at: String,
  )
}

// ============================================================
// Database functions
// ============================================================

fn get_feed(
  pool: pog.Connection,
  page: Int,
  limit: Int,
  sort: String,
  user_id: Option(Int),
) -> Result(List(FeedTemplate), String) {
  let offset = page * limit

  let order_by = case sort {
    "popular" -> "likes_count DESC, created_at DESC"
    _ -> "created_at DESC"
  }

  let sql = "
    SELECT
      pt.id, pt.telegram_id::int, pt.creator_name, pt.creator_avatar,
      u.username as creator_username,
      pt.name, pt.description, pt.thumbnail_url, pt.video_url,
      pt.template_settings::text, pt.assets::text, pt.tracks::text,
      pt.likes_count, pt.views_count, pt.uses_count,
      CASE WHEN tl.user_id IS NOT NULL THEN true ELSE false END as is_liked,
      pt.created_at::text
    FROM public_templates pt
    LEFT JOIN users u ON pt.user_id = u.id
    LEFT JOIN template_likes tl ON pt.id = tl.template_id AND tl.user_id = $3
    WHERE pt.is_public = true
    ORDER BY " <> order_by <> "
    LIMIT $1 OFFSET $2
  "

  let template_decoder = {
    use id <- decode.field(0, decode.int)
    use telegram_id <- decode.field(1, decode.int)
    use creator_name <- decode.field(2, decode.optional(decode.string))
    use creator_avatar <- decode.field(3, decode.optional(decode.string))
    use creator_username <- decode.field(4, decode.optional(decode.string))
    use name <- decode.field(5, decode.string)
    use description <- decode.field(6, decode.optional(decode.string))
    use thumbnail_url <- decode.field(7, decode.optional(decode.string))
    use video_url <- decode.field(8, decode.string)
    use template_settings <- decode.field(9, decode.string)
    use assets <- decode.field(10, decode.string)
    use tracks <- decode.field(11, decode.string)
    use likes_count <- decode.field(12, decode.int)
    use views_count <- decode.field(13, decode.int)
    use uses_count <- decode.field(14, decode.int)
    use is_liked <- decode.field(15, decode.bool)
    use created_at <- decode.field(16, decode.string)
    decode.success(FeedTemplate(
      id: id,
      telegram_id: telegram_id,
      creator_name: creator_name,
      creator_avatar: creator_avatar,
      creator_username: creator_username,
      name: name,
      description: description,
      thumbnail_url: thumbnail_url,
      video_url: video_url,
      template_settings: template_settings,
      assets: assets,
      tracks: tracks,
      likes_count: likes_count,
      views_count: views_count,
      uses_count: uses_count,
      is_liked: is_liked,
      created_at: created_at,
    ))
  }

  let user_id_param = case user_id {
    Some(id) -> id
    None -> 0
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.int(limit))
    |> pog.parameter(pog.int(offset))
    |> pog.parameter(pog.int(user_id_param))
    |> pog.returning(template_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, rows)) -> Ok(rows)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn publish_template(
  pool: pog.Connection,
  data: PublishData,
) -> Result(Int, String) {
  let sql = "
    INSERT INTO public_templates (
      telegram_id, creator_name, creator_avatar, name, description,
      thumbnail_url, video_url, template_settings, assets, tracks
    )
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8::jsonb, $9::jsonb, $10::jsonb)
    RETURNING id
  "

  let id_decoder = {
    use id <- decode.field("id", decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.int(data.telegram_id))
    |> pog.parameter(pog.nullable(pog.text, data.creator_name))
    |> pog.parameter(pog.nullable(pog.text, data.creator_avatar))
    |> pog.parameter(pog.text(data.name))
    |> pog.parameter(pog.nullable(pog.text, data.description))
    |> pog.parameter(pog.nullable(pog.text, data.thumbnail_url))
    |> pog.parameter(pog.text(data.video_url))
    |> pog.parameter(pog.text(data.template_settings))
    |> pog.parameter(pog.text(data.assets))
    |> pog.parameter(pog.text(data.tracks))
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [id])) -> Ok(id)
    Ok(_) -> Error("No id returned")
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn toggle_like(
  pool: pog.Connection,
  template_id: Int,
  user_id: Int,
) -> Result(Bool, String) {
  // Check if already liked
  let check_sql = "SELECT 1 FROM template_likes WHERE template_id = $1 AND user_id = $2"

  let exists_decoder = {
    use _ <- decode.field("?column?", decode.int)
    decode.success(True)
  }

  let exists = case
    pog.query(check_sql)
    |> pog.parameter(pog.int(template_id))
    |> pog.parameter(pog.int(user_id))
    |> pog.returning(exists_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [_])) -> True
    _ -> False
  }

  case exists {
    True -> {
      // Unlike
      let delete_sql = "DELETE FROM template_likes WHERE template_id = $1 AND user_id = $2"
      case
        pog.query(delete_sql)
        |> pog.parameter(pog.int(template_id))
        |> pog.parameter(pog.int(user_id))
        |> pog.execute(pool)
      {
        Ok(_) -> Ok(False)
        Error(e) -> Error(pog_error_to_string(e))
      }
    }
    False -> {
      // Like
      let insert_sql = "INSERT INTO template_likes (template_id, user_id) VALUES ($1, $2)"
      case
        pog.query(insert_sql)
        |> pog.parameter(pog.int(template_id))
        |> pog.parameter(pog.int(user_id))
        |> pog.execute(pool)
      {
        Ok(_) -> Ok(True)
        Error(e) -> Error(pog_error_to_string(e))
      }
    }
  }
}

fn get_template_by_id(
  pool: pog.Connection,
  template_id: Int,
) -> Result(Option(FeedTemplate), String) {
  let sql = "
    SELECT
      pt.id, pt.telegram_id::int, pt.creator_name, pt.creator_avatar,
      u.username as creator_username,
      pt.name, pt.description, pt.thumbnail_url, pt.video_url,
      pt.template_settings::text, pt.assets::text, pt.tracks::text,
      pt.likes_count, pt.views_count, pt.uses_count,
      false as is_liked,
      pt.created_at::text
    FROM public_templates pt
    LEFT JOIN users u ON pt.user_id = u.id
    WHERE pt.id = $1
  "

  let template_decoder = {
    use id <- decode.field(0, decode.int)
    use telegram_id <- decode.field(1, decode.int)
    use creator_name <- decode.field(2, decode.optional(decode.string))
    use creator_avatar <- decode.field(3, decode.optional(decode.string))
    use creator_username <- decode.field(4, decode.optional(decode.string))
    use name <- decode.field(5, decode.string)
    use description <- decode.field(6, decode.optional(decode.string))
    use thumbnail_url <- decode.field(7, decode.optional(decode.string))
    use video_url <- decode.field(8, decode.string)
    use template_settings <- decode.field(9, decode.string)
    use assets <- decode.field(10, decode.string)
    use tracks <- decode.field(11, decode.string)
    use likes_count <- decode.field(12, decode.int)
    use views_count <- decode.field(13, decode.int)
    use uses_count <- decode.field(14, decode.int)
    use is_liked <- decode.field(15, decode.bool)
    use created_at <- decode.field(16, decode.string)
    decode.success(FeedTemplate(
      id: id,
      telegram_id: telegram_id,
      creator_name: creator_name,
      creator_avatar: creator_avatar,
      creator_username: creator_username,
      name: name,
      description: description,
      thumbnail_url: thumbnail_url,
      video_url: video_url,
      template_settings: template_settings,
      assets: assets,
      tracks: tracks,
      likes_count: likes_count,
      views_count: views_count,
      uses_count: uses_count,
      is_liked: is_liked,
      created_at: created_at,
    ))
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.int(template_id))
    |> pog.returning(template_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [template])) -> Ok(Some(template))
    Ok(pog.Returned(_, [])) -> Ok(None)
    Ok(_) -> Ok(None)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn increment_views(pool: pog.Connection, template_id: Int) -> Result(Nil, String) {
  let sql = "UPDATE public_templates SET views_count = views_count + 1 WHERE id = $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(template_id))
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn increment_uses(pool: pog.Connection, template_id: Int) -> Result(Nil, String) {
  let sql = "UPDATE public_templates SET uses_count = uses_count + 1 WHERE id = $1"

  case
    pog.query(sql)
    |> pog.parameter(pog.int(template_id))
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

// ============================================================
// Helpers
// ============================================================

fn template_to_json(t: FeedTemplate) -> json.Json {
  json.object([
    #("id", json.int(t.id)),
    #("telegram_id", json.int(t.telegram_id)),
    #("creator_name", case t.creator_name {
      Some(n) -> json.string(n)
      None -> json.null()
    }),
    #("creator_avatar", case t.creator_avatar {
      Some(a) -> json.string(a)
      None -> json.null()
    }),
    #("creator_username", case t.creator_username {
      Some(u) -> json.string(u)
      None -> json.null()
    }),
    #("name", json.string(t.name)),
    #("description", case t.description {
      Some(d) -> json.string(d)
      None -> json.null()
    }),
    #("thumbnail_url", case t.thumbnail_url {
      Some(u) -> json.string(u)
      None -> json.null()
    }),
    #("video_url", json.string(t.video_url)),
    #("template_settings", json.string(t.template_settings)),
    #("assets", json.string(t.assets)),
    #("tracks", json.string(t.tracks)),
    #("likes_count", json.int(t.likes_count)),
    #("views_count", json.int(t.views_count)),
    #("uses_count", json.int(t.uses_count)),
    #("is_liked", json.bool(t.is_liked)),
    #("created_at", json.string(t.created_at)),
  ])
}

fn find_param(params: List(#(String, String)), key: String) -> Option(String) {
  case params {
    [] -> None
    [#(k, v), ..rest] -> {
      case k == key {
        True -> Some(v)
        False -> find_param(rest, key)
      }
    }
  }
}

fn result_or(result: Result(a, b), default: a) -> a {
  case result {
    Ok(v) -> v
    Error(_) -> default
  }
}

fn pog_error_to_string(err: pog.QueryError) -> String {
  case err {
    pog.ConstraintViolated(msg, constraint, _) ->
      "Constraint violated: " <> constraint <> " - " <> msg
    pog.PostgresqlError(code, name, msg) ->
      "PostgreSQL error " <> code <> " (" <> name <> "): " <> msg
    pog.UnexpectedArgumentCount(expected, got) ->
      "Unexpected argument count: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected, got) ->
      "Unexpected argument type: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(errors) ->
      "Unexpected result type: " <> string.inspect(errors)
    pog.ConnectionUnavailable -> "Connection unavailable"
    pog.QueryTimeout -> "Query timeout"
  }
}

fn json_success_response(data: json.Json) -> Response(ResponseData) {
  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(data))))
}

fn json_error_response(status: Int, message: String) -> Response(ResponseData) {
  let body = json.object([
    #("error", json.string(message)),
  ])
  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(body))))
}
