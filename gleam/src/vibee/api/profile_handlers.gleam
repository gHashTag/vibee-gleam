// Profile API Handlers
// User profiles, follow system, and subscription feed

import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mist.{type Connection, type ResponseData}
import pog
import vibee/db/postgres
import vibee/logging

// ============================================================
// Types
// ============================================================

pub type UserProfile {
  UserProfile(
    id: String,
    telegram_id: String,
    username: String,
    display_name: Option(String),
    bio: Option(String),
    avatar_url: Option(String),
    social_links: String,
    is_public: Bool,
    followers_count: Int,
    following_count: Int,
    templates_count: Int,
    total_views: Int,
    total_likes: Int,
    created_at: String,
    is_following: Bool,
    is_own_profile: Bool,
  )
}

pub type FollowUser {
  FollowUser(
    id: String,
    username: String,
    display_name: Option(String),
    avatar_url: Option(String),
    is_following: Bool,
  )
}

// ============================================================
// GET /api/users/:username
// Get user profile by username
// ============================================================

pub fn get_profile_handler(
  req: Request(Connection),
  username: String,
) -> Response(ResponseData) {
  let query = request.get_query(req)

  // Optional: current user's telegram_id to check following status
  let current_user_id = case query {
    Ok(params) -> {
      case find_param(params, "user_id") {
        Some(id_str) ->
          case int.parse(id_str) {
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
      case get_profile_by_username(pool, username, current_user_id) {
        Ok(Some(profile)) -> json_success_response(profile_to_json(profile))
        Ok(None) -> json_error_response(404, "User not found")
        Error(err) -> {
          logging.quick_error("Failed to get profile: " <> err)
          json_error_response(500, "Failed to get profile")
        }
      }
    }
  }
}

// ============================================================
// GET /api/users/id/:telegram_id
// Get or create user by Telegram ID (called on login)
// ============================================================

pub fn get_or_create_by_telegram_handler(
  req: Request(Connection),
  telegram_id_str: String,
) -> Response(ResponseData) {
  case int.parse(telegram_id_str) {
    Error(_) -> json_error_response(400, "Invalid telegram_id")
    Ok(telegram_id) -> {
      // Get optional profile data from query params
      let query = request.get_query(req)

      let username = case query {
        Ok(params) -> find_param(params, "username")
        Error(_) -> None
      }

      let display_name = case query {
        Ok(params) -> find_param(params, "display_name")
        Error(_) -> None
      }

      let avatar_url = case query {
        Ok(params) -> find_param(params, "avatar_url")
        Error(_) -> None
      }

      case postgres.get_global_pool() {
        None -> json_error_response(500, "Database not connected")
        Some(pool) -> {
          case
            get_or_create_user(pool, telegram_id, username, display_name, avatar_url)
          {
            Ok(profile) -> json_success_response(profile_to_json(profile))
            Error(err) -> {
              logging.quick_error("Failed to get/create user: " <> err)
              json_error_response(500, "Failed to get/create user")
            }
          }
        }
      }
    }
  }
}

// ============================================================
// PUT /api/users/:username
// Update user profile (auth required)
// ============================================================

pub fn update_profile_handler(
  req: Request(Connection),
  username: String,
) -> Response(ResponseData) {
  case mist.read_body(req, 1024 * 1024) {
    Ok(body_req) -> {
      let body_str = case bit_array.to_string(body_req.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      let update_decoder = {
        use telegram_id <- decode.field("telegram_id", decode.int)
        use display_name <- decode.optional_field(
          "display_name",
          None,
          decode.optional(decode.string),
        )
        use bio <- decode.optional_field(
          "bio",
          None,
          decode.optional(decode.string),
        )
        use avatar_url <- decode.optional_field(
          "avatar_url",
          None,
          decode.optional(decode.string),
        )
        use social_links <- decode.optional_field(
          "social_links",
          None,
          decode.optional(decode.string),
        )
        use is_public <- decode.optional_field(
          "is_public",
          None,
          decode.optional(decode.bool),
        )
        decode.success(#(
          telegram_id,
          display_name,
          bio,
          avatar_url,
          social_links,
          is_public,
        ))
      }

      case json.parse(body_str, update_decoder) {
        Ok(#(telegram_id, display_name, bio, avatar_url, social_links, is_public)) -> {
          case postgres.get_global_pool() {
            None -> json_error_response(500, "Database not connected")
            Some(pool) -> {
              // Verify user owns this profile
              case verify_profile_ownership(pool, username, telegram_id) {
                Ok(True) -> {
                  case
                    update_user_profile(
                      pool,
                      username,
                      display_name,
                      bio,
                      avatar_url,
                      social_links,
                      is_public,
                    )
                  {
                    Ok(profile) ->
                      json_success_response(json.object([
                        #("success", json.bool(True)),
                        #("profile", profile_to_json(profile)),
                      ]))
                    Error(err) -> {
                      logging.quick_error("Failed to update profile: " <> err)
                      json_error_response(500, "Failed to update profile")
                    }
                  }
                }
                Ok(False) ->
                  json_error_response(403, "Not authorized to update this profile")
                Error(err) -> {
                  logging.quick_error("Failed to verify ownership: " <> err)
                  json_error_response(500, "Failed to verify ownership")
                }
              }
            }
          }
        }
        Error(_) -> json_error_response(400, "Invalid JSON body")
      }
    }
    Error(_) -> json_error_response(400, "Failed to read request body")
  }
}

// ============================================================
// GET /api/users/:username/templates
// Get user's templates
// ============================================================

pub fn user_templates_handler(
  req: Request(Connection),
  username: String,
) -> Response(ResponseData) {
  let query = request.get_query(req)

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

  case postgres.get_global_pool() {
    None -> json_error_response(500, "Database not connected")
    Some(pool) -> {
      case get_user_templates(pool, username, page, limit) {
        Ok(templates) ->
          json_success_response(json.object([
            #("templates", json.array(templates, template_to_json)),
            #("page", json.int(page)),
            #("limit", json.int(limit)),
          ]))
        Error(err) -> {
          logging.quick_error("Failed to get user templates: " <> err)
          json_error_response(500, "Failed to get user templates")
        }
      }
    }
  }
}

// ============================================================
// GET /api/users/:username/followers
// Get user's followers list
// ============================================================

pub fn followers_handler(
  req: Request(Connection),
  username: String,
) -> Response(ResponseData) {
  let query = request.get_query(req)

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

  let current_user_id = case query {
    Ok(params) -> {
      case find_param(params, "user_id") {
        Some(id_str) ->
          case int.parse(id_str) {
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
      case get_followers(pool, username, page, limit, current_user_id) {
        Ok(users) ->
          json_success_response(json.object([
            #("users", json.array(users, follow_user_to_json)),
            #("page", json.int(page)),
            #("limit", json.int(limit)),
          ]))
        Error(err) -> {
          logging.quick_error("Failed to get followers: " <> err)
          json_error_response(500, "Failed to get followers")
        }
      }
    }
  }
}

// ============================================================
// GET /api/users/:username/following
// Get users this user follows
// ============================================================

pub fn following_handler(
  req: Request(Connection),
  username: String,
) -> Response(ResponseData) {
  let query = request.get_query(req)

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

  let current_user_id = case query {
    Ok(params) -> {
      case find_param(params, "user_id") {
        Some(id_str) ->
          case int.parse(id_str) {
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
      case get_following(pool, username, page, limit, current_user_id) {
        Ok(users) ->
          json_success_response(json.object([
            #("users", json.array(users, follow_user_to_json)),
            #("page", json.int(page)),
            #("limit", json.int(limit)),
          ]))
        Error(err) -> {
          logging.quick_error("Failed to get following: " <> err)
          json_error_response(500, "Failed to get following")
        }
      }
    }
  }
}

// ============================================================
// POST /api/users/:username/follow
// Follow a user
// ============================================================

pub fn follow_handler(
  req: Request(Connection),
  username: String,
) -> Response(ResponseData) {
  case mist.read_body(req, 1024 * 1024) {
    Ok(body_req) -> {
      let body_str = case bit_array.to_string(body_req.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      let follow_decoder = {
        use telegram_id <- decode.field("telegram_id", decode.int)
        decode.success(telegram_id)
      }

      case json.parse(body_str, follow_decoder) {
        Ok(follower_telegram_id) -> {
          case postgres.get_global_pool() {
            None -> json_error_response(500, "Database not connected")
            Some(pool) -> {
              case follow_user(pool, follower_telegram_id, username) {
                Ok(True) ->
                  json_success_response(json.object([
                    #("success", json.bool(True)),
                    #("is_following", json.bool(True)),
                  ]))
                Ok(False) ->
                  json_error_response(400, "Cannot follow (user not found or self-follow)")
                Error(err) -> {
                  logging.quick_error("Failed to follow: " <> err)
                  json_error_response(500, "Failed to follow user")
                }
              }
            }
          }
        }
        Error(_) ->
          json_error_response(400, "Invalid JSON body. Expected: {\"telegram_id\": 123}")
      }
    }
    Error(_) -> json_error_response(400, "Failed to read request body")
  }
}

// ============================================================
// DELETE /api/users/:username/follow
// Unfollow a user
// ============================================================

pub fn unfollow_handler(
  req: Request(Connection),
  username: String,
) -> Response(ResponseData) {
  case mist.read_body(req, 1024 * 1024) {
    Ok(body_req) -> {
      let body_str = case bit_array.to_string(body_req.body) {
        Ok(s) -> s
        Error(_) -> ""
      }

      let unfollow_decoder = {
        use telegram_id <- decode.field("telegram_id", decode.int)
        decode.success(telegram_id)
      }

      case json.parse(body_str, unfollow_decoder) {
        Ok(follower_telegram_id) -> {
          case postgres.get_global_pool() {
            None -> json_error_response(500, "Database not connected")
            Some(pool) -> {
              case unfollow_user(pool, follower_telegram_id, username) {
                Ok(_) ->
                  json_success_response(json.object([
                    #("success", json.bool(True)),
                    #("is_following", json.bool(False)),
                  ]))
                Error(err) -> {
                  logging.quick_error("Failed to unfollow: " <> err)
                  json_error_response(500, "Failed to unfollow user")
                }
              }
            }
          }
        }
        Error(_) ->
          json_error_response(400, "Invalid JSON body. Expected: {\"telegram_id\": 123}")
      }
    }
    Error(_) -> json_error_response(400, "Failed to read request body")
  }
}

// ============================================================
// GET /api/feed/following
// Get templates from users the current user follows
// ============================================================

pub fn following_feed_handler(req: Request(Connection)) -> Response(ResponseData) {
  let query = request.get_query(req)

  let telegram_id = case query {
    Ok(params) -> {
      case find_param(params, "telegram_id") {
        Some(id_str) ->
          case int.parse(id_str) {
            Ok(id) -> Some(id)
            Error(_) -> None
          }
        None -> None
      }
    }
    Error(_) -> None
  }

  case telegram_id {
    None -> json_error_response(400, "telegram_id is required")
    Some(user_telegram_id) -> {
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

      case postgres.get_global_pool() {
        None -> json_error_response(500, "Database not connected")
        Some(pool) -> {
          case get_following_feed(pool, user_telegram_id, page, limit) {
            Ok(templates) ->
              json_success_response(json.object([
                #("templates", json.array(templates, template_to_json)),
                #("page", json.int(page)),
                #("limit", json.int(limit)),
              ]))
            Error(err) -> {
              logging.quick_error("Failed to get following feed: " <> err)
              json_error_response(500, "Failed to get following feed")
            }
          }
        }
      }
    }
  }
}

// ============================================================
// Database Functions
// ============================================================

fn get_profile_by_username(
  pool: pog.Connection,
  username: String,
  current_user_telegram_id: Option(Int),
) -> Result(Option(UserProfile), String) {
  let sql =
    "
    SELECT
      u.id, u.telegram_id, u.username, u.display_name, u.bio, u.avatar_url,
      COALESCE(u.social_links::text, '[]') as social_links,
      u.is_public, u.followers_count, u.following_count, u.templates_count,
      u.total_views, u.total_likes, u.created_at::text,
      CASE WHEN f.id IS NOT NULL THEN true ELSE false END as is_following,
      CASE WHEN u.telegram_id = $2 THEN true ELSE false END as is_own_profile
    FROM users u
    LEFT JOIN follows f ON f.following_id = u.id
      AND f.follower_id = (SELECT id FROM users WHERE telegram_id = $2)
    WHERE LOWER(u.username) = LOWER($1)
  "

  let profile_decoder = {
    use id <- decode.field(0, decode.string)
    use telegram_id <- decode.field(1, decode.string)
    use username <- decode.field(2, decode.string)
    use display_name <- decode.field(3, decode.optional(decode.string))
    use bio <- decode.field(4, decode.optional(decode.string))
    use avatar_url <- decode.field(5, decode.optional(decode.string))
    use social_links <- decode.field(6, decode.string)
    use is_public <- decode.field(7, decode.bool)
    use followers_count <- decode.field(8, decode.int)
    use following_count <- decode.field(9, decode.int)
    use templates_count <- decode.field(10, decode.int)
    use total_views <- decode.field(11, decode.int)
    use total_likes <- decode.field(12, decode.int)
    use created_at <- decode.field(13, decode.string)
    use is_following <- decode.field(14, decode.bool)
    use is_own_profile <- decode.field(15, decode.bool)
    decode.success(UserProfile(
      id: id,
      telegram_id: telegram_id,
      username: username,
      display_name: display_name,
      bio: bio,
      avatar_url: avatar_url,
      social_links: social_links,
      is_public: is_public,
      followers_count: followers_count,
      following_count: following_count,
      templates_count: templates_count,
      total_views: total_views,
      total_likes: total_likes,
      created_at: created_at,
      is_following: is_following,
      is_own_profile: is_own_profile,
    ))
  }

  let user_id_param = case current_user_telegram_id {
    Some(id) -> id
    None -> 0
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.text(username))
    |> pog.parameter(pog.int(user_id_param))
    |> pog.returning(profile_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [profile])) -> Ok(Some(profile))
    Ok(pog.Returned(_, [])) -> Ok(None)
    Ok(_) -> Ok(None)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn get_or_create_user(
  pool: pog.Connection,
  telegram_id: Int,
  username: Option(String),
  display_name: Option(String),
  avatar_url: Option(String),
) -> Result(UserProfile, String) {
  // Try to get existing user first
  let get_sql =
    "
    SELECT
      id, telegram_id, username, display_name, bio, avatar_url,
      COALESCE(social_links::text, '[]') as social_links,
      is_public, followers_count, following_count, templates_count,
      total_views, total_likes, created_at::text,
      false as is_following, true as is_own_profile
    FROM users
    WHERE telegram_id = $1
  "

  let profile_decoder = {
    use id <- decode.field(0, decode.string)
    use tg_id <- decode.field(1, decode.string)
    use uname <- decode.field(2, decode.optional(decode.string))
    use dname <- decode.field(3, decode.optional(decode.string))
    use bio <- decode.field(4, decode.optional(decode.string))
    use avatar <- decode.field(5, decode.optional(decode.string))
    use social_links <- decode.field(6, decode.string)
    use is_public <- decode.field(7, decode.bool)
    use followers_count <- decode.field(8, decode.int)
    use following_count <- decode.field(9, decode.int)
    use templates_count <- decode.field(10, decode.int)
    use total_views <- decode.field(11, decode.int)
    use total_likes <- decode.field(12, decode.int)
    use created_at <- decode.field(13, decode.string)
    use is_following <- decode.field(14, decode.bool)
    use is_own_profile <- decode.field(15, decode.bool)
    decode.success(UserProfile(
      id: id,
      telegram_id: tg_id,
      username: option.unwrap(uname, "user_" <> tg_id),
      display_name: dname,
      bio: bio,
      avatar_url: avatar,
      social_links: social_links,
      is_public: is_public,
      followers_count: followers_count,
      following_count: following_count,
      templates_count: templates_count,
      total_views: total_views,
      total_likes: total_likes,
      created_at: created_at,
      is_following: is_following,
      is_own_profile: is_own_profile,
    ))
  }

  case
    pog.query(get_sql)
    |> pog.parameter(pog.text(int.to_string(telegram_id)))
    |> pog.returning(profile_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [profile])) -> Ok(profile)
    Ok(pog.Returned(_, [])) -> {
      // User doesn't exist, create with generated UUID
      let insert_sql =
        "
        INSERT INTO users (id, telegram_id, username, display_name, avatar_url, created_at, updated_at)
        VALUES (gen_random_uuid()::text, $1, $2, $3, $4, NOW(), NOW())
        RETURNING
          id, telegram_id, username, display_name, bio, avatar_url,
          COALESCE(social_links::text, '[]') as social_links,
          is_public, followers_count, following_count, templates_count,
          total_views, total_likes, created_at::text,
          false as is_following, true as is_own_profile
      "

      case
        pog.query(insert_sql)
        |> pog.parameter(pog.text(int.to_string(telegram_id)))
        |> pog.parameter(pog.nullable(pog.text, username))
        |> pog.parameter(pog.nullable(pog.text, display_name))
        |> pog.parameter(pog.nullable(pog.text, avatar_url))
        |> pog.returning(profile_decoder)
        |> pog.execute(pool)
      {
        Ok(pog.Returned(_, [profile])) -> Ok(profile)
        Ok(_) -> Error("Failed to create user")
        Error(e) -> Error(pog_error_to_string(e))
      }
    }
    Ok(_) -> Error("Unexpected result")
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn verify_profile_ownership(
  pool: pog.Connection,
  username: String,
  telegram_id: Int,
) -> Result(Bool, String) {
  // Note: telegram_id is TEXT in DB, so we pass as text
  let sql =
    "SELECT 1 FROM users WHERE LOWER(username) = LOWER($1) AND telegram_id = $2"

  let exists_decoder = {
    use _ <- decode.field("?column?", decode.int)
    decode.success(True)
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.text(username))
    |> pog.parameter(pog.text(int.to_string(telegram_id)))
    |> pog.returning(exists_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [_])) -> Ok(True)
    Ok(_) -> Ok(False)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn update_user_profile(
  pool: pog.Connection,
  username: String,
  display_name: Option(String),
  bio: Option(String),
  avatar_url: Option(String),
  social_links: Option(String),
  is_public: Option(Bool),
) -> Result(UserProfile, String) {
  // Build update query dynamically (simplified - update all provided fields)
  let sql =
    "
    UPDATE users SET
      display_name = COALESCE($2, display_name),
      bio = COALESCE($3, bio),
      avatar_url = COALESCE($4, avatar_url),
      social_links = COALESCE($5::jsonb, social_links),
      is_public = COALESCE($6, is_public),
      updated_at = NOW()
    WHERE LOWER(username) = LOWER($1)
    RETURNING
      id, telegram_id, username, display_name, bio, avatar_url,
      COALESCE(social_links::text, '[]') as social_links,
      is_public, followers_count, following_count, templates_count,
      total_views, total_likes, created_at::text,
      false as is_following, true as is_own_profile
  "

  let profile_decoder = {
    use id <- decode.field(0, decode.string)
    use telegram_id <- decode.field(1, decode.string)
    use uname <- decode.field(2, decode.string)
    use dname <- decode.field(3, decode.optional(decode.string))
    use bio <- decode.field(4, decode.optional(decode.string))
    use avatar <- decode.field(5, decode.optional(decode.string))
    use social <- decode.field(6, decode.string)
    use is_pub <- decode.field(7, decode.bool)
    use followers <- decode.field(8, decode.int)
    use following <- decode.field(9, decode.int)
    use templates <- decode.field(10, decode.int)
    use views <- decode.field(11, decode.int)
    use likes <- decode.field(12, decode.int)
    use created <- decode.field(13, decode.string)
    use is_follow <- decode.field(14, decode.bool)
    use is_own <- decode.field(15, decode.bool)
    decode.success(UserProfile(
      id: id,
      telegram_id: telegram_id,
      username: uname,
      display_name: dname,
      bio: bio,
      avatar_url: avatar,
      social_links: social,
      is_public: is_pub,
      followers_count: followers,
      following_count: following,
      templates_count: templates,
      total_views: views,
      total_likes: likes,
      created_at: created,
      is_following: is_follow,
      is_own_profile: is_own,
    ))
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.text(username))
    |> pog.parameter(pog.nullable(pog.text, display_name))
    |> pog.parameter(pog.nullable(pog.text, bio))
    |> pog.parameter(pog.nullable(pog.text, avatar_url))
    |> pog.parameter(pog.nullable(pog.text, social_links))
    |> pog.parameter(pog.nullable(pog.bool, is_public))
    |> pog.returning(profile_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, [profile])) -> Ok(profile)
    Ok(_) -> Error("User not found")
    Error(e) -> Error(pog_error_to_string(e))
  }
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

fn get_user_templates(
  pool: pog.Connection,
  username: String,
  page: Int,
  limit: Int,
) -> Result(List(FeedTemplate), String) {
  let offset = page * limit

  let sql =
    "
    SELECT
      pt.id, pt.telegram_id, pt.creator_name, pt.creator_avatar,
      u.username as creator_username,
      pt.name, pt.description, pt.thumbnail_url, pt.video_url,
      pt.template_settings::text, pt.assets::text, pt.tracks::text,
      pt.likes_count, pt.views_count, pt.uses_count,
      false as is_liked, pt.created_at::text
    FROM public_templates pt
    JOIN users u ON u.telegram_id = pt.telegram_id::text
    WHERE LOWER(u.username) = LOWER($1) AND pt.is_public = true
    ORDER BY pt.created_at DESC
    LIMIT $2 OFFSET $3
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
    |> pog.parameter(pog.text(username))
    |> pog.parameter(pog.int(limit))
    |> pog.parameter(pog.int(offset))
    |> pog.returning(template_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, rows)) -> Ok(rows)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn get_followers(
  pool: pog.Connection,
  username: String,
  page: Int,
  limit: Int,
  current_user_telegram_id: Option(Int),
) -> Result(List(FollowUser), String) {
  let offset = page * limit

  let sql =
    "
    SELECT
      u.id, u.username, u.display_name, u.avatar_url,
      CASE WHEN f2.id IS NOT NULL THEN true ELSE false END as is_following
    FROM follows f
    JOIN users u ON u.id = f.follower_id
    JOIN users target ON target.id = f.following_id
    LEFT JOIN follows f2 ON f2.following_id = u.id
      AND f2.follower_id = (SELECT id FROM users WHERE telegram_id = $4)
    WHERE LOWER(target.username) = LOWER($1)
    ORDER BY f.created_at DESC
    LIMIT $2 OFFSET $3
  "

  let user_decoder = {
    use id <- decode.field(0, decode.string)
    use uname <- decode.field(1, decode.string)
    use dname <- decode.field(2, decode.optional(decode.string))
    use avatar <- decode.field(3, decode.optional(decode.string))
    use is_following <- decode.field(4, decode.bool)
    decode.success(FollowUser(
      id: id,
      username: uname,
      display_name: dname,
      avatar_url: avatar,
      is_following: is_following,
    ))
  }

  let user_id_param = case current_user_telegram_id {
    Some(id) -> int.to_string(id)
    None -> "0"
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.text(username))
    |> pog.parameter(pog.int(limit))
    |> pog.parameter(pog.int(offset))
    |> pog.parameter(pog.text(user_id_param))
    |> pog.returning(user_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, rows)) -> Ok(rows)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn get_following(
  pool: pog.Connection,
  username: String,
  page: Int,
  limit: Int,
  current_user_telegram_id: Option(Int),
) -> Result(List(FollowUser), String) {
  let offset = page * limit

  let sql =
    "
    SELECT
      u.id, u.username, u.display_name, u.avatar_url,
      CASE WHEN f2.id IS NOT NULL THEN true ELSE false END as is_following
    FROM follows f
    JOIN users u ON u.id = f.following_id
    JOIN users source ON source.id = f.follower_id
    LEFT JOIN follows f2 ON f2.following_id = u.id
      AND f2.follower_id = (SELECT id FROM users WHERE telegram_id = $4)
    WHERE LOWER(source.username) = LOWER($1)
    ORDER BY f.created_at DESC
    LIMIT $2 OFFSET $3
  "

  let user_decoder = {
    use id <- decode.field(0, decode.string)
    use uname <- decode.field(1, decode.string)
    use dname <- decode.field(2, decode.optional(decode.string))
    use avatar <- decode.field(3, decode.optional(decode.string))
    use is_following <- decode.field(4, decode.bool)
    decode.success(FollowUser(
      id: id,
      username: uname,
      display_name: dname,
      avatar_url: avatar,
      is_following: is_following,
    ))
  }

  let user_id_param = case current_user_telegram_id {
    Some(id) -> int.to_string(id)
    None -> "0"
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.text(username))
    |> pog.parameter(pog.int(limit))
    |> pog.parameter(pog.int(offset))
    |> pog.parameter(pog.text(user_id_param))
    |> pog.returning(user_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, rows)) -> Ok(rows)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn follow_user(
  pool: pog.Connection,
  follower_telegram_id: Int,
  target_username: String,
) -> Result(Bool, String) {
  let sql =
    "
    INSERT INTO follows (follower_id, following_id)
    SELECT
      (SELECT id FROM users WHERE telegram_id = $1),
      (SELECT id FROM users WHERE LOWER(username) = LOWER($2))
    WHERE EXISTS (SELECT 1 FROM users WHERE telegram_id = $1)
      AND EXISTS (SELECT 1 FROM users WHERE LOWER(username) = LOWER($2))
      AND (SELECT telegram_id FROM users WHERE LOWER(username) = LOWER($2)) != $1
    ON CONFLICT (follower_id, following_id) DO NOTHING
    RETURNING id
  "

  let id_decoder = {
    use id <- decode.field("id", decode.int)
    decode.success(id)
  }

  case
    pog.query(sql)
    |> pog.parameter(pog.int(follower_telegram_id))
    |> pog.parameter(pog.text(target_username))
    |> pog.returning(id_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(count, _)) -> Ok(count > 0)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn unfollow_user(
  pool: pog.Connection,
  follower_telegram_id: Int,
  target_username: String,
) -> Result(Nil, String) {
  let sql =
    "
    DELETE FROM follows
    WHERE follower_id = (SELECT id FROM users WHERE telegram_id = $1)
      AND following_id = (SELECT id FROM users WHERE LOWER(username) = LOWER($2))
  "

  case
    pog.query(sql)
    |> pog.parameter(pog.int(follower_telegram_id))
    |> pog.parameter(pog.text(target_username))
    |> pog.execute(pool)
  {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

fn get_following_feed(
  pool: pog.Connection,
  user_telegram_id: Int,
  page: Int,
  limit: Int,
) -> Result(List(FeedTemplate), String) {
  let offset = page * limit

  let sql =
    "
    SELECT
      pt.id, pt.telegram_id, pt.creator_name, pt.creator_avatar,
      u.username as creator_username,
      pt.name, pt.description, pt.thumbnail_url, pt.video_url,
      pt.template_settings::text, pt.assets::text, pt.tracks::text,
      pt.likes_count, pt.views_count, pt.uses_count,
      CASE WHEN tl.user_id IS NOT NULL THEN true ELSE false END as is_liked,
      pt.created_at::text
    FROM public_templates pt
    JOIN users u ON u.telegram_id = pt.telegram_id
    JOIN follows f ON f.following_id = u.id
    LEFT JOIN template_likes tl ON pt.id = tl.template_id AND tl.user_id = $1
    WHERE f.follower_id = (SELECT id FROM users WHERE telegram_id = $1)
      AND pt.is_public = true
    ORDER BY pt.created_at DESC
    LIMIT $2 OFFSET $3
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
    |> pog.parameter(pog.int(user_telegram_id))
    |> pog.parameter(pog.int(limit))
    |> pog.parameter(pog.int(offset))
    |> pog.returning(template_decoder)
    |> pog.execute(pool)
  {
    Ok(pog.Returned(_, rows)) -> Ok(rows)
    Error(e) -> Error(pog_error_to_string(e))
  }
}

// ============================================================
// Helpers
// ============================================================

fn profile_to_json(p: UserProfile) -> json.Json {
  json.object([
    #("id", json.string(p.id)),
    #("telegram_id", json.string(p.telegram_id)),
    #("username", json.string(p.username)),
    #("display_name", case p.display_name {
      Some(n) -> json.string(n)
      None -> json.null()
    }),
    #("bio", case p.bio {
      Some(b) -> json.string(b)
      None -> json.null()
    }),
    #("avatar_url", case p.avatar_url {
      Some(a) -> json.string(a)
      None -> json.null()
    }),
    #("social_links", json.string(p.social_links)),
    #("is_public", json.bool(p.is_public)),
    #("followers_count", json.int(p.followers_count)),
    #("following_count", json.int(p.following_count)),
    #("templates_count", json.int(p.templates_count)),
    #("total_views", json.int(p.total_views)),
    #("total_likes", json.int(p.total_likes)),
    #("created_at", json.string(p.created_at)),
    #("is_following", json.bool(p.is_following)),
    #("is_own_profile", json.bool(p.is_own_profile)),
  ])
}

fn follow_user_to_json(u: FollowUser) -> json.Json {
  json.object([
    #("id", json.string(u.id)),
    #("username", json.string(u.username)),
    #("display_name", case u.display_name {
      Some(n) -> json.string(n)
      None -> json.null()
    }),
    #("avatar_url", case u.avatar_url {
      Some(a) -> json.string(a)
      None -> json.null()
    }),
    #("is_following", json.bool(u.is_following)),
  ])
}

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
  let body = json.object([#("error", json.string(message))])
  response.new(status)
  |> response.set_header("content-type", "application/json")
  |> response.set_header("access-control-allow-origin", "*")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(body))))
}
