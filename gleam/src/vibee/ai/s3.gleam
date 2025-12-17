// AWS S3 Integration
// API Documentation: https://docs.aws.amazon.com/AmazonS3/latest/API/

import gleam/int
import gleam/option.{type Option, None, Some}

// ============================================================
// Types
// ============================================================

pub type Config {
  Config(
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
  )
}

pub type Request {
  Request(
    url: String,
    method: String,
    headers: List(#(String, String)),
    body: String,
  )
}

pub type UploadRequest {
  UploadRequest(
    key: String,
    content_type: String,
    body: String,
  )
}

pub type PresignedUrlRequest {
  PresignedUrlRequest(
    key: String,
    expires_in: Int,
    content_type: Option(String),
  )
}

// ============================================================
// Request Builders
// ============================================================

/// Create a PUT request for uploading an object
pub fn put_object_request(config: Config, req: UploadRequest) -> Request {
  let host = config.bucket <> ".s3." <> config.region <> ".amazonaws.com"
  let url = "https://" <> host <> "/" <> req.key

  Request(
    url: url,
    method: "PUT",
    headers: [
      #("Host", host),
      #("Content-Type", req.content_type),
      #("x-amz-content-sha256", "UNSIGNED-PAYLOAD"),
    ],
    body: req.body,
  )
}

/// Create a GET request for downloading an object
pub fn get_object_request(config: Config, key: String) -> Request {
  let host = config.bucket <> ".s3." <> config.region <> ".amazonaws.com"
  let url = "https://" <> host <> "/" <> key

  Request(
    url: url,
    method: "GET",
    headers: [
      #("Host", host),
      #("x-amz-content-sha256", "UNSIGNED-PAYLOAD"),
    ],
    body: "",
  )
}

/// Create a DELETE request for removing an object
pub fn delete_object_request(config: Config, key: String) -> Request {
  let host = config.bucket <> ".s3." <> config.region <> ".amazonaws.com"
  let url = "https://" <> host <> "/" <> key

  Request(
    url: url,
    method: "DELETE",
    headers: [
      #("Host", host),
      #("x-amz-content-sha256", "UNSIGNED-PAYLOAD"),
    ],
    body: "",
  )
}

/// Create a HEAD request for checking object existence
pub fn head_object_request(config: Config, key: String) -> Request {
  let host = config.bucket <> ".s3." <> config.region <> ".amazonaws.com"
  let url = "https://" <> host <> "/" <> key

  Request(
    url: url,
    method: "HEAD",
    headers: [
      #("Host", host),
      #("x-amz-content-sha256", "UNSIGNED-PAYLOAD"),
    ],
    body: "",
  )
}

/// Create a GET request for listing objects with optional prefix
pub fn list_objects_request(config: Config, prefix: Option(String)) -> Request {
  let host = config.bucket <> ".s3." <> config.region <> ".amazonaws.com"
  let url = case prefix {
    Some(p) -> "https://" <> host <> "/?list-type=2&prefix=" <> p
    None -> "https://" <> host <> "/?list-type=2"
  }

  Request(
    url: url,
    method: "GET",
    headers: [
      #("Host", host),
      #("x-amz-content-sha256", "UNSIGNED-PAYLOAD"),
    ],
    body: "",
  )
}

// ============================================================
// Helper Functions
// ============================================================

/// Generate a presigned URL for temporary access
pub fn generate_presigned_url(config: Config, req: PresignedUrlRequest) -> String {
  let host = config.bucket <> ".s3." <> config.region <> ".amazonaws.com"
  let base_url = "https://" <> host <> "/" <> req.key

  // Note: Full AWS Signature V4 implementation would go here
  // For now, return the base URL - signing should be done in execution layer
  base_url <> "?X-Amz-Expires=" <> int.to_string(req.expires_in)
}

/// Create config with default region
pub fn default_config(
  access_key_id: String,
  secret_access_key: String,
  bucket: String,
) -> Config {
  Config(
    access_key_id: access_key_id,
    secret_access_key: secret_access_key,
    region: "us-east-1",
    bucket: bucket,
  )
}

/// Create config with custom region
pub fn config_with_region(
  access_key_id: String,
  secret_access_key: String,
  bucket: String,
  region: String,
) -> Config {
  Config(
    access_key_id: access_key_id,
    secret_access_key: secret_access_key,
    region: region,
    bucket: bucket,
  )
}

/// Build object URL
pub fn get_object_url(config: Config, key: String) -> String {
  "https://" <> config.bucket <> ".s3." <> config.region <> ".amazonaws.com/" <> key
}

/// Common content types
pub fn content_type_json() -> String {
  "application/json"
}

pub fn content_type_png() -> String {
  "image/png"
}

pub fn content_type_jpeg() -> String {
  "image/jpeg"
}

pub fn content_type_mp3() -> String {
  "audio/mpeg"
}

pub fn content_type_mp4() -> String {
  "video/mp4"
}

pub fn content_type_binary() -> String {
  "application/octet-stream"
}

// ============================================================
// Simple Upload/Download Requests
// ============================================================

/// Simple JSON upload request
pub fn upload_json_request(config: Config, key: String, data: String) -> Request {
  put_object_request(config, UploadRequest(
    key: key,
    content_type: content_type_json(),
    body: data,
  ))
}

/// Simple image upload request
pub fn upload_image_request(
  config: Config,
  key: String,
  content_type: String,
  body: String,
) -> Request {
  put_object_request(config, UploadRequest(
    key: key,
    content_type: content_type,
    body: body,
  ))
}
