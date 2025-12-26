// Instagram Carousel Creator Types
// Migrated from ai-carousel-creator/types.ts

import gleam/option.{type Option}

// ============================================================
// Enums
// ============================================================

pub type AspectRatio {
  Square      // 1:1
  Portrait    // 4:5
  Story       // 9:16
}

pub type OverlayMode {
  Gradient
  Solid
  NoOverlay
}

pub type Language {
  En
  Ru
}

pub type TextAlign {
  Left
  Center
  Right
}

pub type CornerPosition {
  TopLeft
  TopRight
  BottomLeft
  BottomRight
}

pub type VerticalPosition {
  Top
  Bottom
}

pub type ElementType {
  Heading
  Text
  Image
  Sticker
}

pub type FrameType {
  NoFrame
  BrowserLight
  BrowserDark
  Film
  Polaroid
  Paper
  PhotoBooth
}

pub type SlideType {
  Hook    // First slide - attention grabber
  Body    // Middle slides - content
  CTA     // Last slide - call to action
}

// ============================================================
// Position
// ============================================================

pub type Position {
  Position(
    x: Float,  // Percentage 0-100
    y: Float,  // Percentage 0-100
  )
}

// ============================================================
// Element Style (per-element overrides)
// ============================================================

pub type ElementStyle {
  ElementStyle(
    font_size: Option(Int),
    font_family: Option(String),
    text_color: Option(String),
    text_align: Option(TextAlign),
    is_uppercase: Option(Bool),
    opacity: Option(Float),
    filter: Option(String),
  )
}

// ============================================================
// Slide Element
// ============================================================

pub type SlideElement {
  SlideElement(
    id: String,
    element_type: ElementType,
    content: String,           // Text, image URL, or SVG path
    position: Position,
    scale: Float,
    rotation: Float,
    z_index: Int,
    width: Option(Int),        // For images
    locked: Bool,
    style: Option(ElementStyle),
  )
}

// ============================================================
// Slide
// ============================================================

pub type Slide {
  Slide(
    id: String,
    image_url: String,
    image_prompt: String,
    elements: List(SlideElement),
    frame: FrameType,
  )
}

// ============================================================
// Global Settings
// ============================================================

pub type GlobalSettings {
  GlobalSettings(
    // Typography
    font_family: String,
    text_align: TextAlign,
    font_size: Int,
    line_height: Float,
    text_padding: Int,
    is_uppercase: Bool,
    // Colors & Backgrounds
    text_color: String,
    highlight_color: String,
    text_background_color: String,
    text_background_opacity: Int,
    // Overlay / Image Effects
    overlay_mode: OverlayMode,
    overlay_color: String,
    overlay_opacity: Int,
    fade_height: Int,
    // Misc
    corner_radius: Int,
    shadow_intensity: Int,
    show_swipe_arrow: Bool,
    swipe_indicator_position: VerticalPosition,
    art_style: String,
    // Branding
    branding_text: String,
    branding_position: CornerPosition,
  )
}

// ============================================================
// Carousel
// ============================================================

pub type Carousel {
  Carousel(
    id: String,
    user_id: Option(Int),
    topic: String,
    slides: List(Slide),
    settings: GlobalSettings,
    aspect_ratio: AspectRatio,
    language: Language,
    created_at: String,
    updated_at: String,
  )
}

// ============================================================
// Parsed Slide (from LLM)
// ============================================================

pub type ParsedSlide {
  ParsedSlide(
    text: String,
    image_prompt: String,
  )
}

// ============================================================
// Generation Job (for async image generation)
// ============================================================

pub type GenerationStatus {
  Pending
  Processing
  Completed
  Failed(String)
}

pub type GenerationJob {
  GenerationJob(
    id: String,
    carousel_id: String,
    slide_id: String,
    bfl_task_id: Option(String),
    status: GenerationStatus,
    result_url: Option(String),
  )
}

// ============================================================
// Design Palette (preset themes)
// ============================================================

pub type DesignPalette {
  DesignPalette(
    name: String,
    text_color: String,
    highlight_color: String,
    overlay_color: String,
    overlay_mode: OverlayMode,
    overlay_opacity: Int,
    text_background_color: String,
    text_background_opacity: Int,
    font_family: String,
  )
}

// ============================================================
// History Item
// ============================================================

pub type HistoryItem {
  HistoryItem(
    id: String,
    timestamp: Int,
    topic: String,
    slides: List(Slide),
    settings: GlobalSettings,
    aspect_ratio: AspectRatio,
  )
}

// ============================================================
// Default Values
// ============================================================

pub fn default_position() -> Position {
  Position(x: 50.0, y: 60.0)
}

pub fn default_settings() -> GlobalSettings {
  GlobalSettings(
    font_family: "Russo One",
    text_align: Center,
    font_size: 48,
    line_height: 1.2,
    text_padding: 40,
    is_uppercase: True,
    text_color: "#ffffff",
    highlight_color: "#ccff00",
    text_background_color: "#000000",
    text_background_opacity: 0,
    overlay_mode: Gradient,
    overlay_color: "#000000",
    overlay_opacity: 60,
    fade_height: 60,
    corner_radius: 0,
    shadow_intensity: 20,
    show_swipe_arrow: True,
    swipe_indicator_position: Bottom,
    art_style: "PHOTOREALISM",
    branding_text: "",
    branding_position: BottomRight,
  )
}

pub fn default_element_style() -> ElementStyle {
  ElementStyle(
    font_size: option.None,
    font_family: option.None,
    text_color: option.None,
    text_align: option.None,
    is_uppercase: option.None,
    opacity: option.None,
    filter: option.None,
  )
}

// ============================================================
// Aspect Ratio Dimensions
// ============================================================

pub fn aspect_ratio_dimensions(ratio: AspectRatio) -> #(Int, Int) {
  case ratio {
    Square -> #(1080, 1080)
    Portrait -> #(1080, 1350)
    Story -> #(1080, 1920)
  }
}

pub fn aspect_ratio_for_bfl(ratio: AspectRatio) -> #(Int, Int) {
  // BFL recommends specific dimensions for best quality
  case ratio {
    Square -> #(1024, 1024)
    Portrait -> #(864, 1080)
    Story -> #(768, 1344)
  }
}

// ============================================================
// Art Styles
// ============================================================

pub fn art_styles() -> List(#(String, String)) {
  [
    #("PHOTOREALISM", "Ultra-realistic photography, high detail"),
    #("CYBERPUNK", "Neon lights, futuristic, dark atmosphere"),
    #("ACID_GRAPHICS", "Bold colors, abstract shapes, 90s aesthetic"),
    #("3D_ABSTRACT", "3D rendered abstract forms, gradients"),
    #("DARK_NOIR", "High contrast, dramatic shadows, cinematic"),
    #("OIL_PAINTING", "Classical oil painting style, textured"),
    #("MINIMALIST", "Clean, simple, lots of negative space"),
  ]
}

// ============================================================
// Fonts
// ============================================================

pub fn available_fonts() -> List(#(String, String)) {
  [
    #("Russo One", "Bold, viral, attention-grabbing"),
    #("Inter", "Clean, modern UI font"),
    #("Oswald", "Tall, condensed, impactful"),
    #("JetBrains Mono", "Monospace, technical"),
    #("Montserrat", "Geometric, versatile"),
    #("Anton", "Ultra-bold, impact style"),
    #("Rubik", "Rounded, friendly"),
    #("Playfair Display", "Elegant serif"),
  ]
}

// ============================================================
// Preset Palettes
// ============================================================

pub fn preset_palettes() -> List(DesignPalette) {
  [
    DesignPalette(
      name: "ACID MODE",
      text_color: "#ccff00",
      highlight_color: "#ff00ff",
      overlay_color: "#000000",
      overlay_mode: Gradient,
      overlay_opacity: 70,
      text_background_color: "#000000",
      text_background_opacity: 0,
      font_family: "Russo One",
    ),
    DesignPalette(
      name: "HYPEBEAST",
      text_color: "#ffffff",
      highlight_color: "#ff0000",
      overlay_color: "#000000",
      overlay_mode: Solid,
      overlay_opacity: 50,
      text_background_color: "#ff0000",
      text_background_opacity: 100,
      font_family: "Anton",
    ),
    DesignPalette(
      name: "MATRIX",
      text_color: "#00ff00",
      highlight_color: "#ffffff",
      overlay_color: "#001100",
      overlay_mode: Gradient,
      overlay_opacity: 80,
      text_background_color: "#000000",
      text_background_opacity: 0,
      font_family: "JetBrains Mono",
    ),
    DesignPalette(
      name: "VOGUE",
      text_color: "#ffffff",
      highlight_color: "#ff69b4",
      overlay_color: "#000000",
      overlay_mode: NoOverlay,
      overlay_opacity: 0,
      text_background_color: "#000000",
      text_background_opacity: 60,
      font_family: "Playfair Display",
    ),
  ]
}

// ============================================================
// Combo Mode Types (User Photos + LoRA Generation)
// ============================================================

/// Source of slide image
pub type SlideSource {
  UserUploaded     // User uploaded their own photo
  LoraGenerated    // AI generated with LoRA model
}

/// Uploaded photo with optional analysis
pub type UploadedPhoto {
  UploadedPhoto(
    id: String,
    carousel_id: String,
    photo_url: String,
    slide_position: Option(Int),
    analysis: Option(PhotoAnalysis),
    created_at: String,
  )
}

/// Vision LLM analysis of uploaded photo
pub type PhotoAnalysis {
  PhotoAnalysis(
    description: String,   // What's in the photo
    mood: String,          // Emotional tone
    style: String,         // Visual style
    colors: String,        // Dominant colors
    suggested_text: String, // Suggested caption
  )
}

/// Combo slide - can be from photo or generated
pub type ComboSlide {
  ComboSlide(
    position: Int,
    text: String,
    source: SlideSource,
    photo_url: Option(String),    // For UserUploaded
    image_prompt: Option(String), // For LoraGenerated
  )
}

/// Request for combo mode generation
pub type ComboRequest {
  ComboRequest(
    photos: List(UploadedPhoto),
    topic: String,
    total_slides: Int,
    language: Language,
    lora_url: Option(String),
  )
}

/// Extended Slide with source tracking
pub type SlideWithSource {
  SlideWithSource(
    slide: Slide,
    source_type: SlideSource,
    original_photo_url: Option(String),
  )
}
