// B-Roll Video Templates
// Pre-made prompts for generating background footage with AI video generators
// Use with Kling, Veo3/KIE.ai, or other text-to-video models

import gleam/list
import gleam/string

/// B-Roll category types
pub type BRollCategory {
  Nature
  City
  Abstract
  Business
  Technology
  Lifestyle
  Space
  Water
  Fire
  Smoke
  Particles
  Light
  Custom(String)
}

/// B-Roll template with prompt and metadata
pub type BRollTemplate {
  BRollTemplate(
    name: String,
    category: BRollCategory,
    prompt: String,
    style: String,
    loop_friendly: Bool,
    duration_hint: String,
  )
}

// ============================================================
// Nature B-Roll Templates
// ============================================================

pub fn nature_templates() -> List(BRollTemplate) {
  [
    BRollTemplate(
      name: "forest_aerial",
      category: Nature,
      prompt: "Cinematic aerial drone shot flying slowly over a dense green forest, morning mist rising between trees, golden hour sunlight filtering through leaves, 4K, smooth camera movement",
      style: "cinematic",
      loop_friendly: True,
      duration_hint: "5-10s",
    ),
    BRollTemplate(
      name: "ocean_waves",
      category: Nature,
      prompt: "Slow motion ocean waves crashing on sandy beach, crystal clear turquoise water, white foam, sunset golden light, peaceful atmosphere, seamless loop, 4K",
      style: "relaxing",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "mountain_timelapse",
      category: Nature,
      prompt: "Timelapse of clouds moving over majestic snow-capped mountains, dramatic lighting changes, golden to blue hour transition, epic landscape, cinematic",
      style: "epic",
      loop_friendly: False,
      duration_hint: "10s",
    ),
    BRollTemplate(
      name: "rain_window",
      category: Nature,
      prompt: "Close-up of raindrops falling on window glass, blurred city lights in background, cozy mood, soft focus bokeh, seamless loop",
      style: "moody",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "autumn_leaves",
      category: Nature,
      prompt: "Golden autumn leaves falling slowly in sunlight, shallow depth of field, warm colors, peaceful forest background, slow motion",
      style: "warm",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "northern_lights",
      category: Nature,
      prompt: "Aurora borealis dancing across night sky, green and purple lights, stars visible, snowy landscape below, timelapse, 4K",
      style: "magical",
      loop_friendly: True,
      duration_hint: "10s",
    ),
  ]
}

// ============================================================
// City/Urban B-Roll Templates
// ============================================================

pub fn city_templates() -> List(BRollTemplate) {
  [
    BRollTemplate(
      name: "city_night_traffic",
      category: City,
      prompt: "Timelapse of city traffic at night, light trails from cars, neon signs reflection, urban atmosphere, seamless loop, 4K",
      style: "dynamic",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "aerial_skyline",
      category: City,
      prompt: "Smooth aerial shot of modern city skyline at sunset, skyscrapers reflecting golden light, drone flying forward slowly, cinematic",
      style: "corporate",
      loop_friendly: False,
      duration_hint: "10s",
    ),
    BRollTemplate(
      name: "rain_street",
      category: City,
      prompt: "Rainy city street at night, wet pavement reflecting neon lights, puddles, people with umbrellas silhouettes, cinematic noir mood",
      style: "noir",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "subway_motion",
      category: City,
      prompt: "Inside subway train moving through tunnel, windows showing blurred lights, motion blur, urban commute atmosphere",
      style: "urban",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "crowd_walking",
      category: City,
      prompt: "Busy city intersection with crowds of people walking, slow motion, shallow depth of field, urban life, seamless loop",
      style: "documentary",
      loop_friendly: True,
      duration_hint: "5s",
    ),
  ]
}

// ============================================================
// Abstract/VFX B-Roll Templates
// ============================================================

pub fn abstract_templates() -> List(BRollTemplate) {
  [
    BRollTemplate(
      name: "particle_flow",
      category: Particles,
      prompt: "Abstract golden particles flowing in dark space, smooth organic movement, dust motes in light beam, seamless loop, 4K",
      style: "elegant",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "liquid_metal",
      category: Abstract,
      prompt: "Abstract liquid chrome metal morphing and flowing, reflective surface, smooth transitions, dark background, seamless loop",
      style: "futuristic",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "geometric_shapes",
      category: Abstract,
      prompt: "3D geometric shapes rotating slowly in space, clean minimal design, soft shadows, white and gold colors, seamless loop",
      style: "minimal",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "smoke_tendrils",
      category: Smoke,
      prompt: "Slow motion white smoke tendrils curling and flowing on black background, ethereal and mysterious, seamless loop, 4K",
      style: "mysterious",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "fire_flames",
      category: Fire,
      prompt: "Close-up of fire flames burning, orange and blue colors, slow motion, dark background, seamless loop, hypnotic movement",
      style: "intense",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "water_surface",
      category: Water,
      prompt: "Abstract water surface with light reflections, caustics pattern, calm ripples, blue tones, seamless loop, mesmerizing",
      style: "calming",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "light_rays",
      category: Light,
      prompt: "Volumetric light rays through fog, golden sunbeams, dust particles floating, ethereal atmosphere, seamless loop",
      style: "spiritual",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "bokeh_lights",
      category: Light,
      prompt: "Out of focus bokeh lights slowly moving, colorful circles, dreamy atmosphere, black background, seamless loop",
      style: "dreamy",
      loop_friendly: True,
      duration_hint: "5s",
    ),
  ]
}

// ============================================================
// Business/Corporate B-Roll Templates
// ============================================================

pub fn business_templates() -> List(BRollTemplate) {
  [
    BRollTemplate(
      name: "office_workspace",
      category: Business,
      prompt: "Modern minimalist office workspace, laptop on clean desk, natural light from window, plants, professional atmosphere",
      style: "corporate",
      loop_friendly: False,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "typing_hands",
      category: Business,
      prompt: "Close-up of hands typing on laptop keyboard, shallow depth of field, warm office lighting, professional work",
      style: "professional",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "meeting_room",
      category: Business,
      prompt: "Modern glass meeting room, empty conference table, city view through windows, minimalist corporate design",
      style: "corporate",
      loop_friendly: False,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "handshake",
      category: Business,
      prompt: "Professional business handshake, slow motion, bright office background, partnership concept, corporate style",
      style: "professional",
      loop_friendly: False,
      duration_hint: "5s",
    ),
  ]
}

// ============================================================
// Technology B-Roll Templates
// ============================================================

pub fn technology_templates() -> List(BRollTemplate) {
  [
    BRollTemplate(
      name: "circuit_board",
      category: Technology,
      prompt: "Macro shot of circuit board with electrical signals traveling through traces, blue glow, futuristic tech visualization",
      style: "tech",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "data_center",
      category: Technology,
      prompt: "Walking through dark data center corridor, server racks with blinking lights, blue LED glow, tech atmosphere",
      style: "futuristic",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "hologram_ui",
      category: Technology,
      prompt: "Futuristic holographic user interface floating in dark space, blue transparent UI elements, sci-fi style, seamless loop",
      style: "sci-fi",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "binary_code",
      category: Technology,
      prompt: "Matrix-style binary code falling down screen, green characters on black, digital rain effect, seamless loop",
      style: "hacker",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "robot_arm",
      category: Technology,
      prompt: "Industrial robot arm moving precisely, manufacturing automation, sparks flying, blue lighting, tech factory",
      style: "industrial",
      loop_friendly: True,
      duration_hint: "5s",
    ),
  ]
}

// ============================================================
// Space/Cosmic B-Roll Templates
// ============================================================

pub fn space_templates() -> List(BRollTemplate) {
  [
    BRollTemplate(
      name: "stars_timelapse",
      category: Space,
      prompt: "Timelapse of night sky with stars rotating, milky way visible, dark landscape silhouette, long exposure effect",
      style: "cosmic",
      loop_friendly: True,
      duration_hint: "10s",
    ),
    BRollTemplate(
      name: "nebula_flight",
      category: Space,
      prompt: "Flying through colorful space nebula, purple and blue cosmic clouds, stars everywhere, cinematic space journey",
      style: "epic",
      loop_friendly: True,
      duration_hint: "10s",
    ),
    BRollTemplate(
      name: "planet_orbit",
      category: Space,
      prompt: "Earth-like planet slowly rotating in space, atmosphere glow, stars in background, cinematic space view",
      style: "cinematic",
      loop_friendly: True,
      duration_hint: "10s",
    ),
    BRollTemplate(
      name: "asteroid_field",
      category: Space,
      prompt: "Flying through asteroid field in space, rocks tumbling slowly, distant sun light, cinematic depth",
      style: "adventure",
      loop_friendly: True,
      duration_hint: "10s",
    ),
  ]
}

// ============================================================
// Lifestyle B-Roll Templates
// ============================================================

pub fn lifestyle_templates() -> List(BRollTemplate) {
  [
    BRollTemplate(
      name: "coffee_steam",
      category: Lifestyle,
      prompt: "Close-up of hot coffee cup with steam rising, morning light, cozy atmosphere, shallow depth of field, seamless loop",
      style: "cozy",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "yoga_sunset",
      category: Lifestyle,
      prompt: "Silhouette of person doing yoga pose at sunset on beach, peaceful atmosphere, golden light, wellness concept",
      style: "wellness",
      loop_friendly: False,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "cooking_prep",
      category: Lifestyle,
      prompt: "Close-up of hands chopping fresh vegetables, kitchen background, natural light, food preparation, slow motion",
      style: "culinary",
      loop_friendly: True,
      duration_hint: "5s",
    ),
    BRollTemplate(
      name: "reading_book",
      category: Lifestyle,
      prompt: "Hands turning pages of old book, warm lamp light, cozy reading nook, shallow depth of field, relaxing mood",
      style: "cozy",
      loop_friendly: True,
      duration_hint: "5s",
    ),
  ]
}

// ============================================================
// Helper Functions
// ============================================================

/// Get all templates
pub fn all_templates() -> List(BRollTemplate) {
  list.flatten([
    nature_templates(),
    city_templates(),
    abstract_templates(),
    business_templates(),
    technology_templates(),
    space_templates(),
    lifestyle_templates(),
  ])
}

/// Get templates by category
pub fn get_by_category(category: BRollCategory) -> List(BRollTemplate) {
  all_templates()
  |> list.filter(fn(t) { t.category == category })
}

/// Get loop-friendly templates only
pub fn get_loopable() -> List(BRollTemplate) {
  all_templates()
  |> list.filter(fn(t) { t.loop_friendly })
}

/// Find template by name
pub fn find_by_name(name: String) -> Result(BRollTemplate, Nil) {
  all_templates()
  |> list.find(fn(t) { t.name == name })
}

/// Get random template from category
pub fn random_from_category(category: BRollCategory, seed: Int) -> Result(BRollTemplate, Nil) {
  let templates = get_by_category(category)
  case list.length(templates) {
    0 -> Error(Nil)
    len -> {
      let index = seed % len
      list.drop(templates, index)
      |> list.first
    }
  }
}

/// Category to string
pub fn category_to_string(category: BRollCategory) -> String {
  case category {
    Nature -> "nature"
    City -> "city"
    Abstract -> "abstract"
    Business -> "business"
    Technology -> "technology"
    Lifestyle -> "lifestyle"
    Space -> "space"
    Water -> "water"
    Fire -> "fire"
    Smoke -> "smoke"
    Particles -> "particles"
    Light -> "light"
    Custom(name) -> "custom:" <> name
  }
}

/// String to category
pub fn string_to_category(s: String) -> BRollCategory {
  case string.lowercase(s) {
    "nature" -> Nature
    "city" -> City
    "urban" -> City
    "abstract" -> Abstract
    "business" -> Business
    "corporate" -> Business
    "technology" -> Technology
    "tech" -> Technology
    "lifestyle" -> Lifestyle
    "space" -> Space
    "cosmic" -> Space
    "water" -> Water
    "fire" -> Fire
    "smoke" -> Smoke
    "particles" -> Particles
    "light" -> Light
    other -> Custom(other)
  }
}

/// Enhance prompt with additional quality modifiers
pub fn enhance_prompt(template: BRollTemplate, extra_modifiers: List(String)) -> String {
  let base = template.prompt
  let modifiers = string.join(extra_modifiers, ", ")
  case modifiers {
    "" -> base
    mods -> base <> ", " <> mods
  }
}

/// Get prompt optimized for specific AI model
pub fn get_model_prompt(template: BRollTemplate, model: String) -> String {
  case model {
    "kling" -> template.prompt <> ", Kling AI video generation, high quality"
    "veo3" | "kieai" -> template.prompt <> ", photorealistic, Veo 3 style"
    "runway" -> template.prompt <> ", cinematic, professional video"
    _ -> template.prompt
  }
}
