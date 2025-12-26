// Carousel Lustre App - Main TEA Application
// Model-View-Update architecture for Instagram Carousel Creator

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre
import lustre/attribute.{class, id}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import vibee/carousel/types.{
  type AspectRatio, type Carousel, type DesignPalette, type GlobalSettings,
  type Slide, type SlideElement, Portrait,
}

// ============================================================
// Helper for inline styles
// ============================================================

/// Helper to create inline styles from a list of tuples
fn inline_styles(styles: List(#(String, String))) -> attribute.Attribute(a) {
  let style_string =
    styles
    |> list.map(fn(pair) { pair.0 <> ": " <> pair.1 })
    |> string.join("; ")
  attribute.attribute("style", style_string)
}

// ============================================================
// Model
// ============================================================

pub type Model {
  Model(
    // Carousel data
    carousel_id: Option(String),
    slides: List(Slide),
    settings: GlobalSettings,
    aspect_ratio: AspectRatio,
    // Selection state
    active_slide_index: Int,
    active_element_id: Option(String),
    // UI state
    is_loading: Bool,
    is_generating: Bool,
    error: Option(String),
    success_message: Option(String),
    // Input state
    topic_input: String,
    // Panel state
    active_panel: PanelType,
    // History for undo
    history: List(HistoryState),
    history_index: Int,
  )
}

pub type PanelType {
  SettingsPanel
  TextPanel
  ImagesPanel
  ExportPanel
}

pub type HistoryState {
  HistoryState(slides: List(Slide), settings: GlobalSettings)
}

// ============================================================
// Msg (Messages)
// ============================================================

pub type Msg {
  // Navigation
  SelectSlide(Int)
  SelectElement(String)
  DeselectElement
  // Slide management
  AddSlide
  DeleteSlide(Int)
  DuplicateSlide(Int)
  MoveSlide(Int, Int)
  // Content generation
  SetTopicInput(String)
  GenerateCarousel
  GenerationComplete(Result(List(Slide), String))
  RegenerateImage(Int)
  ImageRegenerated(Int, Result(String, String))
  MakeTextViral(Int, String)
  TextViralComplete(Int, String, Result(String, String))
  // Element editing
  UpdateElementContent(String, String)
  UpdateElementPosition(String, Float, Float)
  UpdateElementScale(String, Float)
  UpdateElementRotation(String, Float)
  DeleteElement(String)
  AddTextElement
  // Settings
  UpdateFontFamily(String)
  UpdateFontSize(Int)
  UpdateTextColor(String)
  UpdateHighlightColor(String)
  UpdateOverlayOpacity(Int)
  UpdateArtStyle(String)
  ApplyPalette(DesignPalette)
  SetAspectRatio(AspectRatio)
  // Panels
  SwitchPanel(PanelType)
  // History
  Undo
  Redo
  // Export
  ExportZip
  ExportPdf
  PublishTelegram
  ExportComplete(Result(String, String))
  // UI
  DismissError
  DismissSuccess
  // API responses
  CarouselLoaded(Result(Carousel, String))
  CarouselSaved(Result(Nil, String))
  // No-op
  NoOp
}

// ============================================================
// Init
// ============================================================

pub fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      carousel_id: None,
      slides: [],
      settings: types.default_settings(),
      aspect_ratio: Portrait,
      active_slide_index: 0,
      active_element_id: None,
      is_loading: False,
      is_generating: False,
      error: None,
      success_message: None,
      topic_input: "",
      active_panel: SettingsPanel,
      history: [],
      history_index: -1,
    )

  #(model, effect.none())
}

// ============================================================
// Update
// ============================================================

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    // Navigation
    SelectSlide(index) -> #(
      Model(..model, active_slide_index: index, active_element_id: None),
      effect.none(),
    )

    SelectElement(id) -> #(
      Model(..model, active_element_id: Some(id)),
      effect.none(),
    )

    DeselectElement -> #(Model(..model, active_element_id: None), effect.none())

    // Slide management
    AddSlide -> {
      let new_slide = create_empty_slide(list.length(model.slides))
      let new_slides = list.append(model.slides, [new_slide])
      #(
        save_history(
          Model(
            ..model,
            slides: new_slides,
            active_slide_index: list.length(new_slides) - 1,
          ),
        ),
        effect.none(),
      )
    }

    DeleteSlide(index) -> {
      let new_slides =
        model.slides
        |> list.index_map(fn(s, i) { #(s, i) })
        |> list.filter(fn(pair) { pair.1 != index })
        |> list.map(fn(pair) { pair.0 })

      let new_index = case model.active_slide_index >= list.length(new_slides) {
        True -> list.length(new_slides) - 1
        False -> model.active_slide_index
      }
      |> max(0)

      #(
        save_history(
          Model(..model, slides: new_slides, active_slide_index: new_index),
        ),
        effect.none(),
      )
    }

    DuplicateSlide(index) -> {
      case get_at(model.slides, index) {
        Some(slide) -> {
          let new_slide =
            types.Slide(
              ..slide,
              id: generate_id("slide"),
              elements: list.map(slide.elements, fn(el) {
                types.SlideElement(..el, id: generate_id("el"))
              }),
            )
          let before = list.take(model.slides, index + 1)
          let after = list.drop(model.slides, index + 1)
          let new_slides = list.flatten([before, [new_slide], after])
          #(
            save_history(
              Model(..model, slides: new_slides, active_slide_index: index + 1),
            ),
            effect.none(),
          )
        }
        None -> #(model, effect.none())
      }
    }

    MoveSlide(from, to) -> {
      let new_slides = move_item(model.slides, from, to)
      #(save_history(Model(..model, slides: new_slides)), effect.none())
    }

    // Content generation
    SetTopicInput(text) -> #(Model(..model, topic_input: text), effect.none())

    GenerateCarousel -> {
      case model.topic_input {
        "" -> #(
          Model(..model, error: Some("Please enter a topic")),
          effect.none(),
        )
        _ -> #(
          Model(..model, is_generating: True, error: None),
          // Effect would trigger API call
          effect.none(),
        )
      }
    }

    GenerationComplete(result) -> {
      case result {
        Ok(slides) -> #(
          save_history(
            Model(
              ..model,
              slides: slides,
              is_generating: False,
              success_message: Some("Carousel generated!"),
            ),
          ),
          effect.none(),
        )
        Error(err) -> #(
          Model(..model, is_generating: False, error: Some(err)),
          effect.none(),
        )
      }
    }

    RegenerateImage(slide_index) -> #(
      Model(..model, is_loading: True),
      effect.none(),
    )

    ImageRegenerated(slide_index, result) -> {
      case result {
        Ok(new_url) -> {
          let new_slides =
            update_slide_at(model.slides, slide_index, fn(slide) {
              types.Slide(..slide, image_url: new_url)
            })
          #(
            save_history(Model(..model, slides: new_slides, is_loading: False)),
            effect.none(),
          )
        }
        Error(err) -> #(
          Model(..model, is_loading: False, error: Some(err)),
          effect.none(),
        )
      }
    }

    MakeTextViral(_slide_index, _element_id) -> #(
      Model(..model, is_loading: True),
      effect.none(),
    )

    TextViralComplete(slide_index, element_id, result) -> {
      case result {
        Ok(new_text) -> {
          let new_slides =
            update_element_in_slide(
              model.slides,
              slide_index,
              element_id,
              fn(el) { types.SlideElement(..el, content: new_text) },
            )
          #(
            save_history(Model(..model, slides: new_slides, is_loading: False)),
            effect.none(),
          )
        }
        Error(err) -> #(
          Model(..model, is_loading: False, error: Some(err)),
          effect.none(),
        )
      }
    }

    // Element editing
    UpdateElementContent(element_id, content) -> {
      let new_slides =
        update_element_in_slide(
          model.slides,
          model.active_slide_index,
          element_id,
          fn(el) { types.SlideElement(..el, content: content) },
        )
      #(Model(..model, slides: new_slides), effect.none())
    }

    UpdateElementPosition(element_id, x, y) -> {
      let new_slides =
        update_element_in_slide(
          model.slides,
          model.active_slide_index,
          element_id,
          fn(el) {
            types.SlideElement(..el, position: types.Position(x: x, y: y))
          },
        )
      #(Model(..model, slides: new_slides), effect.none())
    }

    UpdateElementScale(element_id, scale) -> {
      let new_slides =
        update_element_in_slide(
          model.slides,
          model.active_slide_index,
          element_id,
          fn(el) { types.SlideElement(..el, scale: scale) },
        )
      #(Model(..model, slides: new_slides), effect.none())
    }

    UpdateElementRotation(element_id, rotation) -> {
      let new_slides =
        update_element_in_slide(
          model.slides,
          model.active_slide_index,
          element_id,
          fn(el) { types.SlideElement(..el, rotation: rotation) },
        )
      #(Model(..model, slides: new_slides), effect.none())
    }

    DeleteElement(element_id) -> {
      let new_slides =
        update_slide_at(model.slides, model.active_slide_index, fn(slide) {
          types.Slide(
            ..slide,
            elements: list.filter(slide.elements, fn(el) { el.id != element_id }),
          )
        })
      #(
        save_history(Model(..model, slides: new_slides, active_element_id: None)),
        effect.none(),
      )
    }

    AddTextElement -> {
      let new_element =
        types.SlideElement(
          id: generate_id("el"),
          element_type: types.Text,
          content: "New text",
          position: types.Position(x: 50.0, y: 50.0),
          scale: 1.0,
          rotation: 0.0,
          z_index: 10,
          width: None,
          locked: False,
          style: None,
        )
      let new_slides =
        update_slide_at(model.slides, model.active_slide_index, fn(slide) {
          types.Slide(
            ..slide,
            elements: list.append(slide.elements, [new_element]),
          )
        })
      #(
        save_history(
          Model(..model, slides: new_slides, active_element_id: Some(new_element.id)),
        ),
        effect.none(),
      )
    }

    // Settings
    UpdateFontFamily(font) -> {
      let new_settings = types.GlobalSettings(..model.settings, font_family: font)
      #(
        save_history(Model(..model, settings: new_settings)),
        effect.none(),
      )
    }

    UpdateFontSize(size) -> {
      let new_settings = types.GlobalSettings(..model.settings, font_size: size)
      #(
        save_history(Model(..model, settings: new_settings)),
        effect.none(),
      )
    }

    UpdateTextColor(color) -> {
      let new_settings = types.GlobalSettings(..model.settings, text_color: color)
      #(
        save_history(Model(..model, settings: new_settings)),
        effect.none(),
      )
    }

    UpdateHighlightColor(color) -> {
      let new_settings =
        types.GlobalSettings(..model.settings, highlight_color: color)
      #(
        save_history(Model(..model, settings: new_settings)),
        effect.none(),
      )
    }

    UpdateOverlayOpacity(opacity) -> {
      let new_settings =
        types.GlobalSettings(..model.settings, overlay_opacity: opacity)
      #(Model(..model, settings: new_settings), effect.none())
    }

    UpdateArtStyle(style_name) -> {
      let new_settings = types.GlobalSettings(..model.settings, art_style: style_name)
      #(
        save_history(Model(..model, settings: new_settings)),
        effect.none(),
      )
    }

    ApplyPalette(palette) -> {
      let new_settings =
        types.GlobalSettings(
          ..model.settings,
          text_color: palette.text_color,
          highlight_color: palette.highlight_color,
          overlay_color: palette.overlay_color,
          overlay_mode: palette.overlay_mode,
          overlay_opacity: palette.overlay_opacity,
          text_background_color: palette.text_background_color,
          text_background_opacity: palette.text_background_opacity,
          font_family: palette.font_family,
        )
      #(
        save_history(Model(..model, settings: new_settings)),
        effect.none(),
      )
    }

    SetAspectRatio(ratio) -> #(
      save_history(Model(..model, aspect_ratio: ratio)),
      effect.none(),
    )

    // Panels
    SwitchPanel(panel) -> #(Model(..model, active_panel: panel), effect.none())

    // History
    Undo -> {
      case model.history_index > 0 {
        True -> {
          let new_index = model.history_index - 1
          case get_at(model.history, new_index) {
            Some(state) -> #(
              Model(
                ..model,
                slides: state.slides,
                settings: state.settings,
                history_index: new_index,
              ),
              effect.none(),
            )
            None -> #(model, effect.none())
          }
        }
        False -> #(model, effect.none())
      }
    }

    Redo -> {
      case model.history_index < list.length(model.history) - 1 {
        True -> {
          let new_index = model.history_index + 1
          case get_at(model.history, new_index) {
            Some(state) -> #(
              Model(
                ..model,
                slides: state.slides,
                settings: state.settings,
                history_index: new_index,
              ),
              effect.none(),
            )
            None -> #(model, effect.none())
          }
        }
        False -> #(model, effect.none())
      }
    }

    // Export
    ExportZip | ExportPdf | PublishTelegram -> #(
      Model(..model, is_loading: True),
      effect.none(),
    )

    ExportComplete(result) -> {
      case result {
        Ok(url) -> #(
          Model(
            ..model,
            is_loading: False,
            success_message: Some("Export complete: " <> url),
          ),
          effect.none(),
        )
        Error(err) -> #(
          Model(..model, is_loading: False, error: Some(err)),
          effect.none(),
        )
      }
    }

    // UI
    DismissError -> #(Model(..model, error: None), effect.none())
    DismissSuccess -> #(Model(..model, success_message: None), effect.none())

    // API
    CarouselLoaded(result) -> {
      case result {
        Ok(carousel) -> #(
          Model(
            ..model,
            carousel_id: Some(carousel.id),
            slides: carousel.slides,
            settings: carousel.settings,
            aspect_ratio: carousel.aspect_ratio,
            is_loading: False,
          ),
          effect.none(),
        )
        Error(err) -> #(
          Model(..model, is_loading: False, error: Some(err)),
          effect.none(),
        )
      }
    }

    CarouselSaved(result) -> {
      case result {
        Ok(_) -> #(
          Model(..model, success_message: Some("Saved!")),
          effect.none(),
        )
        Error(err) -> #(Model(..model, error: Some(err)), effect.none())
      }
    }

    NoOp -> #(model, effect.none())
  }
}

// ============================================================
// View
// ============================================================

pub fn view(model: Model) -> Element(Msg) {
  html.div([class("carousel-app")], [
    // Header
    view_header(model),
    // Main content
    html.div([class("carousel-main")], [
      // Left: Slide strip
      view_slide_strip(model),
      // Center: Canvas
      view_canvas(model),
      // Right: Control panel
      view_control_panel(model),
    ]),
    // Notifications
    view_notifications(model),
    // Loading overlay
    view_loading_overlay(model),
  ])
}

fn view_header(model: Model) -> Element(Msg) {
  html.header([class("carousel-header")], [
    html.div([class("header-left")], [
      html.h1([], [html.text("Carousel Creator")]),
    ]),
    html.div([class("header-center")], [
      html.input([
        class("topic-input"),
        attribute.placeholder("Enter topic for AI generation..."),
        attribute.value(model.topic_input),
        event.on_input(SetTopicInput),
      ]),
      html.button(
        [
          class("generate-btn"),
          event.on_click(GenerateCarousel),
          attribute.disabled(model.is_generating),
        ],
        [
          html.text(case model.is_generating {
            True -> "Generating..."
            False -> "Generate"
          }),
        ],
      ),
    ]),
    html.div([class("header-right")], [
      html.button([class("btn-undo"), event.on_click(Undo)], [html.text("Undo")]),
      html.button([class("btn-redo"), event.on_click(Redo)], [html.text("Redo")]),
      html.button([class("btn-export"), event.on_click(ExportZip)], [
        html.text("Export"),
      ]),
    ]),
  ])
}

fn view_slide_strip(model: Model) -> Element(Msg) {
  html.aside([class("slide-strip")], [
    html.div(
      [class("slides-container")],
      list.index_map(model.slides, fn(slide, index) {
        view_slide_thumbnail(slide, index, model.active_slide_index == index)
      }),
    ),
    html.button([class("add-slide-btn"), event.on_click(AddSlide)], [
      html.text("+ Add Slide"),
    ]),
  ])
}

fn view_slide_thumbnail(
  slide: Slide,
  index: Int,
  is_active: Bool,
) -> Element(Msg) {
  let active_class = case is_active {
    True -> " active"
    False -> ""
  }

  html.div(
    [class("slide-thumbnail" <> active_class), event.on_click(SelectSlide(index))],
    [
      html.div(
        [
          class("thumbnail-image"),
          inline_styles([
            #(
              "background-image",
              "url(" <> slide.image_url <> ")",
            ),
          ]),
        ],
        [],
      ),
      html.span([class("slide-number")], [
        html.text(int.to_string(index + 1)),
      ]),
    ],
  )
}

fn view_canvas(model: Model) -> Element(Msg) {
  let #(width, height) = types.aspect_ratio_dimensions(model.aspect_ratio)

  html.main([class("carousel-canvas-container")], [
    html.div(
      [
        id("carousel-canvas"),
        class("carousel-canvas"),
        inline_styles([
          #("aspect-ratio", int.to_string(width) <> "/" <> int.to_string(height)),
        ]),
        event.on_click(DeselectElement),
      ],
      case get_at(model.slides, model.active_slide_index) {
        Some(slide) -> [view_slide_content(slide, model)]
        None -> [view_empty_canvas()]
      },
    ),
  ])
}

fn view_slide_content(slide: Slide, model: Model) -> Element(Msg) {
  html.div([class("slide-content")], [
    // Background image
    html.div(
      [
        class("slide-background"),
        inline_styles([#("background-image", "url(" <> slide.image_url <> ")")]),
      ],
      [],
    ),
    // Overlay
    view_overlay(model.settings),
    // Elements
    html.div(
      [class("slide-elements")],
      list.map(slide.elements, fn(el) {
        view_element(el, model.active_element_id, model.settings)
      }),
    ),
  ])
}

fn view_overlay(settings: GlobalSettings) -> Element(Msg) {
  case settings.overlay_mode {
    types.NoOverlay -> html.div([], [])
    types.Solid ->
      html.div(
        [
          class("slide-overlay solid"),
          inline_styles([
            #("background-color", settings.overlay_color),
            #(
              "opacity",
              float.to_string(int.to_float(settings.overlay_opacity) /. 100.0),
            ),
          ]),
        ],
        [],
      )
    types.Gradient ->
      html.div(
        [
          class("slide-overlay gradient"),
          inline_styles([
            #(
              "background",
              "linear-gradient(to top, "
                <> settings.overlay_color
                <> " 0%, transparent "
                <> int.to_string(settings.fade_height)
                <> "%)",
            ),
            #(
              "opacity",
              float.to_string(int.to_float(settings.overlay_opacity) /. 100.0),
            ),
          ]),
        ],
        [],
      )
  }
}

fn view_element(
  el: SlideElement,
  active_id: Option(String),
  settings: GlobalSettings,
) -> Element(Msg) {
  let is_selected = case active_id {
    Some(id) -> id == el.id
    None -> False
  }

  let selected_class = case is_selected {
    True -> " selected"
    False -> ""
  }

  html.div(
    [
      class("slide-element" <> selected_class),
      id(el.id),
      inline_styles([
        #("left", float.to_string(el.position.x) <> "%"),
        #("top", float.to_string(el.position.y) <> "%"),
        #("transform", build_transform(el.scale, el.rotation)),
        #("z-index", int.to_string(el.z_index)),
      ]),
      event.on_click(SelectElement(el.id)),
    ],
    [
      case el.element_type {
        types.Heading | types.Text ->
          html.div(
            [
              class("element-text"),
              inline_styles([
                #("font-family", settings.font_family),
                #("font-size", int.to_string(settings.font_size) <> "px"),
                #("color", settings.text_color),
              ]),
            ],
            [element.unsafe_raw_html("", "span", [], el.content)],
          )
        types.Image ->
          html.img([
            class("element-image"),
            attribute.src(el.content),
            inline_styles([#("width", int.to_string(option.unwrap(el.width, 100)) <> "px")]),
          ])
        types.Sticker ->
          html.img([class("element-sticker"), attribute.src(el.content)])
      },
    ],
  )
}

fn view_empty_canvas() -> Element(Msg) {
  html.div([class("empty-canvas")], [
    html.p([], [html.text("Enter a topic and click Generate")]),
    html.p([], [html.text("or add slides manually")]),
  ])
}

fn view_control_panel(model: Model) -> Element(Msg) {
  html.aside([class("control-panel")], [
    // Panel tabs
    html.div([class("panel-tabs")], [
      view_panel_tab("Settings", SettingsPanel, model.active_panel),
      view_panel_tab("Text", TextPanel, model.active_panel),
      view_panel_tab("Images", ImagesPanel, model.active_panel),
      view_panel_tab("Export", ExportPanel, model.active_panel),
    ]),
    // Panel content
    html.div([class("panel-content")], [
      case model.active_panel {
        SettingsPanel -> view_settings_panel(model)
        TextPanel -> view_text_panel(model)
        ImagesPanel -> view_images_panel(model)
        ExportPanel -> view_export_panel(model)
      },
    ]),
  ])
}

fn view_panel_tab(label: String, panel: PanelType, active: PanelType) -> Element(Msg) {
  let active_class = case panel == active {
    True -> " active"
    False -> ""
  }
  html.button([class("panel-tab" <> active_class), event.on_click(SwitchPanel(panel))], [
    html.text(label),
  ])
}

fn view_settings_panel(model: Model) -> Element(Msg) {
  html.div([class("settings-panel")], [
    // Aspect ratio
    html.div([class("setting-group")], [
      html.label([], [html.text("Aspect Ratio")]),
      html.div([class("ratio-buttons")], [
        view_ratio_button("1:1", types.Square, model.aspect_ratio),
        view_ratio_button("4:5", types.Portrait, model.aspect_ratio),
        view_ratio_button("9:16", types.Story, model.aspect_ratio),
      ]),
    ]),
    // Font family
    html.div([class("setting-group")], [
      html.label([], [html.text("Font")]),
      html.select(
        [event.on_input(UpdateFontFamily)],
        list.map(types.available_fonts(), fn(font) {
          let #(name, _desc) = font
          html.option(
            [
              attribute.value(name),
              attribute.selected(model.settings.font_family == name),
            ],
            name,
          )
        }),
      ),
    ]),
    // Font size
    html.div([class("setting-group")], [
      html.label([], [
        html.text("Size: " <> int.to_string(model.settings.font_size) <> "px"),
      ]),
      html.input([
        attribute.type_("range"),
        attribute.min("24"),
        attribute.max("96"),
        attribute.value(int.to_string(model.settings.font_size)),
        event.on_input(fn(v) { UpdateFontSize(parse_int(v)) }),
      ]),
    ]),
    // Colors
    html.div([class("setting-group")], [
      html.label([], [html.text("Text Color")]),
      html.input([
        attribute.type_("color"),
        attribute.value(model.settings.text_color),
        event.on_input(UpdateTextColor),
      ]),
    ]),
    html.div([class("setting-group")], [
      html.label([], [html.text("Highlight Color")]),
      html.input([
        attribute.type_("color"),
        attribute.value(model.settings.highlight_color),
        event.on_input(UpdateHighlightColor),
      ]),
    ]),
    // Overlay opacity
    html.div([class("setting-group")], [
      html.label([], [
        html.text(
          "Overlay: " <> int.to_string(model.settings.overlay_opacity) <> "%",
        ),
      ]),
      html.input([
        attribute.type_("range"),
        attribute.min("0"),
        attribute.max("100"),
        attribute.value(int.to_string(model.settings.overlay_opacity)),
        event.on_input(fn(v) { UpdateOverlayOpacity(parse_int(v)) }),
      ]),
    ]),
    // Palettes
    html.div([class("setting-group")], [
      html.label([], [html.text("Presets")]),
      html.div(
        [class("palette-grid")],
        list.map(types.preset_palettes(), fn(palette) {
          html.button(
            [
              class("palette-btn"),
              inline_styles([
                #("background-color", palette.text_color),
                #("border-color", palette.highlight_color),
              ]),
              event.on_click(ApplyPalette(palette)),
            ],
            [html.text(palette.name)],
          )
        }),
      ),
    ]),
  ])
}

fn view_ratio_button(
  label: String,
  ratio: AspectRatio,
  current: AspectRatio,
) -> Element(Msg) {
  let active_class = case ratio == current {
    True -> " active"
    False -> ""
  }
  html.button(
    [class("ratio-btn" <> active_class), event.on_click(SetAspectRatio(ratio))],
    [html.text(label)],
  )
}

fn view_text_panel(model: Model) -> Element(Msg) {
  html.div([class("text-panel")], [
    html.button([class("btn-primary"), event.on_click(AddTextElement)], [
      html.text("+ Add Text"),
    ]),
    case model.active_element_id {
      Some(id) -> view_element_editor(model, id)
      None ->
        html.p([class("hint")], [html.text("Select an element to edit")])
    },
  ])
}

fn view_element_editor(model: Model, element_id: String) -> Element(Msg) {
  case find_element(model.slides, model.active_slide_index, element_id) {
    Some(el) ->
      html.div([class("element-editor")], [
        html.div([class("setting-group")], [
          html.label([], [html.text("Content")]),
          html.textarea(
            [
              class("content-input"),
              event.on_input(fn(v) { UpdateElementContent(element_id, v) }),
            ],
            el.content,
          ),
        ]),
        html.div([class("setting-group")], [
          html.label([], [
            html.text("Scale: " <> float.to_string(el.scale)),
          ]),
          html.input([
            attribute.type_("range"),
            attribute.min("0.5"),
            attribute.max("3"),
            attribute.step("0.1"),
            attribute.value(float.to_string(el.scale)),
            event.on_input(fn(v) {
              UpdateElementScale(element_id, parse_float(v))
            }),
          ]),
        ]),
        html.div([class("setting-group")], [
          html.label([], [
            html.text("Rotation: " <> float.to_string(el.rotation) <> "deg"),
          ]),
          html.input([
            attribute.type_("range"),
            attribute.min("-180"),
            attribute.max("180"),
            attribute.value(float.to_string(el.rotation)),
            event.on_input(fn(v) {
              UpdateElementRotation(element_id, parse_float(v))
            }),
          ]),
        ]),
        html.button(
          [class("btn-danger"), event.on_click(DeleteElement(element_id))],
          [html.text("Delete Element")],
        ),
      ])
    None -> html.div([], [])
  }
}

fn view_images_panel(model: Model) -> Element(Msg) {
  html.div([class("images-panel")], [
    // Art style selector
    html.div([class("setting-group")], [
      html.label([], [html.text("Art Style")]),
      html.select(
        [event.on_input(UpdateArtStyle)],
        list.map(types.art_styles(), fn(style_pair) {
          let #(name, _desc) = style_pair
          html.option(
            [
              attribute.value(name),
              attribute.selected(model.settings.art_style == name),
            ],
            name,
          )
        }),
      ),
    ]),
    // Regenerate button
    html.button(
      [
        class("btn-primary"),
        event.on_click(RegenerateImage(model.active_slide_index)),
        attribute.disabled(list.is_empty(model.slides)),
      ],
      [html.text("Regenerate Image")],
    ),
    // Make viral button
    html.button(
      [
        class("btn-secondary"),
        event.on_click(case model.active_element_id {
          Some(id) -> MakeTextViral(model.active_slide_index, id)
          None -> NoOp
        }),
        attribute.disabled(option.is_none(model.active_element_id)),
      ],
      [html.text("Make Text Viral")],
    ),
  ])
}

fn view_export_panel(_model: Model) -> Element(Msg) {
  html.div([class("export-panel")], [
    html.button([class("btn-primary btn-block"), event.on_click(ExportZip)], [
      html.text("Export as ZIP"),
    ]),
    html.button([class("btn-secondary btn-block"), event.on_click(ExportPdf)], [
      html.text("Export as PDF"),
    ]),
    html.hr([]),
    html.button(
      [class("btn-telegram btn-block"), event.on_click(PublishTelegram)],
      [html.text("Publish to Telegram")],
    ),
  ])
}

fn view_notifications(model: Model) -> Element(Msg) {
  html.div([class("notifications")], [
    case model.error {
      Some(err) ->
        html.div([class("notification error"), event.on_click(DismissError)], [
          html.text(err),
        ])
      None -> html.div([], [])
    },
    case model.success_message {
      Some(msg) ->
        html.div([class("notification success"), event.on_click(DismissSuccess)], [
          html.text(msg),
        ])
      None -> html.div([], [])
    },
  ])
}

fn view_loading_overlay(model: Model) -> Element(Msg) {
  case model.is_loading || model.is_generating {
    True ->
      html.div([class("loading-overlay")], [
        html.div([class("spinner")], []),
        html.text(case model.is_generating {
          True -> "Generating carousel..."
          False -> "Loading..."
        }),
      ])
    False -> html.div([], [])
  }
}

// ============================================================
// Helper Functions
// ============================================================

fn save_history(model: Model) -> Model {
  let state = HistoryState(slides: model.slides, settings: model.settings)
  let history_before = list.take(model.history, model.history_index + 1)
  let new_history = list.append(history_before, [state])
  Model(..model, history: new_history, history_index: list.length(new_history) - 1)
}

fn create_empty_slide(index: Int) -> Slide {
  types.Slide(
    id: generate_id("slide"),
    image_url: "",
    image_prompt: "",
    elements: [
      types.SlideElement(
        id: generate_id("el"),
        element_type: types.Heading,
        content: "Slide " <> int.to_string(index + 1),
        position: types.Position(x: 50.0, y: 50.0),
        scale: 1.0,
        rotation: 0.0,
        z_index: 1,
        width: None,
        locked: False,
        style: None,
      ),
    ],
    frame: types.NoFrame,
  )
}

fn generate_id(prefix: String) -> String {
  prefix <> "_" <> random_string()
}

fn update_slide_at(
  slides: List(Slide),
  index: Int,
  f: fn(Slide) -> Slide,
) -> List(Slide) {
  list.index_map(slides, fn(slide, i) {
    case i == index {
      True -> f(slide)
      False -> slide
    }
  })
}

fn update_element_in_slide(
  slides: List(Slide),
  slide_index: Int,
  element_id: String,
  f: fn(SlideElement) -> SlideElement,
) -> List(Slide) {
  update_slide_at(slides, slide_index, fn(slide) {
    types.Slide(
      ..slide,
      elements: list.map(slide.elements, fn(el) {
        case el.id == element_id {
          True -> f(el)
          False -> el
        }
      }),
    )
  })
}

fn find_element(
  slides: List(Slide),
  slide_index: Int,
  element_id: String,
) -> Option(SlideElement) {
  case get_at(slides, slide_index) {
    Some(slide) ->
      case list.find(slide.elements, fn(el: SlideElement) { el.id == element_id }) {
        Ok(el) -> Some(el)
        Error(_) -> None
      }
    None -> None
  }
}

fn move_item(items: List(a), from: Int, to: Int) -> List(a) {
  case get_at(items, from) {
    Some(item) -> {
      let without = list.filter(list.index_map(items, fn(x, i) { #(x, i) }), fn(pair) {
        pair.1 != from
      }) |> list.map(fn(pair) { pair.0 })
      let before = list.take(without, to)
      let after = list.drop(without, to)
      list.flatten([before, [item], after])
    }
    None -> items
  }
}

/// Get item at index (helper since list.at doesn't exist)
fn get_at(items: List(a), index: Int) -> Option(a) {
  items
  |> list.drop(index)
  |> list.first
  |> option.from_result
}

fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

fn build_transform(scale: Float, rotation: Float) -> String {
  "translate(-50%, -50%) scale("
  <> float.to_string(scale)
  <> ") rotate("
  <> float.to_string(rotation)
  <> "deg)"
}

fn parse_int(s: String) -> Int {
  case int.parse(s) {
    Ok(n) -> n
    Error(_) -> 0
  }
}

fn parse_float(s: String) -> Float {
  case float.parse(s) {
    Ok(f) -> f
    Error(_) -> 0.0
  }
}

@external(erlang, "vibee_carousel_ffi", "random_string")
@external(javascript, "../../../ffi.mjs", "randomString")
fn random_string() -> String

// ============================================================
// App Constructor
// ============================================================

pub fn app() -> lustre.App(Nil, Model, Msg) {
  lustre.application(init, update, view)
}
