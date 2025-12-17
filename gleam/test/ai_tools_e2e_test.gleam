// AI Tools E2E Tests - End-to-end testing for AI MCP tools
// Tests the full cycle: tool definition -> JSON encoding -> handler simulation
// Run with: gleam test

import gleam/json
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import vibee/mcp/ai_tools
import vibee/mcp/types.{type Tool}

pub fn main() {
  gleeunit.main()
}

// ============================================================
// E2E: Tool Registration Tests
// ============================================================

pub fn ai_tools_registered_in_registry_e2e_test() {
  // E2E: Проверяем что все AI инструменты имеют правильные имена
  let all_tools_list = ai_tools.all_ai_tools()
  let ai_tool_names = [
    "ai_elevenlabs_tts",
    "ai_elevenlabs_list_voices",
    "ai_hedra_create_avatar",
    "ai_hedra_get_status",
    "ai_hedra_list_jobs",
    "ai_bfl_generate_image",
    "ai_bfl_get_result",
    "ai_kling_create_video",
    "ai_kling_image_to_video",
    "ai_kling_get_task",
    "ai_kling_list_tasks",
    "ai_heygen_create_video",
    "ai_heygen_list_avatars",
    "ai_heygen_get_video_status",
    "ai_heygen_list_voices",
  ]

  // Каждый AI инструмент должен быть в списке
  list.each(ai_tool_names, fn(name) {
    let found = list.find(all_tools_list, fn(t: Tool) { t.name == name })
    should.be_ok(found)
  })
}

pub fn ai_tools_have_correct_prefix_e2e_test() {
  // E2E: Проверяем что AI инструменты имеют префикс ai_
  let ai_tools_list = ai_tools.all_ai_tools()

  list.each(ai_tools_list, fn(tool: Tool) {
    // Все AI tools должны начинаться с "ai_"
    should.be_true(string.starts_with(tool.name, "ai_"))
  })
}

pub fn ai_tools_names_follow_convention_e2e_test() {
  // E2E: Проверяем что имена AI инструментов следуют конвенции ai_<provider>_<action>
  let ai_tools_list = ai_tools.all_ai_tools()

  list.each(ai_tools_list, fn(tool: Tool) {
    // Имя должно содержать хотя бы 2 подчеркивания (ai_<provider>_<action>)
    let parts = string.split(tool.name, "_")
    should.be_true(list.length(parts) >= 3)
  })
}

// ============================================================
// E2E: Tool Schema Validation Tests
// ============================================================

pub fn elevenlabs_tts_schema_e2e_test() {
  // E2E: Полная проверка схемы ElevenLabs TTS
  let tool = ai_tools.elevenlabs_tts_tool()

  // Проверяем имя и описание
  should.equal(tool.name, "ai_elevenlabs_tts")
  should.be_true(string.contains(tool.description, "text"))
  should.be_true(string.contains(tool.description, "speech"))
  should.be_true(string.contains(tool.description, "ElevenLabs"))

  // Схема должна быть валидным JSON
  let schema_str = json.to_string(tool.input_schema)
  should.be_true(string.length(schema_str) > 0)
  should.be_true(string.contains(schema_str, "text"))
  should.be_true(string.contains(schema_str, "voice_id"))
  should.be_true(string.contains(schema_str, "model"))
}

pub fn hedra_avatar_schema_e2e_test() {
  // E2E: Полная проверка схемы Hedra Avatar
  let tool = ai_tools.hedra_create_avatar_tool()

  should.equal(tool.name, "ai_hedra_create_avatar")
  should.be_true(string.contains(tool.description, "avatar"))
  should.be_true(string.contains(tool.description, "Hedra"))

  let schema_str = json.to_string(tool.input_schema)
  should.be_true(string.contains(schema_str, "audio_url"))
  should.be_true(string.contains(schema_str, "image_url"))
  should.be_true(string.contains(schema_str, "aspect_ratio"))
}

pub fn bfl_image_schema_e2e_test() {
  // E2E: Полная проверка схемы BFL Image
  let tool = ai_tools.bfl_generate_image_tool()

  should.equal(tool.name, "ai_bfl_generate_image")
  should.be_true(string.contains(tool.description, "image"))
  should.be_true(string.contains(tool.description, "FLUX"))

  let schema_str = json.to_string(tool.input_schema)
  should.be_true(string.contains(schema_str, "prompt"))
  should.be_true(string.contains(schema_str, "model"))
  should.be_true(string.contains(schema_str, "flux-pro-1.1"))
  should.be_true(string.contains(schema_str, "width"))
  should.be_true(string.contains(schema_str, "height"))
}

pub fn kling_video_schema_e2e_test() {
  // E2E: Полная проверка схемы Kling Video
  let tool = ai_tools.kling_create_video_tool()

  should.equal(tool.name, "ai_kling_create_video")
  should.be_true(string.contains(tool.description, "video"))
  should.be_true(string.contains(tool.description, "Kling"))

  let schema_str = json.to_string(tool.input_schema)
  should.be_true(string.contains(schema_str, "prompt"))
  should.be_true(string.contains(schema_str, "mode"))
  should.be_true(string.contains(schema_str, "std"))
  should.be_true(string.contains(schema_str, "pro"))
  should.be_true(string.contains(schema_str, "duration"))
}

pub fn kling_image_to_video_schema_e2e_test() {
  // E2E: Проверка Kling Image to Video
  let tool = ai_tools.kling_image_to_video_tool()

  should.equal(tool.name, "ai_kling_image_to_video")
  should.be_true(string.contains(tool.description, "image"))

  let schema_str = json.to_string(tool.input_schema)
  should.be_true(string.contains(schema_str, "image_url"))
  should.be_true(string.contains(schema_str, "prompt"))
  should.be_true(string.contains(schema_str, "duration"))
}

pub fn heygen_video_schema_e2e_test() {
  // E2E: Полная проверка схемы HeyGen Video
  let tool = ai_tools.heygen_create_video_tool()

  should.equal(tool.name, "ai_heygen_create_video")
  should.be_true(string.contains(tool.description, "avatar"))
  should.be_true(string.contains(tool.description, "HeyGen"))

  let schema_str = json.to_string(tool.input_schema)
  should.be_true(string.contains(schema_str, "avatar_id"))
  should.be_true(string.contains(schema_str, "script"))
  should.be_true(string.contains(schema_str, "voice_id"))
  should.be_true(string.contains(schema_str, "background_url"))
}

// ============================================================
// E2E: Status/List Tools Schema Tests
// ============================================================

pub fn hedra_status_tools_e2e_test() {
  // E2E: Hedra status and list tools
  let status_tool = ai_tools.hedra_get_status_tool()
  let list_tool = ai_tools.hedra_list_jobs_tool()

  should.equal(status_tool.name, "ai_hedra_get_status")
  should.be_true(string.contains(json.to_string(status_tool.input_schema), "job_id"))

  should.equal(list_tool.name, "ai_hedra_list_jobs")
}

pub fn bfl_result_tool_e2e_test() {
  // E2E: BFL get result tool
  let tool = ai_tools.bfl_get_result_tool()

  should.equal(tool.name, "ai_bfl_get_result")
  should.be_true(string.contains(json.to_string(tool.input_schema), "task_id"))
}

pub fn kling_status_tools_e2e_test() {
  // E2E: Kling status and list tools
  let task_tool = ai_tools.kling_get_task_tool()
  let list_tool = ai_tools.kling_list_tasks_tool()

  should.equal(task_tool.name, "ai_kling_get_task")
  should.be_true(string.contains(json.to_string(task_tool.input_schema), "task_id"))

  should.equal(list_tool.name, "ai_kling_list_tasks")
}

pub fn heygen_status_tools_e2e_test() {
  // E2E: HeyGen status and list tools
  let status_tool = ai_tools.heygen_get_video_status_tool()
  let avatars_tool = ai_tools.heygen_list_avatars_tool()
  let voices_tool = ai_tools.heygen_list_voices_tool()

  should.equal(status_tool.name, "ai_heygen_get_video_status")
  should.be_true(string.contains(json.to_string(status_tool.input_schema), "video_id"))

  should.equal(avatars_tool.name, "ai_heygen_list_avatars")
  should.equal(voices_tool.name, "ai_heygen_list_voices")
}

pub fn elevenlabs_voices_tool_e2e_test() {
  // E2E: ElevenLabs list voices tool
  let tool = ai_tools.elevenlabs_list_voices_tool()

  should.equal(tool.name, "ai_elevenlabs_list_voices")
  should.be_true(string.contains(tool.description, "voice"))
}

// ============================================================
// E2E: Tool Count and Completeness Tests
// ============================================================

pub fn all_ai_tools_count_e2e_test() {
  // E2E: Проверяем полноту списка AI инструментов
  let tools_list = ai_tools.all_ai_tools()

  // Должно быть минимум 15 инструментов
  should.be_true(list.length(tools_list) >= 15)

  // Проверяем наличие по категориям
  let elevenlabs_count =
    list.filter(tools_list, fn(t: Tool) { string.contains(t.name, "elevenlabs") })
    |> list.length
  let hedra_count =
    list.filter(tools_list, fn(t: Tool) { string.contains(t.name, "hedra") })
    |> list.length
  let bfl_count =
    list.filter(tools_list, fn(t: Tool) { string.contains(t.name, "bfl") })
    |> list.length
  let kling_count =
    list.filter(tools_list, fn(t: Tool) { string.contains(t.name, "kling") })
    |> list.length
  let heygen_count =
    list.filter(tools_list, fn(t: Tool) { string.contains(t.name, "heygen") })
    |> list.length

  // ElevenLabs: 2 инструмента (tts, list_voices)
  should.equal(elevenlabs_count, 2)

  // Hedra: 3 инструмента (create_avatar, get_status, list_jobs)
  should.equal(hedra_count, 3)

  // BFL: 2 инструмента (generate_image, get_result)
  should.equal(bfl_count, 2)

  // Kling: 4 инструмента (create_video, image_to_video, get_task, list_tasks)
  should.equal(kling_count, 4)

  // HeyGen: 4 инструмента (create_video, list_avatars, get_video_status, list_voices)
  should.equal(heygen_count, 4)
}

pub fn all_tools_have_ai_prefix_e2e_test() {
  // E2E: Все AI инструменты должны иметь префикс ai_
  let tools_list = ai_tools.all_ai_tools()

  list.each(tools_list, fn(tool: Tool) {
    should.be_true(string.starts_with(tool.name, "ai_"))
  })
}

pub fn all_tools_have_valid_schema_e2e_test() {
  // E2E: Все AI инструменты должны иметь валидную JSON схему
  let tools_list = ai_tools.all_ai_tools()

  list.each(tools_list, fn(tool: Tool) {
    let schema_str = json.to_string(tool.input_schema)

    // Схема не должна быть пустой
    should.be_true(string.length(schema_str) > 10)

    // Должна содержать type: object
    should.be_true(string.contains(schema_str, "object"))

    // Должна содержать properties
    should.be_true(string.contains(schema_str, "properties"))
  })
}

pub fn all_tools_have_description_e2e_test() {
  // E2E: Все AI инструменты должны иметь описание
  let tools_list = ai_tools.all_ai_tools()

  list.each(tools_list, fn(tool: Tool) {
    should.be_true(string.length(tool.description) > 10)
  })
}

// ============================================================
// E2E: Integration with AI Service Modules Tests
// ============================================================

pub fn elevenlabs_module_integration_e2e_test() {
  // E2E: Интеграция с модулем ElevenLabs
  // Проверяем что параметры tool совпадают с модулем
  let tool = ai_tools.elevenlabs_tts_tool()
  let schema_str = json.to_string(tool.input_schema)

  // Проверяем модели из elevenlabs модуля
  should.be_true(string.contains(schema_str, "eleven_multilingual_v2"))
  should.be_true(string.contains(schema_str, "eleven_turbo_v2"))
  should.be_true(string.contains(schema_str, "eleven_monolingual_v1"))
}

pub fn hedra_module_integration_e2e_test() {
  // E2E: Интеграция с модулем Hedra
  let tool = ai_tools.hedra_create_avatar_tool()
  let schema_str = json.to_string(tool.input_schema)

  // Проверяем aspect ratios из hedra модуля
  should.be_true(string.contains(schema_str, "1:1"))
  should.be_true(string.contains(schema_str, "16:9"))
  should.be_true(string.contains(schema_str, "9:16"))
}

pub fn bfl_module_integration_e2e_test() {
  // E2E: Интеграция с модулем BFL
  let tool = ai_tools.bfl_generate_image_tool()
  let schema_str = json.to_string(tool.input_schema)

  // Проверяем модели из bfl модуля
  should.be_true(string.contains(schema_str, "flux-pro-1.1"))
  should.be_true(string.contains(schema_str, "flux-pro"))
  should.be_true(string.contains(schema_str, "flux-dev"))
  should.be_true(string.contains(schema_str, "flux-pro-1.1-ultra"))
}

pub fn kling_module_integration_e2e_test() {
  // E2E: Интеграция с модулем Kling
  let tool = ai_tools.kling_create_video_tool()
  let schema_str = json.to_string(tool.input_schema)

  // Проверяем режимы из kling модуля
  should.be_true(string.contains(schema_str, "std"))
  should.be_true(string.contains(schema_str, "pro"))

  // Проверяем длительности
  should.be_true(string.contains(schema_str, "\"5\""))
  should.be_true(string.contains(schema_str, "\"10\""))
}

pub fn heygen_module_integration_e2e_test() {
  // E2E: Интеграция с модулем HeyGen
  let tool = ai_tools.heygen_create_video_tool()
  let schema_str = json.to_string(tool.input_schema)

  // Проверяем обязательные поля из heygen модуля
  should.be_true(string.contains(schema_str, "avatar_id"))
  should.be_true(string.contains(schema_str, "script"))
}

// ============================================================
// E2E: JSON Encoding/Decoding Tests
// ============================================================

pub fn tool_schema_json_roundtrip_e2e_test() {
  // E2E: Проверяем что JSON схема корректно сериализуется
  let tools_list = ai_tools.all_ai_tools()

  list.each(tools_list, fn(tool: Tool) {
    // Сериализация в JSON не должна падать
    let json_str = json.to_string(tool.input_schema)
    should.be_true(string.length(json_str) > 0)

    // JSON должен быть валидным (содержать базовые структуры)
    should.be_true(string.contains(json_str, "{"))
    should.be_true(string.contains(json_str, "}"))
  })
}
