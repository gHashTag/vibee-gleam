#!/bin/bash
# VIBEE Telegram Agent
# Аналог TelegramService из plugin-telegram-craft
#
# Архитектура:
# ┌──────────────────┐     ┌────────────────┐     ┌──────────────┐
# │  VIBEE Agent     │────▶│   Go Bridge    │────▶│   Telegram   │
# │  (этот скрипт)   │     │  :8081         │     │   MTProto    │
# └──────────────────┘     └────────────────┘     └──────────────┘
#         │
#         ▼
# ┌──────────────────┐
# │  OpenRouter LLM  │
# │  (Grok/GPT)      │
# └──────────────────┘

# Конфигурация
GO_BRIDGE_URL="http://localhost:8081"
SESSION_ID="REDACTED_SESSION"
LOG_FILE="/Users/playra/vibee-eliza-999/vibee/gleam/logs/vibee.log"
SEEN_FILE="/tmp/vibee_agent_seen.txt"
CHARACTER_FILE="/Users/playra/vibee-eliza-999/vibee/gleam/config/characters/vibee_agent.json"

# Целевые чаты (аналог targetChats.ts)
TARGET_CHATS=(
  "-1002643951085"   # AiStars ОФИС
  "-1002737186844"   # Agent Vibe
)

# Триггеры для автоответа
TRIGGERS=("vibee" "vibe" "@vibee" "бот" "агент" "agent")

# OpenRouter API (опционально)
OPENROUTER_API_KEY="${OPENROUTER_API_KEY:-}"
LLM_MODEL="x-ai/grok-4.1-fast"

# Cooldown между ответами (секунды)
REPLY_COOLDOWN=30
LAST_REPLY_TIME=0

# Статистика
TOTAL_MESSAGES=0
TOTAL_REPLIES=0

# Создать директории
mkdir -p "$(dirname "$LOG_FILE")"
touch "$SEEN_FILE"

# === Логирование ===
log() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "$timestamp $1" | tee -a "$LOG_FILE"
}

# === Проверка seen сообщений ===
is_seen() {
    grep -q "^$1$" "$SEEN_FILE" 2>/dev/null
}

mark_seen() {
    echo "$1" >> "$SEEN_FILE"
}

# === Проверка целевого чата ===
is_target_chat() {
    local chat_id="$1"
    for target in "${TARGET_CHATS[@]}"; do
        # Нормализация ID
        local norm_target="${target#-100}"
        local norm_chat="${chat_id#-100}"
        if [[ "$norm_target" == "$norm_chat" ]] || [[ "$target" == "$chat_id" ]]; then
            return 0
        fi
    done
    return 1
}

# === Проверка триггеров ===
contains_trigger() {
    local text="$1"
    local lower_text=$(echo "$text" | tr '[:upper:]' '[:lower:]')

    for trigger in "${TRIGGERS[@]}"; do
        if [[ "$lower_text" == *"$trigger"* ]]; then
            return 0
        fi
    done
    return 1
}

# === Генерация ответа через OpenRouter ===
generate_llm_reply() {
    local user_message="$1"

    if [ -z "$OPENROUTER_API_KEY" ]; then
        # Fallback ответ без LLM
        echo "Privet! Ya VIBEE - agent na Gleam/BEAM platforme. LLM otvety budut dostupy posle nastrojki OPENROUTER_API_KEY."
        return
    fi

    local system_prompt="Ty VIBEE - druzhelubnyj agent na Gleam/BEAM platforme. Otvechaj kratko na russkom. Ispol'zuj emoji."

    local response=$(curl -s -X POST "https://openrouter.ai/api/v1/chat/completions" \
        -H "Authorization: Bearer $OPENROUTER_API_KEY" \
        -H "Content-Type: application/json" \
        -d "{
            \"model\": \"$LLM_MODEL\",
            \"messages\": [
                {\"role\": \"system\", \"content\": \"$system_prompt\"},
                {\"role\": \"user\", \"content\": \"$user_message\"}
            ],
            \"max_tokens\": 500
        }" 2>/dev/null)

    # Парсим ответ
    local content=$(echo "$response" | jq -r '.choices[0].message.content // empty' 2>/dev/null)

    if [ -n "$content" ]; then
        echo "$content"
    else
        echo "Izvini, ne smogu otvetit' seychas. Poprobuj pozzhe!"
    fi
}

# === Отправка сообщения ===
send_message() {
    local chat_id="$1"
    local text="$2"
    local reply_to="$3"

    local json_body="{\"chat_id\": \"$chat_id\", \"text\": \"$text\""
    if [ -n "$reply_to" ]; then
        json_body="$json_body, \"reply_to\": $reply_to"
    fi
    json_body="$json_body}"

    log "[INFO] 📤 Sending reply to $chat_id"

    local response=$(curl -s -X POST "$GO_BRIDGE_URL/api/v1/send" \
        -H "Content-Type: application/json" \
        -H "X-Session-ID: $SESSION_ID" \
        -d "$json_body" 2>/dev/null)

    if echo "$response" | grep -q '"success":true'; then
        log "[INFO] ✅ Message sent successfully"
        TOTAL_REPLIES=$((TOTAL_REPLIES + 1))
        return 0
    else
        log "[ERROR] ❌ Failed to send message: $response"
        return 1
    fi
}

# === Обработка сообщения ===
process_message() {
    local chat_id="$1"
    local chat_title="$2"
    local from_name="$3"
    local text="$4"
    local message_id="$5"

    TOTAL_MESSAGES=$((TOTAL_MESSAGES + 1))

    # Логируем все сообщения
    log "[INFO] 💬 [$chat_title] $from_name: ${text:0:150}"

    # Проверяем целевой чат
    if ! is_target_chat "$chat_id"; then
        return
    fi

    log "[INFO] 🎯 Target chat detected: $chat_title"

    # Проверяем триггер
    if ! contains_trigger "$text"; then
        return
    fi

    log "[INFO] 🔔 Trigger found in message!"

    # Проверяем cooldown
    local current_time=$(date +%s)
    local time_diff=$((current_time - LAST_REPLY_TIME))

    if [ $time_diff -lt $REPLY_COOLDOWN ]; then
        log "[INFO] ⏳ Cooldown active, skipping reply (${time_diff}s < ${REPLY_COOLDOWN}s)"
        return
    fi

    # Генерируем ответ
    log "[INFO] 🤖 Generating reply..."
    local reply=$(generate_llm_reply "$text")

    # Отправляем ответ
    if send_message "$chat_id" "$reply" "$message_id"; then
        LAST_REPLY_TIME=$current_time
    fi
}

# === Polling сообщений ===
poll_messages() {
    # Получаем список диалогов
    local dialogs=$(curl -s "$GO_BRIDGE_URL/api/v1/dialogs?limit=20" -H "X-Session-ID: $SESSION_ID" 2>/dev/null)

    if [ -z "$dialogs" ]; then
        return
    fi

    # Извлекаем ID групп
    local group_ids=$(echo "$dialogs" | jq -r '.dialogs[]? | select(.type == "supergroup" or .type == "group") | .id' 2>/dev/null | head -10)

    for group_id in $group_ids; do
        # Получаем название группы
        local title=$(echo "$dialogs" | jq -r ".dialogs[]? | select(.id == $group_id) | .title" 2>/dev/null)

        # Получаем последние сообщения
        local history=$(curl -s "$GO_BRIDGE_URL/api/v1/history/$group_id?limit=3" -H "X-Session-ID: $SESSION_ID" 2>/dev/null)

        if [ -z "$history" ]; then
            continue
        fi

        # Парсим сообщения
        echo "$history" | jq -r '.messages[]? | "\(.id)|\(.from_name // "Unknown")|\(.text // "")"' 2>/dev/null | while IFS='|' read -r msg_id from_name msg_text; do
            local key="${group_id}_${msg_id}"

            if ! is_seen "$key"; then
                mark_seen "$key"

                if [ -n "$msg_text" ] && [ "$msg_text" != "null" ]; then
                    process_message "$group_id" "$title" "$from_name" "$msg_text" "$msg_id"
                fi
            fi
        done
    done
}

# === Главная функция ===
main() {
    log "[INFO] =========================================="
    log "[INFO] 🚀 VIBEE Telegram Agent started"
    log "[INFO] =========================================="
    log "[INFO] 📁 Log file: $LOG_FILE"
    log "[INFO] 🔗 Go bridge: $GO_BRIDGE_URL"
    log "[INFO] 🔑 Session: $SESSION_ID"
    log "[INFO] 🎯 Target chats: ${TARGET_CHATS[*]}"
    log "[INFO] 🔔 Triggers: ${TRIGGERS[*]}"
    log "[INFO] 🤖 LLM: $LLM_MODEL"
    log "[INFO] ⏱️  Cooldown: ${REPLY_COOLDOWN}s"

    if [ -n "$OPENROUTER_API_KEY" ]; then
        log "[INFO] ✅ OpenRouter API key configured"
    else
        log "[WARN] ⚠️  No OPENROUTER_API_KEY - using fallback replies"
    fi

    # Проверяем Go bridge
    log "[INFO] 🔍 Checking Go bridge..."
    local health=$(curl -s "$GO_BRIDGE_URL/health" 2>/dev/null)

    if [ -z "$health" ]; then
        log "[ERROR] ❌ Go bridge not available at $GO_BRIDGE_URL"
        log "[ERROR] ❌ Please start telegram-bridge first"
        exit 1
    fi

    log "[INFO] ✅ Go bridge is healthy"

    # Начальная синхронизация
    log "[INFO] 🔄 Initial sync..."
    poll_messages

    log "[INFO] 👀 Watching for messages (polling every 5s)..."
    log "[INFO] =========================================="

    # Основной цикл
    while true; do
        sleep 5
        poll_messages
    done
}

# Запуск
main "$@"
