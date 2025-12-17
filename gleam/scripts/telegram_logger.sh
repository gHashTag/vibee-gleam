#!/bin/bash
# VIBEE Telegram Logger
# Polls messages from Go bridge and writes to vibee.log

GO_BRIDGE_URL="http://localhost:8081"
SESSION_ID="sess_desrtl4nry3c"
LOG_FILE="/Users/playra/vibee-eliza-999/vibee/gleam/logs/vibee.log"
SEEN_FILE="/tmp/vibee_seen_messages.txt"

# Create log directory if not exists
mkdir -p "$(dirname "$LOG_FILE")"
touch "$SEEN_FILE"

log() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "$timestamp $1" | tee -a "$LOG_FILE"
}

is_seen() {
    grep -q "^$1$" "$SEEN_FILE" 2>/dev/null
}

mark_seen() {
    echo "$1" >> "$SEEN_FILE"
}

log "[INFO] 🚀 VIBEE Telegram Logger started"
log "[INFO] 📁 Log file: $LOG_FILE"
log "[INFO] 🔗 Go bridge: $GO_BRIDGE_URL"
log "[INFO] 🔑 Session: $SESSION_ID"

# Get dialogs (groups)
log "[INFO] 📋 Fetching dialogs..."
DIALOGS=$(curl -s "$GO_BRIDGE_URL/api/v1/dialogs?limit=10" -H "X-Session-ID: $SESSION_ID" 2>/dev/null)

if [ -z "$DIALOGS" ]; then
    log "[ERROR] ❌ Failed to fetch dialogs - Go bridge not available"
    exit 1
fi

log "[INFO] ✅ Connected to Telegram"

# Extract group IDs (supergroups start with -100)
GROUP_IDS=$(echo "$DIALOGS" | grep -o '"id":-100[0-9]*' | grep -o '\-100[0-9]*' | head -5)

log "[INFO] 📝 Monitoring groups:"
for ID in $GROUP_IDS; do
    TITLE=$(echo "$DIALOGS" | grep -o "\"id\":$ID[^}]*\"title\":\"[^\"]*" | grep -o '"title":"[^"]*' | cut -d'"' -f4)
    log "[INFO]     $TITLE ($ID)"
done

poll_messages() {
    for GROUP_ID in $GROUP_IDS; do
        # Get group title
        TITLE=$(echo "$DIALOGS" | grep -o "\"id\":$GROUP_ID[^}]*\"title\":\"[^\"]*" | grep -o '"title":"[^"]*' | cut -d'"' -f4)

        # Fetch history
        HISTORY=$(curl -s "$GO_BRIDGE_URL/api/v1/history/$GROUP_ID?limit=3" -H "X-Session-ID: $SESSION_ID" 2>/dev/null)

        if [ -z "$HISTORY" ]; then
            continue
        fi

        # Parse messages using jq if available, otherwise grep
        if command -v jq &> /dev/null; then
            echo "$HISTORY" | jq -r '.messages[]? | "\(.id)|\(.from_name // "Unknown")|\(.text // "")"' 2>/dev/null | while IFS='|' read -r MSG_ID FROM_NAME MSG_TEXT; do
                KEY="${GROUP_ID}_${MSG_ID}"
                if ! is_seen "$KEY"; then
                    mark_seen "$KEY"
                    if [ -n "$MSG_TEXT" ] && [ "$MSG_TEXT" != "null" ]; then
                        TRUNCATED=$(echo "$MSG_TEXT" | head -c 150 | tr '\n' ' ')
                        log "[INFO] 💬 [$TITLE] $FROM_NAME: $TRUNCATED"
                    fi
                fi
            done
        else
            # Fallback: simple grep parsing
            echo "$HISTORY" | grep -o '"id":[0-9]*,"text":"[^"]*","from_id":[0-9]*,"from_name":"[^"]*' | while read MSG_DATA; do
                MSG_ID=$(echo "$MSG_DATA" | grep -o '"id":[0-9]*' | cut -d':' -f2)
                MSG_TEXT=$(echo "$MSG_DATA" | grep -o '"text":"[^"]*' | cut -d'"' -f4 | head -c 150)
                FROM_NAME=$(echo "$MSG_DATA" | grep -o '"from_name":"[^"]*' | cut -d'"' -f4)

                KEY="${GROUP_ID}_${MSG_ID}"
                if ! is_seen "$KEY"; then
                    mark_seen "$KEY"
                    if [ -n "$MSG_TEXT" ] && [ "$MSG_TEXT" != "null" ]; then
                        log "[INFO] 💬 [$TITLE] $FROM_NAME: $MSG_TEXT"
                    fi
                fi
            done
        fi
    done
}

# Initial poll to mark existing messages as seen
log "[INFO] 🔄 Initial sync..."
poll_messages

log "[INFO] 👀 Watching for new messages (polling every 5 seconds)..."

# Poll every 5 seconds
while true; do
    sleep 5
    poll_messages
done
