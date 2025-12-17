#!/bin/bash
#
# Setup Test Assets
#
# Generates placeholder test assets using FFmpeg for testing the template factory.
# Run this script to create test videos and audio files.
#
# Usage:
#   ./scripts/setup-test-assets.sh
#
# Requirements:
#   - FFmpeg installed (brew install ffmpeg)
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
PUBLIC_DIR="$PROJECT_DIR/public/test"

echo "🎬 Setting up test assets for Vibe Reels Template Factory"
echo ""

# Check for FFmpeg
if ! command -v ffmpeg &> /dev/null; then
    echo "❌ FFmpeg is required but not installed."
    echo "   Install with: brew install ffmpeg"
    exit 1
fi

# Create directories
mkdir -p "$PUBLIC_DIR/lipsync"
mkdir -p "$PUBLIC_DIR/broll"
mkdir -p "$PUBLIC_DIR/music"
mkdir -p "$PUBLIC_DIR/covers"

# Generate test avatar (30 seconds, 1080x1920, gradient with text)
echo "📹 Generating test avatar video..."
ffmpeg -y -f lavfi \
    -i "color=c=#1a1a2e:s=1080x1920:d=30,format=yuv420p" \
    -vf "drawtext=text='TEST AVATAR':fontsize=72:fontcolor=white:x=(w-text_w)/2:y=h/2-100,\
         drawtext=text='30 seconds':fontsize=48:fontcolor=gray:x=(w-text_w)/2:y=h/2+50" \
    -c:v libx264 -preset fast -crf 23 \
    "$PUBLIC_DIR/lipsync/test-avatar.mp4" \
    -loglevel warning

echo "  ✓ test-avatar.mp4"

# Generate B-roll clips (10 seconds each, different colors)
echo "📹 Generating B-roll clips..."

BROLL_COLORS=("0x6c5ce7" "0x00cec9" "0xe94560" "0xffeaa7")
BROLL_LABELS=("TECH" "NATURE" "BUSINESS" "LIFESTYLE")

for i in {0..3}; do
    num=$(printf "%02d" $((i+1)))
    color="${BROLL_COLORS[$i]}"
    label="${BROLL_LABELS[$i]}"

    ffmpeg -y -f lavfi \
        -i "color=c=${color}:s=1920x1080:d=10,format=yuv420p" \
        -vf "drawtext=text='B-ROLL ${num}':fontsize=96:fontcolor=white:x=(w-text_w)/2:y=h/2-60,\
             drawtext=text='${label}':fontsize=48:fontcolor=white@0.7:x=(w-text_w)/2:y=h/2+60" \
        -c:v libx264 -preset fast -crf 23 \
        "$PUBLIC_DIR/broll/test-broll-${num}.mp4" \
        -loglevel warning

    echo "  ✓ test-broll-${num}.mp4"
done

# Generate test music (60 seconds, ambient tone)
echo "🎵 Generating test music..."
ffmpeg -y -f lavfi \
    -i "sine=frequency=440:duration=60" \
    -af "volume=0.3,lowpass=f=800" \
    -c:a libmp3lame -q:a 4 \
    "$PUBLIC_DIR/music/test-music.mp3" \
    -loglevel warning

echo "  ✓ test-music.mp3"

# Generate test cover (1080x1920, gradient)
echo "🖼️  Generating test cover..."
ffmpeg -y -f lavfi \
    -i "color=c=#1a1a2e:s=1080x1920:d=1,format=yuv420p" \
    -vf "drawtext=text='TEST COVER':fontsize=72:fontcolor=white:x=(w-text_w)/2:y=h/2" \
    -frames:v 1 \
    "$PUBLIC_DIR/covers/test-cover.jpg" \
    -loglevel warning

echo "  ✓ test-cover.jpg"

echo ""
echo "✅ Test assets generated successfully!"
echo ""
echo "Assets location: $PUBLIC_DIR"
echo ""
echo "You can now run:"
echo "  npx ts-node scripts/batch-render.ts --test-mode --dry-run"
