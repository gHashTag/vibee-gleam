# VIBEE Player - Changelog

All notable changes to the VIBEE Video Editor will be documented in this file.

---

## [1.0.0] - 2024-12-24

### Release: Caption Styles System

Full dynamic caption styling system with Google Fonts support.

#### Features
- **Dynamic Font Loading** - 150+ Cyrillic fonts from Google Fonts
- **Real-time Font Preview** - Font dropdown shows each font in its own style
- **Complete Style Control**:
  - Font Size (24-120px)
  - Font Weight (Regular to ExtraBold)
  - Text Color (color picker)
  - Highlight Color (for active words)
  - Text Shadow toggle
  - Bottom % position (5-50%)
  - Max Width % (50-100%)

#### Technical Changes

**`compositions/Captions.tsx`**
- Dynamic Google Fonts loading via CSS `<link>` injection
- New props: `fontId`, `fontWeight`, `showShadow`, `maxWidthPercent`
- Font lookup from centralized fonts registry
- Cyrillic subset support

**`compositions/SplitTalkingHead.tsx`**
- Updated `CaptionStyleSchema` with all style fields
- Proper `bottomPercent` → `topPercent` conversion
- Pass all style props to Captions component

**`components/Panels/CaptionsPanel.tsx`**
- Inline `fontFamily` style on dropdown options
- Google Fonts preloading when dropdown opens
- Full style editing UI

**`atoms/derived/templateProps.ts`**
- Added `fontId: 'Montserrat'` to caption style defaults
- Persisted to localStorage via `atomWithStorage`

**`atoms/index.ts`**
- Export `transcribeVideoAtom` and `transcribingAtom`

#### Files Modified
- `src/compositions/Captions.tsx`
- `src/compositions/SplitTalkingHead.tsx`
- `src/components/Panels/CaptionsPanel.tsx`
- `src/atoms/derived/templateProps.ts`
- `src/atoms/index.ts`

#### Deployment
- **App**: vibee-player
- **URL**: https://vibee-player.fly.dev
- **Strategy**: immediate

---

## Previous History

### Jotai Migration (2024-12-23)
- Migrated from Zustand to Jotai for state management
- Signal-based atoms architecture
- Improved performance with granular subscriptions

### Volume Controls (2024-12-22)
- Added volume slider control
- Zoom controls positioning fixes
- Hidden Remotion Player built-in controls

### Initial Release
- Timeline editor with multi-track support
- Canvas preview with Remotion Player
- B-roll video segments
- TikTok-style captions
- Background music with HTML5 Audio sync
