# VIBEE Player - Claude Code Rules

## Deployment

**ALWAYS use Fly.io for deployment. NEVER use Vercel!**

```bash
# Standard deploy
fly deploy --app vibee-player

# Force rebuild (if Docker caches old code)
fly deploy --app vibee-player --no-cache

# Immediate strategy (faster, no rolling)
fly deploy --app vibee-player --strategy immediate
```

Production URL: https://vibee-player.fly.dev/

### Deployment Troubleshooting

1. **Docker cache issue**: If deploy logs show `CACHED` for build steps, your code changes aren't included!
   ```bash
   fly deploy --app vibee-player --no-cache
   ```

2. **Browser cache issue**: Changes deployed but not visible?
   - Hard refresh: `Cmd+Shift+R` (Mac) / `Ctrl+Shift+R` (Windows)
   - Or test in incognito mode

3. **Verify deployment**:
   ```bash
   fly status -a vibee-player  # Check version number increased
   ```

## Brand

- **VIBEE = Vibe Bee** - yellow/amber colors and bee emojis
- Primary color: `#f59e0b` (amber-500)
- Primary hover: `#fbbf24` (amber-400)
- NO purple colors!

## Tech Stack

- React 19 + TypeScript
- Vite 7
- Remotion (video player)
- Tailwind CSS v4
- Zustand (state)

## Commands

```bash
npm run dev      # Development server (localhost:5174)
npm run build    # Production build
npm run preview  # Preview production build
```

## Structure

- `/src/pages/` - Home (landing), Editor
- `/src/components/landing/` - Landing page sections
- `/src/components/` - Editor components (Header, Timeline, Canvas, Panels)
- `/src/hooks/useLanguage.tsx` - i18n (RU/EN)

## Social Links

- Telegram: https://t.me/vibee_super_agent
- GitHub: https://github.com/gHashTag
