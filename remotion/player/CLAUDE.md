# VIBEE Player - Claude Code Rules

## Deployment

**ALWAYS use Fly.io for deployment. NEVER use Vercel!**

```bash
# Deploy command
fly deploy --app vibee-player
```

Production URL: https://vibee-player.fly.dev/

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
