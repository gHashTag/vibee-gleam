/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        'editor-bg': '#0a0a0a',
        'panel-bg': '#1a1a1a',
        'panel-border': '#2a2a2a',
        'accent': '#7c3aed',
        'accent-hover': '#6d28d9',
      }
    },
  },
  plugins: [],
}
