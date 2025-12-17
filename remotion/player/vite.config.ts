import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import path from 'path'

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
      '@compositions': path.resolve(__dirname, './src/compositions'),
      // Dedupe React - ensure single instance
      'react': path.resolve(__dirname, 'node_modules/react'),
      'react-dom': path.resolve(__dirname, 'node_modules/react-dom'),
    },
    dedupe: ['react', 'react-dom'],
  },
  optimizeDeps: {
    include: ['react', 'react-dom', '@remotion/player', 'remotion'],
  },
  server: {
    port: 5174,
    proxy: {
      '/api': {
        target: 'http://localhost:3333',
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ''),
      },
      '/renders': {
        target: 'http://localhost:3333',
        changeOrigin: true,
      },
      '/backgrounds': {
        target: 'http://localhost:3333',
        changeOrigin: true,
        rewrite: (path) => '/public' + path,
      },
      '/lipsync': {
        target: 'http://localhost:3333',
        changeOrigin: true,
        rewrite: (path) => '/public' + path,
      },
      '/covers': {
        target: 'http://localhost:3333',
        changeOrigin: true,
        rewrite: (path) => '/public' + path,
      },
      '/music': {
        target: 'http://localhost:3333',
        changeOrigin: true,
        rewrite: (path) => '/public' + path,
      },
    },
  },
})
