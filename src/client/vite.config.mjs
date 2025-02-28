import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:5000'
      }
    }
  },
  plugins: [
    react(),
    tailwindcss(),
  ],
  root: './src',
  build: {
    outDir: '../dist',
  }
})
