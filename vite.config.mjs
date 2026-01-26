import { defineConfig } from "vite"
import elmWatchPlugin from "vite-plugin-elm-watch"

export default defineConfig({
  plugins: [elmWatchPlugin()],
})
