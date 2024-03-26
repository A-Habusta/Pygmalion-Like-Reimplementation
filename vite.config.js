import { defineConfig } from 'vite'

// https://vitejs.dev/config/
export default defineConfig({
    clearScreen: false,
    base: "",
    server: {
        host: "127.0.0.1",
        watch: {
            ignored: [
                "**/*.fs" // Don't watch F# files
            ]
        }
    },
    build: {
        assetsDir: "./"
    }
})
