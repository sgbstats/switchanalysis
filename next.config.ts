// next.config.ts — set Turbopack root to the project directory and allow your dev host
import path from 'path'
import type { NextConfig } from 'next'

const nextConfig: NextConfig = {
    // HMR host
    allowedDevOrigins: ['10.97.3.0'],

    // Ensure turbopack.root points to your project root (do NOT point above it)
    turbopack: {
        root: path.resolve(__dirname) // <- project root; remove this block if unnecessary
    },

    // Optional: only set if you intentionally want a custom output dir; keep it inside the project
    // distDir: '.next' // default — fine when root is correct
}

export default nextConfig
