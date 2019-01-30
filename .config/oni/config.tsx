import * as React from "react"
import * as Oni from "oni-api"

export const activate = (oni: Oni.Plugin.Api) => {}

export const deactivate = (oni: Oni.Plugin.Api) => {
}

export const configuration = {
  "oni.loadInitVim": true,
  "oni.useDefaultConfig": true,
  "oni.useExternalPopupMenu": true,

  // Editor
  "editor.fontSize": "16px",
  "editor.fontFamily": "Hack",
  "editor.linePadding": 0,

  // UI customizations
  "ui.animations.enabled": true,
  "ui.colorscheme": "molotov",
  "ui.fontFamily": "BlinkMacSystemFont, 'Segoe UI', sans-serif",
  "ui.fontSmoothing": "subpixel-antialiased",

  // Tabs
  "tabs.mode": "buffers",

  // Sidebar
  "sidebar.default.open": false,

  // Auto-closing pairs
  "autoClosingPairs.enabled": true,

  // Experimental
  "experimental.markdownPreview.enabled": true,
  "experimental.preview.enabled": false,
  "experimental.particles.enabled": false,

  // Plugin: Prettier
  "oni.plugins.prettier": {
    settings: {
      semi: true,
      tabWidth: 2,
      useTabs: false,
      singleQuote: false,
      trailingComma: "es5",
      bracketSpacing: false,
      jsxBracketSameLine: false,
      arrowParens: "avoid",
      printWidth: 100,
    },
    enabled: true,
    formatOnSave: true,
  },
}
