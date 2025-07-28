return {
  "folke/which-key.nvim",
  lazy = true,
  event = "VeryLazy",
  opts = {
    preset = "modern",
    plugins = {
      marks = false,
      spelling = {
        suggestions = 10,
      },
      presets = {
        operators = false,
        motions = false,
        windows = false,
        nav = false,
        z = false,
        g = false,
      },
    },
    icons = {
      mappings = false,
    },
  },
}
