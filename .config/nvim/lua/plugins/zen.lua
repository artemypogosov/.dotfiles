return {
  "folke/zen-mode.nvim",
  event = "VeryLazy",
  opts = {
    window = {
      backdrop = 0.75,
      width = 175,
    },
    plugins = {
      options = {
        enabled = true,
        showcmd = false,
        laststatus = 0,
        ruler = false,
      },
      alacritty = {
        enabled = true,
        font = "13",
      },
    },
  },
}
