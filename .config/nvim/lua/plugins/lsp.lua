return {
  {
    "stevearc/conform.nvim",
    event = "VeryLazy",
    opts = require "configs.conform",
  },

  {
    "neovim/nvim-lspconfig",
    event = "VeryLazy",
    config = function()
      require "configs.lspconfig"
    end,
  }
}
