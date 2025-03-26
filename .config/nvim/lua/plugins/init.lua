return {
  ---------------------
  -- DEFAULT PLUGINS --
  ---------------------
  "nvim-lua/plenary.nvim",

  -- Optional, needed for theme switcher
  "nvzone/volt",

  {
    "stevearc/conform.nvim",
    -- event = 'BufWritePre', -- uncomment for format on save
    opts = require "configs.conform",
  },

  {
    "neovim/nvim-lspconfig",
    config = function()
      require "configs.lspconfig"
    end,
  },

  {
    "nvchad/ui",
    config = function()
      require "nvchad"
    end
  },

  {
    "nvchad/base46",
    lazy = true,
    build = function()
      require("base46").load_all_highlights()
    end,
  },

  ----------------
  -- MY PLUGINS --
  ----------------
  {
    "nvim-telescope/telescope-file-browser.nvim",
    lazy = true,
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
  },

  {
    'nvim-orgmode/orgmode',
    lazy = true,
    event = 'VeryLazy',
    ft = { 'org' },
    config = function()
      -- Setup orgmode
      require('orgmode').setup({
          org_agenda_files = '~/Org/**/*',
          org_default_notes_file = '~/Org/Code.org',
                              })
    end,
  },

  {
    'nvim-telescope/telescope-fzf-native.nvim',
    lazy = true,
    event = "BufRead",
    build = 'make',
    dependencies = { "nvim-telescope/telescope.nvim" },
    config = function()
      require("telescope").load_extension("fzf")
    end,
  },

  {
    "folke/todo-comments.nvim",
    lazy = true,
    event = "BufRead",
    dependencies = { "nvim-lua/plenary.nvim" },
    --opts = {
    --  signs = false,
    --  highlight = {
    --    keyword = "fg",
    --  }
    --}
  },
}
