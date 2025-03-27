local info    = "#B8BB26"
local warning = "#FE8019"
local hint    = "#F9F5D7"
local error   = "#FB4934"
local test    = "#D3869B"
local default = "#7C3AED"

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
          org_default_notes_file = '~/Org/Code.org',})
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
    opts = {
      signs = false,
      highlight = {
        keyword = "fg",
      },
      keywords = {
        FIX = {
          icon = " ", -- icon used for the sign, and in search results

          color = error, -- can be a hex color, or a named color (see below)
          alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
        },
        TODO = { icon = " ", color = info },
        HACK = { icon = " ", color = warning },
        WARN = { icon = " ", color = warning, alt = { "WARNING", "XXX" } },
        NOTE = { icon = " ", color = hint, alt = { "INFO" } },
        TEST = { icon = "T",  color = test, alt = { "TESTING", "PASSED", "FAILED" } },
        PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
      },
    }
  },

  {
    "justinmk/vim-sneak",
    event = "VeryLazy",
  },

  {
    "szw/vim-maximizer",
    keys = {
      { "<leader>wm", "<cmd>MaximizerToggle<CR>", desc = "Maximize window" }
    }
  }

}
