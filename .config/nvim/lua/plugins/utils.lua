return {
  {
    "justinmk/vim-sneak",
    event = "VeryLazy",
  },

  {
    "szw/vim-maximizer",
    event = "VeryLazy",
  },

  {
    "folke/zen-mode.nvim",
    event = "VeryLazy",
    opts = {
      window = {
        backdrop = 0.75,
        width = 175
      },
      plugins = {
        options = {
          enabled = true,
          showcmd = false,
          laststatus = 0,
          ruler = false
        },
        alacritty = {
          enabled = true,
          font = "13",
        },
      }
    }
  },

  {
    "RRethy/vim-illuminate",
    event = "VeryLazy",
    config = function()
      require("illuminate").configure({
        modes_allowlist = { "n" }, -- only enable in normal mode
        min_count_to_highlight = 2
      })
    end,
  },

  {
    "HiPhish/rainbow-delimiters.nvim",
    event = "VeryLazy",
  },

  {
    "folke/which-key.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = {
      preset = "modern",
      plugins = {
        marks = false,
        spelling = {
          suggestions = 10
        },
        presets = {
          operators = false,
          motions = false,
          windows = false,
          nav = false,
          z = false,
          g = false
        },
      },
      icons = {
        mappings = false
      }
    },
  },

  {
    'nvim-orgmode/orgmode',
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
    "NeogitOrg/neogit",
    event = 'VeryLazy',
    dependencies = {
      "nvim-lua/plenary.nvim",         -- required
      "sindrets/diffview.nvim",        -- optional - Diff integration
      -- Only one of these is needed.
      "nvim-telescope/telescope.nvim", -- optional
    },
  }
}
