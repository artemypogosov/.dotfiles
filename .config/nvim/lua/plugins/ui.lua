-- Colors for folke/todo-comments.nvim
local info    = "#B8BB26"
local warning = "#FE8019"
local hint    = "#D5C4A1"
local error   = "#FB4934"
local test    = "#D3869B"
local default = "#FABD2F"

return {
  {
   "folke/todo-comments.nvim",
    lazy = true,
    event = "BufRead",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      signs = true,
      highlight = { keyword = "fg" },
      keywords = {
        FIX = {
          icon = " ",
          color = error,
          alt = { "FIXME", "BUG", "FIXIT", "ISSUE" },
        },
        TODO = { icon = " ", color = info },
        HACK = { icon = " ", color = warning },
        WARN = { icon = " ", color = warning, alt = { "WARNING", "XXX" } },
        NOTE = { icon = "",  color = hint, alt = { "INFO" } },
        TEST = { icon = "T",   color = test, alt = { "TESTING", "PASSED", "FAILED" } },
        PERF = { icon = "󰔟", color = default, alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
      },
    }
  },

  {
    'tomasky/bookmarks.nvim',
    lazy = true,
    event = "BufRead",
    config = function()
      require('bookmarks').setup {
        save_file = vim.fn.expand "$HOME/.cache/nvim/.bookmarks",
      }
    end
  },

  {
    "FabijanZulj/blame.nvim",
    event = 'VeryLazy',
    config = function()
      require('blame').setup {
        focus_blame = false,
        merge_consecutive = true,
       }
    end,
  }
}
