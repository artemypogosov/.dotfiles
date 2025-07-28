return {
  { "nvim-lua/plenary.nvim" },

  {
    "nvim-telescope/telescope.nvim",

    cmd = "Telescope", -- Lazy-load the plugin **only when** the `:Telescope` command is run

    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
      "nvim-telescope/telescope-fzf-native.nvim",
      "nvim-tree/nvim-web-devicons",

      -- "nvim-telescope/telescope-bookmarks.nvim",
      -- "nvim-telescope/telescope-workspaces.nvim",
    },
    config = function()
      local telescope = require("telescope")

      telescope.setup({
        defaults = {
          cwd = vim.fn.getcwd(),
          prompt_prefix = "   ",
          selection_caret = " ",
          entry_prefix = "  ",
          sorting_strategy = "ascending",
          layout_strategy = "horizontal",
          layout_config = {
            prompt_position = "top",
            horizontal = {
              preview_width = 0.55,
              results_width = 0.8,
            },
            width = 0.5,
            height = 0.6,
            preview_cutoff = 120,
          },
          winblend = 0, -- Set to 10–20 for slight transparency
          border = true,
          borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
          color_devicons = true,
          file_ignore_patterns = {
            "node_modules",
            ".java",
            ".cache",
            "Downloads/Installed",
            ".steam",
            ".sane",
          },
        },
        pickers = {
          buffers = {
            theme = "dropdown",
          },
          find_files = {
            theme = "dropdown",
            find_command = { "fd", "--type", "f", "--follow", "--exclude", ".git" },
            hidden = true,
            previewer = false,
          },
        },
        extensions = {
          file_browser = {
            theme = "dropdown",
            hidden = true,
            follow_symlinks = true,
            path = "%:p:h",
            hide_parent_dir = true,
            prompt_path = true,
            grouped = true,
            select_buffer = true,
            cwd_to_path = true,
          },
          fzf = {
            fuzzy = false,
            override_generic_sorter = true,
            override_file_sorter = true,
            -- case_mode = "respect_case",
          },
        },
      })

      -- Load extensions safely
      pcall(telescope.load_extension, "file_browser")
      pcall(telescope.load_extension, "fzf")
      pcall(telescope.load_extension, "nvim-tree/nvim-web-devicons")
      -- pcall(telescope.load_extension, "bookmarks")
      -- pcall(telescope.load_extension, "workspaces")
    end,
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    lazy = true,
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    -- lazy = true,
    -- event = "BufRead",
    -- build = 'make',
    build = "make",
    cond = function()
      return vim.fn.executable("make") == 1
    end,
    event = "VeryLazy", -- or lazy-load after telescope loaded

    dependencies = { "nvim-telescope/telescope.nvim" },
    config = function()
      require("telescope").load_extension("fzf")
    end,
  },
}
