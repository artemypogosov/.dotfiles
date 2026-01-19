return {
  "yetone/avante.nvim",
  build = vim.fn.has("win32") ~= 0 and "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false"
    or "make",
  event = "VeryLazy",
  version = false, -- Never set this value to "*"! Never!
  ---@module 'avante'
  ---@type avante.Config
  opts = {
    -- this file can contain specific instructions for your project
    mode = "legacy",
    instructions_file = "avante.md",
    provider = "gemini", -- openai, gemini
    providers = {
      openai = {
        endpoint = "https://api.openai.com/v1",
        -- gpt-4o (Higher quality, slower, more expensive)
        model = "gpt-4o-mini",
        timeout = 30000, -- Timeout in milliseconds, increase this for reasoning models
        context_window = 128000, -- Number of tokens to send to the model for context
        extra_request_body = {
          temperature = 0.75,
        },
      },
      gemini = {
        -- gemini-2.5-pro
        model = "gemini-2.5-flash",
        timeout = 30000,
        temperature = 0,
        max_tokens = 8192,
      },
      -- claude = {
      -- endpoint = "https://api.anthropic.com",
      -- model = "claude-sonnet-4-5-20250929",
      -- timeout = 30000,
      -- context_window = 200000,
      -- extra_request_body = {
      -- temperature = 0.75,
      -- max_tokens = 20480, -- 4096
      -- },
      -- },
    },
    -- Add here you custom shortcuts which will expand in a prompt
    shortcuts = {
      {
        name = "refactor",
        description = "Refactor code with best practices",
        details = "Automatically refactor code to improve readability and follow best practices",
        prompt = "Please refactor this code following best practices, improving readability and maintainability while preserving functionality.",
      },
    },
    behaviour = {
      auto_set_keymaps = false,
    },
    input = {
      provider = "snacks",
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      --- The below dependencies are optional,
      "nvim-mini/mini.pick", -- for file_selector provider mini.pick
      "hrsh7th/nvim-cmp", -- autocompletion for avante commands and mentions
      "ibhagwan/fzf-lua", -- for file_selector provider fzf
      "stevearc/dressing.nvim", -- for input provider dressing
      "folke/snacks.nvim", -- for input provider snacks
      "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
      "zbirenbaum/copilot.lua", -- for providers='copilot'
      {
        -- support for image pasting
        "HakonHarnes/img-clip.nvim",
        event = "VeryLazy",
        opts = {
          -- recommended settings
          default = {
            embed_image_as_base64 = false,
            prompt_for_file_name = false,
            drag_and_drop = {
              insert_mode = true,
            },
            -- required for Windows users
            use_absolute_path = true,
          },
        },
      },
      {
        -- Make sure to set this up properly if you have lazy=true
        "MeanderingProgrammer/render-markdown.nvim",
        opts = {
          file_types = { "markdown", "Avante" },
        },
        ft = { "markdown", "Avante" },
      },
    },
  },
}

-----------
-- KEYS --
-----------

-- C-n/p - next/prev prompt

-- diff = {
--   ours = "co",
--   theirs = "ct",
--   all_theirs = "ca",
--   cursor = "cc",
--   next = "]x",
--   prev = "[x",
-- },
-- jump = {
--   next = "]]",
--   prev = "[[",
-- },
-- submit = {
--   normal = "<CR>",
--   insert = "<C-s>",
-- },
-- sidebar = {
--   add_file = "@",
-- }
