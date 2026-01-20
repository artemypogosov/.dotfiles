-- Use your Neovim like using Cursor AI IDE!

-- Some keys to remember:
-- Ctrl + n/p --> next/prev prompt
-- A/a --> apply all/cursor
-- r/e --> retry/edit user previous request
-- @/d --> add/remove file
return {
  "yetone/avante.nvim",
  build = vim.fn.has("win32") ~= 0 and "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false"
    or "make",
  event = "VeryLazy",
  version = false, -- Never set this value to "*"! Never!
  ---@module 'avante'
  ---@type avante.Config
  opts = {
    mode = "legacy",
    -- This file can contain specific instructions for your project
    instructions_file = "avante.md",
    provider = "gemini", -- openai, gemini
    providers = {
      openai = {
        endpoint = "https://api.openai.com/v1",
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
      -- "nvim-mini/mini.pick", -- for file_selector provider mini.pick
      -- "hrsh7th/nvim-cmp", -- autocompletion for avante commands and mentions
      -- "ibhagwan/fzf-lua", -- for file_selector provider fzf
      "stevearc/dressing.nvim", -- for input provider dressing
      "folke/snacks.nvim", -- for input provider snacks
      "echasnovski/mini.icons", -- or nvim-tree/nvim-web-devicons
      "zbirenbaum/copilot.lua", -- for providers='copilot'
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
