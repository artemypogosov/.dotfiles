-- ChatGPT Neovim Plugin: Effortless Natural Language Generation with OpenAI's ChatGPT API

-- Some keys to remember:
-- Edit with instructions window:
-- Ctrl+h -  toggle_help (list of all keys)

return {
  "jackMort/ChatGPT.nvim",
  event = "VeryLazy",
  config = function()
    require("chatgpt").setup({
      -- This config assumes you have OPENAI_API_KEY environment variable set
      openai_params = {
        model = "gpt-4o-mini",
        frequency_penalty = 0,
        presence_penalty = 0,
        max_completion_tokens = 4095,
        n = 1,
        -- temperature = 0.2,
        -- top_p = 0.1,
      },
      edit_with_instructions = {
        keymaps = {
          close_n = "q",
        },
      },
      popup_input = {
        placeholder = "",
      },
      -- default
      context = {
        project = {
          enabled = true,
          auto_detect = true,
          context_files = {
            ".chatgpt.md",
            ".cursorrules",
            ".github/copilot-instructions.md",
          },
        },
      },
    })
  end,
  dependencies = {
    "MunifTanjim/nui.nvim",
    "nvim-lua/plenary.nvim",
    "folke/trouble.nvim", -- optional
    -- Looks like ChatGPT.nvim is the only plugin in my config which uses telescope.nvim as a dependency
    "nvim-telescope/telescope.nvim",
  },
}
