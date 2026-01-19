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
    "nvim-telescope/telescope.nvim",
  },
}
----------
-- KEYS --
----------

-- edit_with_instructions = {
--   keymaps = {
--     toggle_help = "<C-h>", (list of all keys)
-- },
