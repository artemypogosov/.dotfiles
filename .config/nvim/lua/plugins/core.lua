return {
  "nvim-lua/plenary.nvim",

  -- Theme switcher (<leader>tT)
  "nvzone/volt",

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
}
