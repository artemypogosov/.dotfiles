return {
  -- Rainbow delimiters for Neovim with Tree-sitter
  "HiPhish/rainbow-delimiters.nvim",
  event = "VeryLazy",
  config = function()
    require("rainbow-delimiters.setup").setup({
      strategy = {
        [""] = "rainbow-delimiters.strategy.global", -- default for all
        html = nil,
        xml = nil,
      },
    })
  end,
}
