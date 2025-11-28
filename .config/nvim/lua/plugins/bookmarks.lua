return {
  -- A Bookmarks Plugin With Global File Store For Neovim Written In Lua
  "tomasky/bookmarks.nvim",
  lazy = true,
  event = "BufRead",
  config = function()
    require("bookmarks").setup({
      save_file = vim.fn.expand("$HOME/.cache/nvim/.bookmarks"),
    })
  end,
}
