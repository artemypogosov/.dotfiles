-- Library of 40+ independent Lua modules improving Neovim experience with minimal effort

return {
  "echasnovski/mini.nvim",
  version = false,
  config = function()
    require("mini.pairs").setup()
    require("mini.comment").setup()
    require("mini.move").setup()
    require("mini.sessions").setup()
    require("mini.icons").setup()
  end,
}
