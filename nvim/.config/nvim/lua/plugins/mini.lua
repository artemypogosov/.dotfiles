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

--- mini.pairs
--- Write pairs for you like () {} "" etc.

--- mini.comment
-- 'ctrl + /' mapped to 'gcc' in both N and V modes
-- 'dgc' - remove comment block
-- 'gcip' - comment inner paragraph

--- mini.move
--- 'alt + j/k' - move lines up and down (also works for selection)
