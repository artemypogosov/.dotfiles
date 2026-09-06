local helpers = require("helpers")
local set_hl = vim.api.nvim_set_hl

-- Lua port of the most famous vim colorscheme
helpers.add({ "ellisonleao/gruvbox.nvim" })
-- sainnhe/gruvbox-material as an alternative

vim.cmd.colorscheme("gruvbox")

-- Mute or change the shortcut prefix highlight group
set_hl(0, "MiniStarterItemPrefix", { fg = "#ebdbb2", bold = true })

-- Fix Neogit heavy background blocks for deleted items
set_hl(0, "NeogitChangeDeleted", { fg = "#cc241d", bg = "NONE" })
set_hl(0, "GitSignsDelete", { fg = "#cc241d", bg = "NONE" })
