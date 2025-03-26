-- This file needs to have same structure as nvconfig.lua
-- https://github.com/NvChad/ui/blob/v3.0/lua/nvconfig.lua
-- Please read that file to know all available options :(

---@type ChadrcConfig
require("custom.telescope").setup()
require "custom.autocmds"

local M = {}
local git_modules = require("custom.git").modules

M.base46 = {
  theme = "gruvbox",
  theme_toggle = { "gruvbox", "gruvbox_light" },
}

M.nvdash = {
  load_on_startup = true,
  header = {
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣾⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣾⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣦⡀⢰⠒⠒⠦⣀⠴⠒⠒⡆⢀⣴⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⡉⠁⠀⠀⠀⠀⠀⠈⢉⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠀⢀⠔⢺⣿⣿⣿⠿⠛⠛⠉⠀⠀⠀⠀⠀⠀⠀⠉⠛⠛⠿⢿⣿⣿⡟⠢⡄⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⢀⡜⠁⠀⠀⠛⠁⠀⠀⣠⣴⣶⣿⣿⣿⣿⣿⣿⣿⣶⣦⣄⡀⠀⠈⠙⠀⠀⠈⠣⡀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⢀⡎⠀⠀⠀⠀⠀⣀⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⣀⠀⠀⠀⠀⠀⢹⡀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠘⡲⠋⠀⠀⠀⣠⣤⣤⣤⣤⡙⠿⣿⣿⣿⣿⣿⣿⣿⡿⢋⣤⣤⣤⣤⣤⠀⠀⠀⠙⢶⠃⠀⠀⠀⠀",
    "⠀⠀⠀⢠⠞⠀⠀⠀⠀⣼⣿⡿⠟⠛⠿⣿⣧⡘⢿⣿⣿⣿⣿⢋⣴⣿⠿⠛⠻⢿⣿⣧⠀⠀⠀⠀⠱⡄⠀⠀⠀",
    "⠀⠀⢀⠏⠀⠀⠀⠀⢸⡿⠁⣠⣴⣶⣤⡈⠻⣷⡈⣿⣿⣿⠁⣾⠟⢁⣤⣶⣦⣄⠈⢿⡇⠀⠀⠀⠀⠹⡀⠀⠀",
    "⠀⠀⣾⠀⠀⠀⠀⠀⣾⡇⢸⣿⣿⣿⣿⣧⠀⣿⣧⠸⣿⠇⣼⣿⠀⣼⣿⣿⣿⣿⡇⢸⣿⠀⠀⠀⠀⠀⢷⠀⠀",
    "⠀⠀⡇⠀⡀⠀⠀⠀⣿⣧⠈⠿⣿⣿⡿⠃⣰⣿⣿⣦⣤⣴⣿⣿⣆⠘⢿⣿⣿⠿⠁⣼⣿⠀⠀⠀⢀⠀⢸⠀⠀",
    "⠀⠀⠧⠞⣇⠀⠀⠀⢻⣿⣷⣦⣤⣤⣤⣾⣿⡿⠛⠛⠛⠛⠛⠿⣿⣷⣦⣤⣤⣴⣾⣿⡟⠀⠀⠀⢸⠓⠼⠀⠀",
    "⠀⠀⠀⠀⡏⠀⠀⠀⠈⠉⠛⠛⠛⠛⠛⣋⣤⣶⣿⠿⠿⠿⣿⣶⣤⣉⠛⠛⠛⠛⠛⠉⠁⠀⠀⠀⢸⠀⠀⠀⠀",
    "⠀⠀⠀⠀⢣⠀⠀⠀⠀⠸⣿⣿⣿⣿⣿⣿⣿⡟⢁⣴⣶⣦⡈⢻⣿⣿⣿⣿⣿⣿⣿⠏⠀⠀⠀⠀⡸⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠘⡆⠀⠀⠀⠀⠈⢿⣿⣿⣿⣿⣿⡇⣾⣿⣿⣿⣿⢈⣿⣿⣿⣿⣿⡿⠃⠀⠀⠀⠀⢰⠃⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠘⢆⠀⣄⠀⠀⠀⠉⠻⣿⣿⣿⣷⣌⠛⠛⠛⣡⣾⣿⣿⣿⠟⠋⠀⠀⠀⣠⠀⣰⠃⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠈⠓⠛⠑⢤⡀⠀⠀⠀⠈⠙⠛⠛⠻⠿⠿⠛⠛⠋⠉⠀⠀⠀⢀⡤⠊⠙⠚⠁⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠢⢄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⠔⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠒⠤⣀⡀⠀⠀⠀⠀⣀⡤⠖⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
    "⠀⠀ To see with eyes unclouded by hate.⠀⠀ ",
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
  },

  buttons = {
    { txt = "  Find File", keys = "SPC f f", cmd = "Telescope find_files" },
    { txt = "  Recent Files", keys = "SPC f o", cmd = "Telescope oldfiles" },
    { txt = "󰈭  Find Word", keys = "SPC f w", cmd = "Telescope live_grep" },
    -- TODO: complete list
  },
}

M.ui = {
  statusline = {
    theme = "default",
    separator_style = "default",
    order = { "f", "file_status", "%=", "diagnostics", "lsp_msg", "space", "git" },
    modules = {
      f = " %F",
      git = git_modules.statusline.git_custom,
      file_status = function()
        return vim.bo.modified and " [+]" or ""
      end,
      space = " ",
    },
  },

  tabufline = {
    enabled = true,
    lazyload = true,
    order = { "treeOffset", "buffers", "tabs" },
    modules = nil,
    bufwidth = 21,
  },
}

return M
