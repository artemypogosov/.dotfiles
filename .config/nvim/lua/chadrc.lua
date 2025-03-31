-- This file needs to have same structure as nvconfig.lua
-- https://github.com/NvChad/ui/blob/v3.0/lua/nvconfig.lua
-- Please read that file to know all available options :(

--@type ChadrcConfig
require("custom.telescope").setup()
require "custom.autocmds"

local M = {}
local git_modules = require("custom.git").modules

-- Saves window-local options in a session (when session are used)
--vim.opt.ssop:append { "localoptions" }

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
    { txt = "Find File", keys = "ff", cmd = "Telescope file_browser previewer=false path=%:p:h select_buffer=true"},
    { txt = "Recent Files", keys = "fr", cmd = "Telescope oldfiles previewer=false" },
    { txt = "Open project", keys = "fp", cmd = "Telescope workspaces<CR>" },
    { txt = "Org Agenda", keys = "oa", cmd = "Org agenda" },
    { txt = "─", hl = "NvDashFooter", no_gap = true, rep = true },
    {
      txt = function()
        local stats = require("lazy").stats()
        local ms = math.floor(stats.startuptime) .. " ms"
        return " Loaded " .. stats.loaded .. "/" .. stats.count .. " plugins in " .. ms
      end,
      hl = "NvDashFooter",
      no_gap = true,
    },
    { txt = "─", hl = "NvDashFooter", no_gap = true, rep = true },
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
