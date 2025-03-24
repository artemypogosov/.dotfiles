-- This file needs to have same structure as nvconfig.lua 
-- https://github.com/NvChad/ui/blob/v3.0/lua/nvconfig.lua
-- Please read that file to know all available options :( 

---@type ChadrcConfig
require('custom.telescope').setup()
require("telescope").load_extension "file_browser"

local M = {}
local git_modules = require ("custom.git").modules

-- Change a directory when opening a file
vim.api.nvim_create_autocmd("BufEnter", {
  callback = function()
    vim.cmd("silent! cd %:p:h")
  end
})

M.base46 = {
  theme = "gruvbox",
  theme_toggle = { "gruvbox", "gruvbox_light" },
}

M.nvdash = { load_on_startup = true }

M.ui = {
  statusline = {
    theme = "vscode_colored",
    separator_style = "default",
    order = { "f", "file_status", "%=", "lsp_msg", "%=", "lsp", "git" },
    modules = {
      f = " %F",
      git = git_modules.statusline.git_custom,
      file_status = function()
        return vim.bo.modified and " [+]" or ""
      end,
    }
  },
  -- lazyload it when there are 1+ buffers
  tabufline = {
    enabled = true,
    lazyload = true,
    order = { "treeOffset", "buffers", "tabs", "btns" },
    modules = nil,
    bufwidth = 21,
  },
}

return M
