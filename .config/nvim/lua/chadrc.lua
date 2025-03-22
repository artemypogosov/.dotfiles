-- This file needs to have same structure as nvconfig.lua 
-- https://github.com/NvChad/ui/blob/v3.0/lua/nvconfig.lua
-- Please read that file to know all available options :( 

---@type ChadrcConfig
local M = {}

M.base46 = {
	theme = "gruvbox",
    theme_toggle = { "gruvbox", "gruvbox_light" },
}

M.nvdash = { load_on_startup = true }

 M.ui = {
   statusline = {
     theme = "vscode",
     separator_style = "default",
     order = { "mode", "f" , "%=", "lsp_msg", "%=", "lsp", "git" },
     modules = {
       f = "%F",
       --git = "%GitBranch"
     }
   },
 }

return M
