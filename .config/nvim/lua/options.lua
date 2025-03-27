require "nvchad.options"

local opt = vim.opt
local o = vim.o
local g = vim.g

o.number= true
o.relativenumber = false

opt.ignorecase = false
opt.whichwrap = "<,>,[,],b,s"
opt.termguicolors = true

vim.api.nvim_set_hl(0, "Sneak", { fg = "#FBF1C7", bg = "#928374" })
vim.api.nvim_set_hl(0, "SneakCurrent", { fg = "#D79921", bg = "NONE", bold = true })
