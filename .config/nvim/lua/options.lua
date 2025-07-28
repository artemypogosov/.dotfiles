local opt = vim.opt
local o = vim.o
local g = vim.g

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
g.mapleader = " "
g.maplocalleader = " "

g.have_nerd_font = true

-- Make line numbers default
o.number = true
o.relativenumber = false

-- Do not move between lines when cursor at the start/end of the line
opt.whichwrap = "<,>,[,],b,s"

-- Tabs and indentation
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.softtabstop = 2
opt.smarttab = true
opt.smartindent = true
opt.autoindent = true
opt.breakindent = true

-- Show which line your cursor is on
opt.cursorline = true

-- Disable top tabline
o.showtabline = 0

-- Decrease update time
vim.o.updatetime = 250

-- Store undos between sessions
opt.undofile = true

-- Enable mouse mode, can be handy for resizing splits
opt.mouse = "a"

-- Don't show the mode, since it's already in the statusline
opt.showmode = false

-- Disable a command line
-- vim.opt.cmdheight = 0

-- Sync clipboard between OS and Neovim.
-- Schedule the setting after `UiEnter` because it can increase startup-time.
-- Remove this option if you want your OS clipboard to remain independent.
vim.schedule(function()
  vim.o.clipboard = "unnamedplus"
end)

-- If performing an operation that would fail due to unsaved changes in the buffer (like `:q`),
-- instead raise a dialog asking if you wish to save the current file(s)
vim.o.confirm = true

opt.ignorecase = true
opt.smartcase = true

-- Verical inform column left to the line numbers
opt.signcolumn = "yes"

opt.splitright = true
opt.splitbelow = true

o.list = true
opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
opt.fillchars = { eob = " " }

opt.inccommand = "split"

opt.scrolloff = 10
