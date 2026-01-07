-- Set langmap very early for langmapper.lua
-- local function escape(str)
-- local escape_chars = [[;,."|\]]
-- return vim.fn.escape(str, escape_chars)
-- end

-- local en = [[`qwertyuiop[]asdfghjkl;'zxcvbnm]]
-- local ru = [[ёйцукенгшщзхъфывапролджэячсмить]]
-- local en_shift = [[~QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>]]
-- local ru_shift = [[ËЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ]]

-- vim.opt.langmap = vim.fn.join({
-- escape(ru_shift) .. ";" .. escape(en_shift),
-- escape(ru) .. ";" .. escape(en),
-- }, ",")

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Neovim options [options.lua]
require("options")

-- Setup lazy.nvim
require("lazy").setup({
  spec = {
    -- Import plugins
    { import = "plugins" },
  },
  -- Configure any other settings here. See the documentation for more details.
  -- Colorscheme that will be used when installing plugins.
  install = { colorscheme = { "gruvbox" } },
  -- Automatically check for plugin updates
  checker = { enabled = true, notify = false },
})

-- Neovim mappings [mappings.lua]
require("mappings")

-- Neovim autocmd [autocmd.lua]
require("autocmd")
