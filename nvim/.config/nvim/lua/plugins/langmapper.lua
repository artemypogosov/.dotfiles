-- Set langmap very early
local function escape(str)
  local escape_chars = [[;,."|\]]
  return vim.fn.escape(str, escape_chars)
end

local en = [[`qwertyuiop[]asdfghjkl;'zxcvbnm]]
local ru = [[ёйцукенгшщзхъфывапролджэячсмить]]
local en_shift = [[~QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>]]
local ru_shift = [[ËЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ]]

vim.opt.langmap = vim.fn.join({
  escape(ru_shift) .. ";" .. escape(en_shift),
  escape(ru) .. ";" .. escape(en),
}, ",")

return {
  "Wansmer/langmapper.nvim",
  lazy = false,
  priority = 100, -- ensure early load
  config = function()
    local langmapper = require("langmapper")

    langmapper.setup({
      hack_keymap = true, -- wrap all future keymaps
      map_all_ctrl = true, -- wrap Ctrl-mappings
      use_layouts = { "ru", "ua" },
    })

    -- Do NOT call automapping() to avoid nil errors
    -- All mappings defined after this setup will work automatically
  end,
}
