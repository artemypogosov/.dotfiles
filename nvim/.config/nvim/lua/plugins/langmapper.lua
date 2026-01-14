local en = [[abcdefghijklmnopqrstuvwxyz+-)(*]]
local en_shift = [[ABCDEFGHIJKLMNOPQRSTUVWXYZ+-)(*]]

local ru = [[ёйцукенгшщзхъфывапролджэячсмить]]
local ru_shift = [[ЁЙЦУКЕНГШЩЗХЪФЫВАРРОЛДЖЭЯЧСМИТЬ]]

local ua = [[йцукенгшщзхїфівапролджєячсмить*]]
local ua_shift = [[ЙЦУКЕНГШЩЗХЇФІВАПРОЛДЖЄЯЧСМИТЬ*]]

return {
  "Wansmer/langmapper.nvim",
  lazy = false,
  priority = 100,
  config = function()
    local langmapper = require("langmapper")

    langmapper.setup({
      layouts = {
        ru = {
          default_layout = en .. en_shift,
          layout = ru .. ru_shift,
        },
        ua = {
          default_layout = en .. en_shift,
          layout = ua .. ua_shift,
        },
      },
      hack_keymap = true,
      map_all_ctrl = true,
      use_layouts = { "ru", "ua" },
    })
  end,
}
