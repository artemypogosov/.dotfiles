local helpers = require("helpers")

-- Neovim's answer to the mouse
helpers.add({ "andyg/leap.nvim" }, "codeberg")

local wk = require("which-key")

wk.add({
	{ "s", "<Plug>(leap)", desc = "Leap", mode = { "n", "x", "o" } },
	{ "S", "<Plug>(leap-from-window)", desc = "Leap from multiple windows", mode = "n" },
})
