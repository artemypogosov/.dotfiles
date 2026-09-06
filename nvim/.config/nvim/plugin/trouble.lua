local helpers = require("helpers")
local wk = require("which-key")

-- A pretty diagnostics, references, telescope results, quickfix and location list to help you
-- solve all the trouble your code is causing.
helpers.add({ "folke/trouble.nvim" })

require("trouble").setup({
	focus = true,
	modes = {
		symbols = {
			win = {
				position = "bottom",
				size = 20,
			},
			format = "{kind_icon} {symbol.name} {pos}",
		},
		quickfix = {
			win = {
				position = "bottom",
				size = 20,
			},
		},
	},
})

local modes = { "n", "t" }

wk.add({
	{
		mode = modes,
		{
			"<leader>cs",
			helpers.execute("Trouble symbols toggle"),
			desc = "File symbols",
		},
		{
			"<leader>cl",
			helpers.execute("Trouble lsp toggle win.position=right win.size=55"),
			desc = "Defs/Refs/Impl...",
		},
		{
			"<leader>cx",
			helpers.execute("Trouble diagnostics toggle filter.buf=0 win.size=15"),
			desc = "Buffer Diagnostics",
		},
		{
			"<leader>cX",
			helpers.execute("Trouble diagnostics toggle win.size=15"),
			desc = "Project Diagnostics",
		},
	},
	{
		"<leader>sta",
		helpers.execute("Trouble todo toggle filter = {tag = {TODO,FIX,FIXME,WARN,WARNING}} win.size=15"),
		desc = "List all TODO/FIXME/WARN",
	},
})
