local helpers = require("helpers")
local wk = require("which-key")

-- Neovim file explorer: edit your filesystem like a buffer
helpers.add({ "stevearc/oil.nvim" })

require("oil").setup({
	keymaps = {
		["g?"] = "actions.show_help",
		["<CR>"] = "actions.select",
		["l"] = "actions.select", -- Open file/directory (like Dired RET)
		["h"] = "actions.parent", -- Go up to parent directory (like Dired ^)
		["-"] = "actions.parent", -- Alternative parent directory shortcut
		["_"] = "actions.open_cwd", -- Set current working directory to current oil directory
		["`"] = "actions.cd", -- Change directory interactively
		["~"] = "actions.tcd", -- Change tab-local directory
		["gs"] = "actions.change_sort", -- Change sort method
		["gx"] = "actions.open_external", -- Open external app / system default handler
		["g."] = "actions.toggle_hidden", -- Toggle hidden files visibility (like dired-omit-mode)
		["g\\"] = "actions.toggle_trash", -- Toggle trash
	},
})

wk.add({
	{ "<leader>.", helpers.execute("Oil"), desc = "Find file", mode = "n" },
})
