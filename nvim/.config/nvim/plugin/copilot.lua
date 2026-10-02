local helpers = require("helpers")
local wk = require("which-key")

-- Neovim plugin for GitHub Copilot
helpers.add({ "github/copilot.vim" })

vim.g.copilot_filetypes = {
	["grug-far"] = false,
	["grug-far-history"] = false,
	["grug-far-help"] = false,
}

local INFO = vim.log.levels.INFO

wk.add({
	{
		"<leader>tc",
		helpers.execute("Copilot enable", function()
			vim.notify("Copilot: enabled", INFO)
		end),
		desc = "Copilot: enable",
		mode = { "n", "v" },
	},
	{
		"<leader>tC",
		helpers.execute("Copilot disable", function()
			vim.notify("Copilot: disabled", INFO)
		end),
		desc = "Copilot: disable",
		mode = { "n", "v" },
	},
	{ "<M-i>", "<Plug>(copilot-suggest)", mode = "i", desc = "Trigger Copilot" },
	{
		"<M-a>",
		'copilot#Accept("\\<CR>")',
		desc = "Copilot Accept",
		expr = true,
		replace_keycodes = false,
		mode = "i",
	},
})
