local helpers = require("helpers")
local wk = require("which-key")

-- Syntax aware text-objects, select, move, swap, and peek support.
helpers.add({ "nvim-treesitter/nvim-treesitter-textobjects" })

vim.cmd.packadd("nvim-treesitter")
vim.cmd.packadd("nvim-treesitter-textobjects")

require("nvim-treesitter-textobjects").setup()

wk.add({
	{
		"<A-n>",
		function()
			require("nvim-treesitter-textobjects.swap").swap_next("@parameter.inner")
		end,
		desc = "Swap next parameter",
	},
	{
		"<A-N>",
		function()
			require("nvim-treesitter-textobjects.swap").swap_previous("@parameter.inner")
		end,
		desc = "Swap previous parameter",
	},
})
