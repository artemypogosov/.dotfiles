local helpers = require("helpers")
local wk = require("which-key")

-- Find And Replace
helpers.add({ "MagicDuck/grug-far.nvim" })

local grug_far = require("grug-far")

grug_far.setup({
	engines = {
		ripgrep = {
			-- extraArgs = "--hidden",
			defaults = {
				flags = "--smart-case -g=!node_modules/*",
			},
		},
	},
})

wk.add({
	{ "<leader>rR", grug_far.open, desc = "Replace [project]", mode = "n" },
	{
		"<leader>rP",
		function()
			grug_far.open({ prefills = { search = vim.fn.expand("<cword>") } })
		end,
		desc = "Replace at point [project]",
		mode = "n",
	},
	{
		"<leader>rp",
		function()
			grug_far.open({ prefills = { paths = vim.fn.expand("%"), search = vim.fn.expand("<cword>") } })
		end,
		desc = "Replace at point",
		mode = "n",
	},
})

-- Handy flags to use:
-- -w --> wrap-regexp (only show matches surrounded by word boundaries)
-- -S --> smart-case (lower letters - case insensitive; upper letters - case sensitive)
