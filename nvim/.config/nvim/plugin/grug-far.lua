local helpers = require("helpers")
local wk = require("which-key")

local grug_far_loaded = false

local function get_grug_far()
	if not grug_far_loaded then
		-- Find And Replace plugin for neovim
		helpers.add({ "MagicDuck/grug-far.nvim" })

		require("grug-far").setup({
			engines = {
				ripgrep = {
					defaults = {
						flags = "--smart-case -g=!node_modules/*",
					},
				},
			},
		})

		grug_far_loaded = true
	end
	return require("grug-far")
end

wk.add({
	{
		"<leader>rR",
		function()
			get_grug_far().open()
		end,
		desc = "Replace [project]",
		mode = "n",
	},
	{
		"<leader>rP",
		function()
			get_grug_far().open({ prefills = { search = vim.fn.expand("<cword>") } })
		end,
		desc = "Replace at point [project]",
		mode = "n",
	},
	{
		"<leader>rp",
		function()
			get_grug_far().open({ prefills = { paths = vim.fn.expand("%"), search = vim.fn.expand("<cword>") } })
		end,
		desc = "Replace at point",
		mode = "n",
	},
})

-- Handy flags to use:
-- -w --> wrap-regexp (only show matches surrounded by word boundaries)
-- -S --> smart-case (lower letters - case insensitive; upper letters - case sensitive)
