local helpers = require("helpers")
local wk = require("which-key")

local neogit_loaded = false

local function ensure_neogit()
	if not neogit_loaded then
		-- An interactive and powerful Git interface for Neovim, inspired by Magit
		helpers.add({
			"nvim-lua/plenary.nvim",
			"dlyongemallo/diffview-plus.nvim",
			"NeogitOrg/neogit",
		})

		require("neogit").setup()
		neogit_loaded = true
	end
end

wk.add({
	{
		"<leader>gg",
		function()
			ensure_neogit()
			vim.cmd("Neogit kind=tab")
		end,
		desc = "Git status",
		mode = "n",
	},
})
