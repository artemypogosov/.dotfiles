local helpers = require("helpers")

-- Faster LuaLS setup for Neovim
helpers.add({ "folke/lazydev.nvim" })

require("lazydev").setup({
	library = {
		{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
	},
})
