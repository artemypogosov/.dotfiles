local helpers = require("helpers")
local wk = require("which-key")

-- An interactive and powerful Git interface for Neovim, inspired by Magit
helpers.add({ "nvim-lua/plenary.nvim", "sindrets/diffview.nvim", "NeogitOrg/neogit" })

require("neogit").setup()

wk.add({
	{ "<leader>gg", helpers.execute("Neogit kind=tab"), desc = "Git status", mode = "n" },
})
