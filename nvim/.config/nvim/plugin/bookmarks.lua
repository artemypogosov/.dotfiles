local helpers = require("helpers")
local wk = require("which-key")

-- A Bookmarks Plugin With Global File Store
helpers.add({ "tomasky/bookmarks.nvim" })

local bookmarks = require("bookmarks")

bookmarks.setup({
	save_file = vim.fn.expand("$HOME/.cache/nvim/.bookmarks"),
})

wk.add({
	{ "<leader>m", group = "marks" },

	{ "<leader>ms", bookmarks.bookmark_toggle, desc = "Set mark", mode = "n" },
	{ "<leader>md", helpers.delete_all_bookmarks, desc = "Delete all marks", mode = "n" },
	{
		"<leader>mf",
		function()
			require("custom.snacks.bookmark_picker").open()
		end,
		desc = "Find bookmarks",
		mode = "n",
	},
	{ "mm", bookmarks.bookmark_next, desc = "Next mark", mode = "n" },
	{ "m]", bookmarks.bookmark_next, desc = "Next mark", mode = "n" },
	{ "mn", bookmarks.bookmark_prev, desc = "Prev mark", mode = "n" },
	{ "m[", bookmarks.bookmark_prev, desc = "Prev mark", mode = "n" },
})
