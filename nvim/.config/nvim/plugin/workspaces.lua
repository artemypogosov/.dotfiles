local helpers = require("helpers")
local wk = require("which-key")

-- A simple plugin to manage workspace directories in neovim
helpers.add({ "natecraddock/workspaces.nvim" })

require("workspaces").setup({
	hooks = {
		open = function()
			local path = require("workspaces").path()
			if path then
				vim.cmd("tcd " .. path)
				require("snacks").picker.files()
			end
		end,
	},
})

local modes = { "n", "t" }

wk.add({
	{
		mode = modes,
		{ "<leader>p", group = "project" },
		{ "<leader>pl", helpers.execute("WorkspacesList"), desc = "List workspaces" },
		{ "<leader>pL", helpers.execute("WorkspacesListDirs"), desc = "List projects' dirs" },
		{ "<leader>pa", helpers.prefill("WorkspacesAdd"), desc = "Add project" },
		{ "<leader>pA", helpers.prefill("WorkspacesAddDir"), desc = "Add project dir" },
		{ "<leader>pr", helpers.prefill("WorkspacesRename"), desc = "Rename project" },
		{ "<leader>pd", helpers.prefill("WorkspacesRemove"), desc = "Remove project" },
		{ "<leader>pD", helpers.prefill("WorkspacesRemoveDir"), desc = "Remove project dir" },
	},
})
