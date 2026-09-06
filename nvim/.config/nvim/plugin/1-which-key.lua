local helpers = require("helpers")

-- Helps you remember your Neovim keymaps by showing available keybindings in a popup as you type
helpers.add({ "folke/which-key.nvim" })

local wk = require("which-key")

wk.setup({
	preset = "modern",
	plugins = {
		marks = false,
		registers = false,
		spelling = {
			suggestions = 10,
		},
		presets = {
			operators = false,
			motions = false,
			windows = false,
			nav = false,
			z = false,
			g = false,
		},
	},
	icons = {
		mappings = false,
	},
})

local modes = { "n", "t" }

-- Register global keybindings that do not depend on any plugin
wk.add({
	-- Window
	{
		mode = modes,
		{ "<leader>w", group = "window" },
		{ "<leader>ww", "<Cmd>wincmd w<CR>", desc = "Next window" },
		{ "<leader>wh", "<Cmd>wincmd h<CR>", desc = "Switch left" },
		{ "<leader>wl", "<Cmd>wincmd l<CR>", desc = "Switch right" },
		{ "<leader>wj", "<Cmd>wincmd j<CR>", desc = "Switch down" },
		{ "<leader>wk", "<Cmd>wincmd k<CR>", desc = "Switch up" },
		{ "<leader>ws", "<Cmd>wincmd s<CR>", desc = "Split H" },
		{ "<leader>wv", "<Cmd>wincmd v<CR>", desc = "Split V" },
		{ "<leader>wH", "<Cmd>wincmd H<CR>", desc = "Move left" },
		{ "<leader>wL", "<Cmd>wincmd L<CR>", desc = "Move right" },
		{ "<leader>wJ", "<Cmd>wincmd J<CR>", desc = "Move down" },
		{ "<leader>wK", "<Cmd>wincmd K<CR>", desc = "Move up" },
		{ "<leader>wx", "<Cmd>wincmd x<CR>", desc = "Swap with next" },
		{ "<leader>wc", "<Cmd>wincmd c<CR>", desc = "Close" },
		{ "<leader>wC", "<Cmd>wincmd o<CR>", desc = "Close other windows" },
		{ "<leader>w=", "<Cmd>wincmd =<CR>", desc = "Equalize windows" },
	},

	-- Buffer
	{
		mode = modes,
		{ "<leader>b", group = "buffer" },
		{ "<leader>bN", helpers.execute("enew"), desc = "New empty buffer" },
		{ "<leader>b]", helpers.execute("bnext"), desc = "Next buffer" },
		{ "<leader>b[", helpers.execute("bprev"), desc = "Prev buffer" },
	},

	-- Toggle
	{
		mode = modes,
		{ "<leader>t", group = "toggle" },
		{ "<leader>ts", "<Cmd>set spell!<CR>", desc = "Toggle spellcheck" },
		{
			"<leader>tl",
			function()
				vim.o.list = not vim.o.list
				vim.notify("Listchars " .. (vim.o.list and "enabled" or "disabled"), vim.log.levels.INFO)
			end,
			desc = "Toggle listchars (whitespace)",
		},
	},

	-- Workspaces
	{
		mode = modes,
		{ "<leader><Tab>", group = "workspace" },
		{ "<leader><Tab>n", helpers.execute("tabnew"), desc = "New workspace" },
		{
			"<leader><Tab>k",
			function()
				vim.ui.select({ "Yes", "No" }, {
					prompt = "Close current workspace (tab)?",
				}, function(choice)
					if choice == "Yes" then
						vim.cmd("tabclose")
					end
				end)
			end,
			desc = "Delete workspace",
		},
		{ "<leader><Tab>]", helpers.execute("tabnext"), desc = "Next workspace" },
		{ "<leader><Tab>[", helpers.execute("tabNext"), desc = "Prev workspace" },
		{ "<leader><Tab>r", helpers.prefill("LualineRenameTab"), desc = "Rename workspace" },
	},

	-- Git
	{ "<leader>g", group = "git" },

	-- Replace
	{
		mode = { "n", "x" },
		{ "<leader>r", group = "replace" },
		-- Ctrl+f opens a temporary mini-buffer for ease of use
		{ "<leader>rr", helpers.search_replace, desc = "Replace" },
		{ "<leader>r?", helpers.search_replace_menu, desc = "Show all matches" },
	},

	-- Help
	{ "<leader>h", group = "help" },
	{ "<leader>hp", group = "vimpack" },

	-- Other
	{
		mode = modes,
		{
			"<leader>hf",
			function()
				print("Filetype:", vim.bo.filetype)
			end,
			desc = "Print filetype",
		},

		{ "<C-c>", "<Cmd>%y+<CR>", desc = "Copy entire buffer" },

		{ "<Esc>", "<Cmd>noh<CR>", desc = "Clean search highlights" },
		{ "<leader>`", "<Cmd>messages<CR>", desc = "Messages" },
		{ "<leader>qq", "<Cmd>qa<CR>", desc = "Quit" },

		-- [[ / ]] - move between plugin sections
		{
			"<leader>hpu",
			function()
				vim.pack.update()
			end,
			desc = "Update plugins",
		},
		{ "<leader>hp?", "<Cmd>checkhealth vim.pack<CR>", desc = "Check vim.pack" },
		-- Remove plugin's file and call this mapping
		{
			"<leader>hpd",
			function()
				vim.ui.input({ prompt = "Plugins to delete (comma-separated): " }, function(input)
					if input and input ~= "" then
						local packs = {}
						for item in string.gmatch(input, "([^,]+)") do
							local clean_item = item:match("^%s*(.-)%s*$")
							if clean_item ~= "" then
								table.insert(packs, clean_item)
							end
						end

						if #packs > 0 then
							vim.pack.del(packs)
							vim.notify("Deleted packages: " .. table.concat(packs, ", "), vim.log.levels.INFO)
						end
					end
				end)
			end,
			desc = "Delete plugins",
		},
	},

	{ "<C-s>", "<Cmd>w<CR>", desc = "Save current buffer", mode = modes },
	{ "<C-s>", "<Esc><Cmd>w<CR>", desc = "Save current buffer", mode = "i" },
})
