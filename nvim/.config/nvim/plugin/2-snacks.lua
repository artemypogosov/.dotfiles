local helpers = require("helpers")

-- A collection of QoL (Quality Of Life) plugins
helpers.add({ "folke/snacks.nvim" })

local wk = require("which-key")
local snacks = require("snacks")
local man_picker = require("custom.snacks.man_picker")

snacks.setup({
	-- Open remote repo
	gitbrowse = {},
	-- Create and toggle floating/split terminals
	terminal = {},
	-- File explore
	explorer = {},
	-- Indent lines
	indent = {},
	-- Enhanced notifications
	notifier = {},
	-- Triggers when the file is larger than the configured size. (1.5Mb by default)
	-- This automatically prevents things like LSP and Treesitter attaching to the buffer.
	bigfile = {},
	-- When doing nvim somefile.txt, it will render the file as quickly as possible,
	-- before loading your plugins.
	quickfile = {},
	-- Image viewer
	image = {},
	-- Adds ability to blame a single line
	git = {},
	-- Show LSP references and quickly navigate between them
	words = { debounce = 150, modes = { "n" } },
	-- Scratch buffer
	scratch = {
		win = {
			width = 170,
			height = 50,
		},
	},
	-- Zen mode for pure focus
	zen = {
		win = {
			backdrop = { transparent = false },
		},
		toggles = {
			dim = false,
			mini_diff_signs = true,
		},
	},
	-- Pickers config
	picker = {
		sources = {
			-- Left side file tree
			explorer = {
				hidden = true,
				win = {
					list = {
						keys = {
							-- Mimic treemacs
							["o"] = false,
							["y"] = false,
							["<Esc>"] = false,

							-- Selection
							["<TAB>"] = "select_and_next",
							["<S-TAB>"] = "select_and_prev",
							["<C-A>"] = "select_all",

							["<C-Q>"] = "qflist",

							-- Open
							["l"] = "confirm",
							["oo"] = { "confirm", desc = "Open" },
							["ov"] = { "edit_vsplit", desc = "Open vertical" },
							["oh"] = { "edit_split", desc = "Open horizontal" },

							["oaa"] = { { "pick_win", "jump" }, desc = "Open ace" },
							["oav"] = { { "pick_win", "edit_vsplit" }, desc = "Open ace vertical" },
							["oah"] = { { "pick_win", "edit_split" }, desc = "Open ace horizontal" },

							-- Manage files
							["cf"] = { { "pick_win", "edit_split" }, desc = "Add file/dir" },
							["a"] = { { "pick_win", "edit_split" }, desc = "Add file/dir" },

							["R"] = "explorer_rename",
							["d"] = "explorer_del",

							["yf"] = { "explorer_yank", mode = { "n", "x" } },
							["p"] = "explorer_paste",

							-- Yank paths
							["ya"] = function()
								-- Safely fetch the active explorer picker instance
								local picker = snacks.picker.get({ source = "explorer" })[1]
								if picker then
									local item = picker:current()
									if item and item.file then
										vim.fn.setreg("+", item.file)
										vim.notify("Yanked Absolute Path: " .. item.file)
									end
								end
							end,

							["yr"] = function()
								-- Safely fetch the active explorer picker instance
								local picker = snacks.picker.get({ source = "explorer" })[1]
								if picker then
									local item = picker:current()
									if item and item.file then
										-- Compute relative path from current working directory
										local rel_path = vim.fn.fnamemodify(item.file, ":.")
										vim.fn.setreg("+", rel_path)
										vim.notify("Yanked Relative Path: " .. rel_path)
									end
								end
							end,

							-- Toggle files
							["th"] = "toggle_hidden",
							["ti"] = "toggle_ignored",

							-- Git
							["]g"] = "explorer_git_next",
							["[g"] = "explorer_git_prev",

							-- Diagnostic
							["]d"] = "explorer_diagnostic_next",
							["[d"] = "explorer_diagnostic_prev",
							["]w"] = "explorer_warn_next",
							["[w"] = "explorer_warn_prev",
							["]e"] = "explorer_error_next",
							["[e"] = "explorer_error_prev",

							-- Other
							["Z"] = "explorer_close_all",
							["P"] = "toggle_preview",
							["u"] = "explorer_update",
							["<c-t>"] = "terminal",
							["."] = "explorer_focus",
							["<BS>"] = "explorer_up",
							["<M-m>"] = "toggle_maximize",
						},
					},
				},
			},
			-- Search in buffer
			lines = {
				layout = { preset = "select", layout = { height = 0.5 } },
				matcher = {
					fuzzy = false,
				},
			},
			-- Recent files
			recent = {
				layout = { preset = "select", layout = { height = 0.5 } },
				matcher = {
					fuzzy = false,
				},
			},
			grep = {
				hidden = true,
			},
			-- Find files
			files = {
				layout = { preset = "select", layout = { height = 0.5 } },
				ignored = true,
				hidden = true,
				exclude = { ".git", "node_modules", ".cache" },
				matcher = {
					fuzzy = false,
				},
			},
			-- Find buffers
			buffers = {
				current = false,
				sort_lastused = true,
				unloaded = false,
				layout = { preset = "select", layout = { height = 0.25 } },
				matcher = {
					fuzzy = false,
				},
			},
		},
	},
})

local modes = { "n", "t" }

wk.add({
	{
		mode = modes,
		-- File
		{ "<leader>f", group = "file" },
		{ "<leader>fr", helpers.find_recent_files, desc = "Recent files" },
		{
			"<leader>fp",
			function()
				helpers.find_files({ title = "Find file in config", cwd = vim.fn.stdpath("config") })
			end,
			desc = "Find file in config",
		},
		{ "<leader>fd", helpers.find_files_in_current_dir, desc = "Find file from here" },
		{ "<leader><leader>", helpers.find_files, desc = "Find files in project" },
		{ "<leader>fZ", helpers.delete_recent_files, desc = "Delete recent files" },

		-- Window
		{
			"<leader>wM",
			function()
				snacks.zen.zoom()
			end,
			desc = "Maximize window",
		},

		-- Buffers
		{ "<leader>,", helpers.switch_buffers, desc = "Switch buffer", mode = "n" },
		{ "<leader>bk", helpers.kill_buffer, desc = "Kill buffer", mode = "n" },
		{ "<leader>bO", helpers.kill_all_buffers_except_current, desc = "Kill all buffers except current", mode = "n" },

		-- Git
		{
			"<leader>go",
			function()
				snacks.gitbrowse()
			end,
			desc = "Open remote repo",
		},
		{
			"<leader>gC",
			function()
				snacks.picker.git_diff()
			end,
			desc = "Changes [float]",
			mode = "n",
		},
		{
			"<leader>gl",
			function()
				snacks.picker.git_log_file()
			end,
			desc = "File log",
			mode = "n",
		},
		{
			"<leader>gL",
			function()
				local file = vim.fn.expand("%:p")
				local root = vim.fs.root(file, { ".git" })
				if not root then
					vim.notify("Not inside a git repository", vim.log.levels.WARN)
					return
				end

				local cwd = vim.fn.getcwd()
				vim.cmd("lcd " .. root)
				snacks.picker.git_log()
				vim.cmd("lcd " .. cwd)
			end,
			desc = "Branch log",
			mode = "n",
		},
		{
			"<leader>gb",
			function()
				snacks.picker.git_branches()
			end,
			desc = "Switch branch",
			mode = "n",
		},

		-- Scratch
		{
			"<leader>x",
			function()
				snacks.scratch()
			end,
			desc = "Toggle scratch buffer",
		},

		{ "<leader>bx", group = "scratch" },
		{ "<leader>bxs", snacks.scratch.select, desc = "Select scratch" },
		{ "<leader>bxd", helpers.delete_scratch_files, desc = "Delete all scratch files" },

		-- Project [custom picker]
		{ "<leader>pp", helpers.switch_project, desc = "Switch project" },

		-- Toggle
		{
			"<leader>tt",
			function()
				snacks.explorer({ hidden = true })
			end,
			desc = "Toggle sidebar",
		},
		{ "<leader>tT", helpers.snacks_explorer_focus, desc = "Focus  sidebar" },
		{ "<leader>ti", helpers.indent_lines, desc = "Indent lines" },
		{
			"<leader>tZ",
			function()
				snacks.zen()
			end,
			desc = "Zen mode",
		},

		-- Open
		{ "<leader>o", group = "open" },
		{ "<leader>ot", snacks.terminal.toggle, desc = "Toggle terminal" },
		{ "<leader>oT", snacks.terminal.open, desc = "New terminal" },

		-- Help
		{
			mode = "n",
			"<leader>hn",
			function()
				snacks.notifier.show_history()
			end,
			desc = "Notifications",
		},
		{
			"<leader>hc",
			function()
				snacks.picker.command_history()
			end,
			desc = "Command history",
		},
		{
			"<leader>hh",
			function()
				snacks.picker.help()
			end,
			desc = "Help pages",
		},
		{
			"z=",
			function()
				snacks.picker.spelling()
			end,
			desc = "Spelling suggestions",
		},

		-- Search
		{ "<leader>s", group = "search" },
		{ "<leader>ss", helpers.search_buffer, desc = "Search in buffer" },
		{ "<leader>sS", helpers.search_opened_buffers, desc = "Search all open buffers" },
		{ "<leader>/", helpers.search_project, desc = "Search in project" },
		{
			"<leader>sm",
			function()
				man_picker.open()
			end,
			desc = "Man pages",
		},
		{
			"<leader>su",
			function()
				snacks.picker.undo()
			end,
			desc = "Search undo history",
		},
		{
			"<leader>s'",
			function()
				snacks.picker.registers()
			end,
			desc = "Registers",
		},
		{
			"<leader>sw",
			function()
				snacks.picker.grep_word()
			end,
			desc = "Search word at point/visual",
			-- Mode will be overrided in this case
			mode = { "n", "x" },
		},
	},

	-- Jump between matching underlined words
	{
		mode = "n",
		"]]",
		function()
			snacks.words.jump(1, true)
		end,
		desc = "Next word reference",
	},
	{
		"[[",
		function()
			snacks.words.jump(-1, true)
		end,
		desc = "Prev word reference",
	},
})

local api = vim.api

-- Force custom underline highlights for LSP and Snacks words across color scheme changes
api.nvim_create_autocmd("ColorScheme", {
	desc = "Clean up LSP highlights and set Snacks underlines",
	callback = function()
		local hl_groups = {
			"LspReferenceText",
			"LspReferenceRead",
			"LspReferenceWrite",
			"SnacksWords",
			"SnacksWordsRead",
			"SnacksWordsWrite",
		}
		for _, group in ipairs(hl_groups) do
			api.nvim_set_hl(0, group, { underline = true, bg = "none", force = true })
		end
	end,
})
