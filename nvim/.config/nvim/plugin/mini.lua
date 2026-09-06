local wk = require("which-key")
local helpers = require("helpers")

-- Library of 45+ independent Lua modules improving Neovim experience with minimal effort
helpers.add({ "nvim-mini/mini.nvim" })

-- Autopairs
require("mini.pairs").setup()
-- Icon provider
require("mini.icons").setup()
-- Comment lines
require("mini.comment").setup({
	mappings = {
		comment = "<C-/>",
		comment_line = "<C-/>",
		comment_visual = "<C-/>",
		textobject = "gc",
	},
})
-- Move any selection in any direction
require("mini.move").setup()
-- Extend f, F, t, T to work on multiple lines.
require("mini.jump").setup()
-- Align text interactively
require("mini.align").setup({
	mappings = {
		start = "<leader>=",
	},
})
-- Add, delete, replace, find, highlight surrounding (like pair of parenthesis, quotes, etc.).
require("mini.surround").setup({
	silent = true,
	mappings = {
		add = "<leader>za", -- Add surrounding e.g., aiw"
		delete = "<leader>zd", -- Delete surrounding e.g., ds"
		highlight = "<leader>zh", -- Highlight surrounding
		replace = "<leader>zr", -- Replace surrounding e.g., rs([
		find = "", -- Find surrounding to right (disabled)
		find_left = "", -- Find surrounding to left (disabled)

		suffix_last = "l",
		suffix_next = "n",
	},
})

wk.add({ { "<leader>z", group = "surround", mode = "n" } })

-- Split and join arguments
require("mini.splitjoin").setup({
	mappings = {
		toggle = "<leader>-",
	},
})

-- Visits
local visits = require("mini.visits")
visits.setup()

wk.add({
	{ "<leader>v", group = "visits", mode = "n" },
	{
		"<leader>va",
		function()
			visits.add_label()
		end,
		desc = "Add label to buffer",
		mode = "n",
	},
	{
		"<leader>vl",
		function()
			visits.select_label("", "")
		end,
		desc = "Select labeled files",
		mode = "n",
	},
	{
		"<leader>vr",
		function()
			visits.remove_label()
		end,
		desc = "Remove label from buffer",
		mode = "n",
	},
	{
		"<leader>vD",
		function()
			-- Interactively select a label to remove from ALL files in the project
			visits.remove_label(nil, "")
			visits.write_index() -- Save changes to disk immediately
		end,
		desc = "Delete label globally",
		mode = "n",
	},
	{
		"<leader>vf",
		function()
			visits.select_path(vim.fn.getcwd())
		end,
		desc = "Select project visited files",
		mode = "n",
	},
	{
		"[v",
		function()
			visits.iterate_paths("backward")
		end,
		desc = "Previous visited file",
		mode = "n",
	},
	{
		"]v",
		function()
			visits.iterate_paths("forward")
		end,
		desc = "Next visited file",
		mode = "n",
	},
})

-- Extend and create a/i textobjects
local ai = require("mini.ai")

ai.setup({
	custom_textobjects = {
		-- Treesitter queries (capitalized to avoid overriding mini.ai's builtin 'f', 'c', etc.)
		F = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }),
		C = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }),
		O = ai.gen_spec.treesitter({ a = "@comment.outer", i = "@comment.inner" }),
		A = ai.gen_spec.treesitter({ a = "@attribute.outer", i = "@attribute.inner" }),
		B = ai.gen_spec.treesitter({ a = "@block.outer", i = "@block.inner" }),
		R = ai.gen_spec.treesitter({ a = "@return.outer", i = "@return.inner" }),

		-- Whole buffer text-object ('yag', 'dag')
		g = function()
			local from = { line = 1, col = 1 }
			local to = {
				line = vim.api.nvim_buf_line_count(0),
				col = math.max(1, #vim.api.nvim_buf_get_lines(0, -2, -1, false)[1] or 1),
			}
			return { from = from, to = to }
		end,
	},
})

-- 'a' - function argument 'cia' 'dia' etc.
-- 'f' - function calls 'cif' 'daf' etc.
-- 'n/l' - next/last targets *an", *in", *al{, *il{
-- 1-9 - bracket navigation with count (levels) di(, di{, d2{

-- g[ / g] - goto left/right g[a, g]a, g[f, etc.
-- gUin" - jump to next "text" and make it uppercase "TEXT"
-- gU2in" - same, but for the second next "text"

-- t - tag vat, vit etc.
-- q - any type of quote "", '', ``
-- b - any type of [], {}, ()

local starter = require("mini.starter")

-- Raw ASCII string (use [=[ ... ]=] so you don't have to escape backslashes)
local ascii_header = [=[To see with eyes unclouded by hate.]=]

starter.setup({
	silent = true,
	header = ascii_header,
	items = {
		starter.sections.sessions(5, true),
		-- starter.sections.recent_files(5, false, false),
	},
	content_hooks = {
		starter.gen_hook.adding_bullet("• "),
		starter.gen_hook.aligning("center", "center"),
	},
	footer = function()
		local count = vim.tbl_count(vim.pack.get())
		return string.format("Loaded %d packages in %.2fms", count, _G.StartupTimeMs or 0)
	end,
})

-- Session management
local session = require("mini.sessions")

session.setup({
	-- ~/.local/share/nvim/mini-sessions/
	directory = vim.fn.stdpath("data") .. "/mini-sessions",
})

wk.add({
	{ "<leader>q", group = "quit/session" },
	{
		mode = { "n" },
		{
			"<leader>qs",
			function()
				helpers.prompt_session_save()
			end,
			desc = "Save session",
		},
		{
			"<leader>ql",
			function()
				session.select("read")
			end,
			desc = "Load session",
		},
		{
			"<leader>qr",
			function()
				session.restart()
			end,
			desc = "Restart & Restore",
		},
		{
			"<leader>qD",
			function()
				session.select("delete")
			end,
			desc = "Delete session",
		},

		{
			"<leader>qD",
			function()
				session.select("delete")
			end,
			desc = "Delete session",
		},
		-- Open mini.starter
		{
			"<leader>os",
			function()
				starter.open()
			end,
			desc = "Open dashboard",
		},
	},
})
