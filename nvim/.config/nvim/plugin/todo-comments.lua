local wk = require("which-key")
local helpers = require("helpers")

-- Highlight, list and search todo comments in your projects
helpers.add({ "folke/todo-comments.nvim" })

local info = "#B8BB26"
local warning = "#FE8019"
local hint = "#D5C4A1"
local error = "#FB4934"
local test = "#D3869B"
local default = "#FABD2F"

local tc = require("todo-comments")

tc.setup({
	signs = true,
	highlight = { keyword = "fg" },
	search = {
		args = {
			"--color=never",
			"--no-heading",
			"--with-filename",
			"--line-number",
			"--column",
			"--hidden",
			"--glob=!.git/*",
			"--glob=!**/node_modules/*",
		},
	},
	keywords = {
		FIX = {
			icon = " ",
			color = error,
			alt = { "FIXME", "BUG", "FIXIT", "ISSUE" },
		},
		TODO = { icon = " ", color = info },
		HACK = { icon = " ", color = warning },
		WARN = { icon = " ", color = warning, alt = { "WARNING", "XXX" } },
		NOTE = { icon = " ", color = hint, alt = { "INFO" } },
		TEST = { icon = " T", color = test, alt = { "TESTING", "PASSED", "FAILED" } },
		PERF = { icon = " 󰔟", color = default, alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
	},
})

wk.add({
	{ "<leader>st", group = "TODO" },
	{
		"<leader>stt",
		function()
			require("snacks").picker.todo_comments({ keywords = { "TODO" }, hidden = true })
		end,
		desc = "List all TODOS",
		mode = "n",
	},
	{
		"<leader>stf",
		function()
			require("snacks").picker.todo_comments({ keywords = { "FIXME", "FIX" }, hidden = true })
		end,
		desc = "List all FIXME",
		mode = "n",
	},
	{
		"<leader>stw",
		function()
			require("snacks").picker.todo_comments({ keywords = { "WARN", "WARNING" }, hidden = true })
		end,
		desc = "List all WARNING",
		mode = "n",
	},
	{
		"]t",
		function()
			helpers.todo_jump(tc.jump_next)
		end,
		desc = "Next todo comment",
		mode = "n",
	},
	{
		"[t",
		function()
			helpers.todo_jump(tc.jump_prev)
		end,
		desc = "Prev todo comment",
		mode = "n",
	},
})
