local helpers = require("helpers")
local wk = require("which-key")

-- Multiple cursors in neovim
helpers.add({
	{ src = "jake-stewart/multicursor.nvim", version = "1.0" },
})

local mc = require("multicursor-nvim")
mc.setup()

wk.add({
	-- Add or skip adding a new cursor by matching word/selection
	{
		"<M-d>",
		function()
			mc.matchAddCursor(1)
		end,
		desc = "Add cursor matching word",
		mode = { "n", "x" },
	},
	{
		"<M-D>",
		function()
			mc.matchAddCursor(-1)
		end,
		desc = "Add cursor backward matching word",
		mode = { "n", "x" },
	},
	{
		"<M-s>",
		function()
			mc.matchSkipCursor(1)
		end,
		desc = "Skip cursor matching word",
		mode = { "n", "x" },
	},
	{
		"<M-S>",
		function()
			mc.matchSkipCursor(-1)
		end,
		desc = "Skip cursor backward matching word",
		mode = { "n", "x" },
	},

	-- Add and remove cursors with control + left click
	{ "<c-leftmouse>", mc.handleMouse, desc = "Add/remove cursor with mouse", mode = "n" },
	{ "<c-leftdrag>", mc.handleMouseDrag, desc = "Drag to add cursors", mode = "n" },
	{ "<c-leftrelease>", mc.handleMouseRelease, desc = "Release mouse cursor action", mode = "n" },

	-- Global actions
	{ "R", mc.matchAllAddCursors, desc = "Add cursor for all matches", mode = "x" },
	{ "I", mc.insertVisual, desc = "Insert for visual lines", mode = "x" },
	{ "A", mc.appendVisual, desc = "Append for visual lines", mode = "x" },
})

-- Mappings defined in a keymap layer only apply when there are multiple cursors
mc.addKeymapLayer(function(_)
	local l_wk = require("which-key")
	l_wk.add({
		{ "<left>", mc.prevCursor, desc = "Previous cursor", mode = { "n", "x" }, buffer = true },
		{ "<right>", mc.nextCursor, desc = "Next cursor", mode = { "n", "x" }, buffer = true },
		{ "<leader>d", mc.deleteCursor, desc = "Delete main cursor", mode = { "n", "x" }, buffer = true },
		{
			"<esc>",
			function()
				if not mc.cursorsEnabled() then
					mc.enableCursors()
				else
					mc.clearCursors()
				end
			end,
			desc = "Toggle/Clear cursors",
			mode = "n",
			buffer = true,
		},
	})
end)

-- Customize how cursors look
local hl = vim.api.nvim_set_hl

hl(0, "MultiCursorCursor", { reverse = true })
hl(0, "MultiCursorVisual", { link = "Visual" })
hl(0, "MultiCursorSign", { link = "SignColumn" })
hl(0, "MultiCursorMatchPreview", { link = "Search" })
hl(0, "MultiCursorDisabledCursor", { reverse = true })
hl(0, "MultiCursorDisabledVisual", { link = "Visual" })
hl(0, "MultiCursorDisabledSign", { link = "SignColumn" })
