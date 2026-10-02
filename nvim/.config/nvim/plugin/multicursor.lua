local helpers = require("helpers")
local wk = require("which-key")

local multicursor_loaded = false

local function get_multicursor()
	if not multicursor_loaded then
		helpers.add({
			{ src = "jake-stewart/multicursor.nvim", version = "1.0" },
		})

		local mc = require("multicursor-nvim")
		mc.setup()

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

		multicursor_loaded = true
	end
	return require("multicursor-nvim")
end

wk.add({
	-- Add or skip adding a new cursor by matching word/selection
	{
		"<M-d>",
		function()
			get_multicursor().matchAddCursor(1)
		end,
		desc = "Add cursor matching word",
		mode = { "n", "x" },
	},
	{
		"<M-D>",
		function()
			get_multicursor().matchAddCursor(-1)
		end,
		desc = "Add cursor backward matching word",
		mode = { "n", "x" },
	},
	{
		"<M-s>",
		function()
			get_multicursor().matchSkipCursor(1)
		end,
		desc = "Skip cursor matching word",
		mode = { "n", "x" },
	},
	{
		"<M-S>",
		function()
			get_multicursor().matchSkipCursor(-1)
		end,
		desc = "Skip cursor backward matching word",
		mode = { "n", "x" },
	},

	-- Add and remove cursors with control + left click
	{
		"<c-leftmouse>",
		function()
			get_multicursor().handleMouse()
		end,
		desc = "Add/remove cursor with mouse",
		mode = "n",
	},
	{
		"<c-leftdrag>",
		function()
			get_multicursor().handleMouseDrag()
		end,
		desc = "Drag to add cursors",
		mode = "n",
	},
	{
		"<c-leftrelease>",
		function()
			get_multicursor().handleMouseRelease()
		end,
		desc = "Release mouse cursor action",
		mode = "n",
	},

	-- Global actions
	{
		"R",
		function()
			get_multicursor().matchAllAddCursors()
		end,
		desc = "Add cursor for all matches",
		mode = "x",
	},
	{
		"I",
		function()
			get_multicursor().insertVisual()
		end,
		desc = "Insert for visual lines",
		mode = "x",
	},
	{
		"A",
		function()
			get_multicursor().appendVisual()
		end,
		desc = "Append for visual lines",
		mode = "x",
	},
})
