local helpers = require("helpers")
local wk = require("which-key")

-- Git integration for buffers
helpers.add({ "lewis6991/gitsigns.nvim" })

require("gitsigns").setup()

vim.api.nvim_create_user_command("GitsignsBlameToggle", function()
	-- Get current state
	local cur_win = vim.api.nvim_get_current_win()

	-- Check if this specific buffer has a 'blame_window' associated with it
	-- OR if the current window IS the blame window itself
	local tracked_blame_win = vim.t.gitsigns_blame_win
	local source_win = vim.t.gitsigns_source_win

	-- TOGGLE OFF: If blame window is open and valid, close it
	if tracked_blame_win and vim.api.nvim_win_is_valid(tracked_blame_win) then
		vim.api.nvim_win_close(tracked_blame_win, true)

		-- Return focus to the source window if we know where it is
		if source_win and vim.api.nvim_win_is_valid(source_win) then
			vim.api.nvim_set_current_win(source_win)
		end

		-- Clear the tracking
		vim.t.gitsigns_blame_win = nil
		vim.t.gitsigns_source_win = nil
		return
	end

	-- GIT CHECK: Only when trying to OPEN
	if not vim.b.gitsigns_status_dict then
		return
	end

	-- TOGGLE ON: Open Blame
	local original_win = cur_win
	vim.cmd("Gitsigns blame")

	-- Gitsigns opens a new window; we wait a tiny bit to capture its ID
	vim.defer_fn(function()
		local new_win = vim.api.nvim_get_current_win()
		if new_win ~= original_win then
			-- Store the IDs in the Tab scope (vim.t) so they persist
			-- and are accessible from any window in this tab
			vim.t.gitsigns_blame_win = new_win
			vim.t.gitsigns_source_win = original_win
		end
		-- 50ms delay to ensure Gitsigns has finished the split
	end, 50)
end, {})

local modes = { "n", "t" }

wk.add({
	{
		mode = modes,
		{ "<leader>gs", helpers.execute("Gitsigns stage_hunk"), desc = "Stage/Unstage hunk" },
		{ "<leader>gS", helpers.execute("Gitsigns stage_buffer"), desc = "Stage file" },
		{ "<leader>gr", helpers.execute("Gitsigns reset_hunk"), desc = "Reset hunk" },
		{ "<leader>gR", helpers.execute("Gitsigns reset_buffer"), desc = "Reset file" },
		{ "<leader>gv", helpers.execute("Gitsigns select_hunk"), desc = "Select hunk" },

		{ "<leader>g[", helpers.execute("Gitsigns nav_hunk prev"), desc = "Prev hunk" },
		{ "<leader>g]", helpers.execute("Gitsigns nav_hunk next"), desc = "Next hunk" },

		{
			"<leader>gp",
			helpers.execute("Gitsigns preview_hunk_inline"),
			desc = "Preview hunk [inline]",
		},
		{ "<leader>gP", helpers.execute("Gitsigns preview_hunk"), desc = "Previw hunk [popup]" },

		{ "<leader>gd", helpers.execute("Gitsigns diffthis"), desc = "Diff this file" },
		{ "<leader>gc", helpers.execute("Gitsigns setqflist all"), desc = "Changes [pinned]" },

		{ "<leader>gw", helpers.execute("Gitsigns toggle_word_diff"), desc = "Toggle word diff" },

		{ "<M-a>", helpers.execute("GitsignsBlameToggle"), desc = "Git side annotations" },
		{ "<M-A>", helpers.execute("Gitsigns blame_line"), desc = "Git popup annotations" },
	},
})
