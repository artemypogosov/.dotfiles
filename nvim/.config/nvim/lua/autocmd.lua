-- Save file on focus lost
vim.api.nvim_create_autocmd("FocusLost", {
	pattern = "*",
	command = "silent! update", -- Saves only if there are changes
})

-- Use Alt+n to switch workspaces
for i = 1, 9 do
	vim.keymap.set("n", string.format("<A-%s>", i), function()
		local tab_count = vim.fn.tabpagenr("$")
		if i <= tab_count then
			vim.cmd(i .. "tabnext")
		else
			vim.notify(
				string.format("Tab %d does not exist (only %d open)", i, tab_count),
				vim.log.levels.WARN,
				{ title = "Tabs" }
			)
		end
	end, { desc = "Go to tab " .. i })
end

-- Highlight text for some time after yanking
-- Define a visible highlight group and use it in the TextYankPost autocmd
vim.api.nvim_set_hl(0, "YankHighlight", { bg = "#7c6f64", fg = "#282828" })

vim.api.nvim_create_autocmd("TextYankPost", {
	group = vim.api.nvim_create_augroup("YankHighlightGroup", { clear = true }),
	pattern = "*",
	callback = function()
		vim.hl.on_yank({ higroup = "YankHighlight", timeout = 200 })
	end,
	desc = "Highlight yanked text",
})

-- LSP-integrated file renaming in Oil (using snacks.rename)
vim.api.nvim_create_autocmd("User", {
	pattern = "OilActionsPost",
	callback = function(event)
		if not _G.Snacks then
			return
		end

		local parse = require("oil").parse_url
		local actions = event.data and event.data.actions or {}

		for _, action in ipairs(actions) do
			if action.type == "move" then
				local src = parse and parse(action.src_url) or action.src_url
				local dest = parse and parse(action.dest_url) or action.dest_url
				Snacks.rename.on_rename_file(src, dest)
			end
		end
	end,
})

-- Open QuickFix buffer after :grep
-- Use [:grep pattern] to search across a project and show result in quickfix list.
-- Alternative to SPC + /
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	pattern = "grep",
	callback = function()
		-- Only open Trouble if there are actually items in the quickfix list
		if #vim.fn.getqflist() > 0 then
			-- Close the standard quickfix if it accidentally opened
			vim.cmd("cclose")
			-- Open Trouble's quickfix view
			vim.cmd("Trouble quickfix open")
		end
	end,
})

-- Disable Copilot by default at startup if it is installed
vim.api.nvim_create_autocmd("VimEnter", {
	callback = function()
		if vim.fn.exists(":Copilot") == 2 then
			vim.cmd("Copilot disable")
		end
	end,
})

--- Automatically updates Tree-sitter parsers upon "nvim-treesitter" plugin update
--- to prevent syntax breaking.
vim.api.nvim_create_autocmd("PackChanged", {
	callback = function(ev)
		local name, kind = ev.data.spec.name, ev.data.kind
		if name == "nvim-treesitter" and kind == "update" then
			if not ev.data.active then
				vim.cmd.packadd("nvim-treesitter")
			end
			vim.cmd("TSUpdate")
		end
	end,
})

local hl = vim.api.nvim_set_hl

--- Clears SignColumn backgrounds and fixes diagnostic/Todo highlights on colorscheme change.
vim.api.nvim_create_autocmd("ColorScheme", {
	group = vim.api.nvim_create_augroup("clear-signcolumn-bg", { clear = true }),
	callback = function()
		hl(0, "SignColumn", { bg = "NONE" })
		-- Gruvbox red, yellow, blue and aqua-green colors
		hl(0, "DiagnosticSignError", { fg = "#fb4934", bg = "NONE" })
		hl(0, "DiagnosticSignWarn", { fg = "#fabd2f", bg = "NONE" })
		hl(0, "DiagnosticSignInfo", { fg = "#83a598", bg = "NONE" })
		hl(0, "DiagnosticSignHint", { fg = "#8ec07c", bg = "NONE" })
		hl(0, "CursorLineSign", { link = "CursorLine" })
		hl(0, "CursorLineNr", { link = "CursorLine", bold = true })

		hl(0, "Todo", { bg = "NONE", bold = true })
	end,
})
