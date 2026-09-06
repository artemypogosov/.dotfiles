local helpers = require("helpers")

-- Highly experimental plugin that completely replaces the UI
-- for messages, cmdline and the popupmenu.
helpers.add({ "folke/noice.nvim", "MunifTanjim/nui.nvim" })

require("noice").setup({
	cmdline = {
		view = "cmdline", -- Keeps your classic bottom cmdline layout
	},
	-- Enabled by default, but snacks.notifier will override it
	-- notify = {
	-- 	enabled = false,
	-- },
	lsp = {
		override = {
			["vim.lsp.util.convert_input_to_markdown_lines"] = true,
			["vim.lsp.util.stylize_markdown"] = true,
			["cmp.entry.get_documentation"] = true,
		},
	},
	presets = {
		bottom_search = false,
		long_message_to_split = true,
		inc_rename = false,
		lsp_doc_border = false,
	},
})
