local helpers = require("helpers")
local wk = require("which-key")

local trouble_loaded = false

local function ensure_trouble()
	if not trouble_loaded then
		helpers.add({ "folke/trouble.nvim" })

		require("trouble").setup({
			focus = true,
			modes = {
				symbols = {
					win = {
						position = "bottom",
						size = 20,
					},
					format = "{kind_icon} {symbol.name} {pos}",
				},
				quickfix = {
					win = {
						position = "bottom",
						size = 20,
					},
				},
			},
		})

		trouble_loaded = true
	end
end

local modes = { "n", "t" }

--- Ensure Trouble is loaded and execute the specified Trouble command
--- @param cmd_name string The Trouble command
local function trouble(cmd_name)
	ensure_trouble()
	vim.cmd(cmd_name)
end

wk.add({
	{
		mode = modes,
		{
			"<leader>cs",
			function()
				trouble("Trouble symbols toggle")
			end,
			desc = "File symbols",
		},
		{
			"<leader>cl",
			function()
				trouble("Trouble lsp toggle win.position=right win.size=55")
			end,
			desc = "Defs/Refs/Impl...",
		},
		{
			"<leader>cx",
			function()
				trouble("Trouble diagnostics toggle filter.buf=0 win.size=15")
			end,
			desc = "Buffer Diagnostics",
		},
		{
			"<leader>cX",
			function()
				ensure_trouble()
				trouble("Trouble diagnostics toggle win.size=15")
			end,
			desc = "Project Diagnostics",
		},
	},
	{
		"<leader>sta",
		function()
			ensure_trouble()
			trouble("Trouble todo toggle filter = {tag = {TODO,FIX,FIXME,WARN,WARNING}} win.size=15")
		end,
		desc = "List all TODO/FIXME/WARN",
	},
})
