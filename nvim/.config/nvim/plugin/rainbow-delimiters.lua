local helpers = require("helpers")

-- Rainbow delimiters for Neovim with Tree-sitter
helpers.add({ "HiPhish/rainbow-delimiters.nvim" })

local status_ok, rainbow_delimiters = pcall(require, "rainbow-delimiters")
if status_ok then
	vim.g.rainbow_delimiters = {
		strategy = {
			[""] = rainbow_delimiters.strategy["global"],
			html = nil,
			xml = nil,
		},
		query = {
			[""] = "rainbow-delimiters",
			lua = "rainbow-blocks",
		},
	}
end
