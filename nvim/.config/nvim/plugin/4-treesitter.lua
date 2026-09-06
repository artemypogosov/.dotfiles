local helpers = require("helpers")

-- Nvim Treesitter configurations and abstraction layer
helpers.add({ "nvim-treesitter/nvim-treesitter" })

vim.cmd("packadd! nvim-treesitter")

local ts = require("nvim-treesitter")

ts.setup()

ts.install({
	"html",
	"css",
	"scss",
	"javascript",
	"typescript",
	"vue",
	"sql",
	"dockerfile",
	"git_config",
	"git_rebase",
	"diff",
	"gitattributes",
	"gitcommit",
	"gitignore",
	"graphql",
	"json",
	"json5",
	"jsdoc",
	"markdown",
	"markdown_inline",
	"make",
	"lua",
	"luadoc",
	"clojure",
	"vim",
	"vimdoc",
	"regex",
	"nginx",
	"ssh_config",
	"yaml",
	"toml",
	"xml",
	"xresources",
	"editorconfig",
	"bash",
	"csv",
})

-- Enable Highlighting safely via Neovim core
vim.api.nvim_create_autocmd("FileType", {
	group = vim.api.nvim_create_augroup("TreesitterHighlight", { clear = true }),
	callback = function(args)
		-- Safely try to start treesitter only if a parser exists for this filetype
		pcall(vim.treesitter.start, args.buf)
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	group = vim.api.nvim_create_augroup("TreesitterIndent", { clear = true }),
	callback = function()
		vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
	end,
})
