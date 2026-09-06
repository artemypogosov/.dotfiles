local helpers = require("helpers")
local wk = require("which-key")
local snacks = require("snacks")

-- Quickstart configs for Nvim LSP
helpers.add({
	"neovim/nvim-lspconfig",
	"mason-org/mason.nvim",
	"mason-org/mason-lspconfig.nvim",
	"WhoIsSethDaniel/mason-tool-installer.nvim",
})

require("mason").setup({
	ui = {
		border = "rounded",
		icons = {
			package_installed = "✓",
			package_pending = "➜",
			package_uninstalled = "✗",
		},
	},
})

wk.add({
	{ "<leader>hM", helpers.execute("Mason"), desc = "Mason", mode = "n" },
})

-- LSP Attach Autocmd (Keymaps & Feature Toggles)
vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("modern-lsp-attach", { clear = true }),
	callback = function(event)
		wk.add({ { "<leader>c", group = "code", buffer = event.buf } })

		local map = function(keys, func, desc, mode)
			mode = mode or "n"
			vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = desc })
		end

		-- Code Navigation & Actions
		map("<leader>cr", vim.lsp.buf.rename, "Rename")
		map("<leader>ca", vim.lsp.buf.code_action, "Code action", { "n", "x" })
		map("<leader>c+", vim.diagnostic.open_float, "Diagnostic details", { "n", "x" })

		map("<leader>cd", snacks.picker.lsp_definitions, "Goto Definition")
		map("<leader>cD", snacks.picker.lsp_references, "Goto References")
		map("<leader>ci", snacks.picker.lsp_implementations, "Find implementations")
		map("<leader>ct", snacks.picker.lsp_type_definitions, "Find type definition")

		map("<leader>cO", snacks.picker.treesitter, "Treesitter objects")

		-- Inlay Hints
		local client = vim.lsp.get_client_by_id(event.data.client_id)
		if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
			map("<leader>ch", function()
				vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
			end, "Toggle Inlay Hints")
		end
	end,
})

-- Diagnostic Configuration
vim.diagnostic.config({
	severity_sort = true,
	float = { border = "rounded", source = "if_many" },
	underline = { severity = vim.diagnostic.severity.ERROR },
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = "󰅚",
			[vim.diagnostic.severity.WARN] = "󰀪",
			[vim.diagnostic.severity.INFO] = "󰋽",
			[vim.diagnostic.severity.HINT] = "󰌶",
		},
	},
	virtual_text = {
		current_line = true, -- Ghost-text rules: only current line
		prefix = " ■ ",
		format = function(diagnostic)
			return diagnostic.message
		end,
	},
})

-- Server Configurations & Capabilities
require("lspconfig")

local original_capabilities = vim.lsp.protocol.make_client_capabilities()
local capabilities = require("blink.cmp").get_lsp_capabilities(original_capabilities)

local servers = {
	html = {},
	cssls = {},
	jsonls = {},
	yamlls = {},
	svelte = {},
	vue_ls = {},
	astro = {},
	tailwindcss = {},
	bashls = {},
	basedpyright = {},
	dockerls = {},
	prismals = {},
	eslint = {
		settings = {
			workingDirectories = { mode = "auto" },
		},
	},
	lua_ls = {
		settings = {
			Lua = {
				hint = { enable = true },
				workspace = { checkThirdParty = false },
				telemetry = { enable = false },
				diagnostics = { disable = { "missing-fields" } },
			},
		},
	},
	ts_ls = {
		settings = {
			typescript = {
				inlayHints = {
					includeInlayParameterNameHints = "all",
					includeInlayParameterNameHintsWhenArgumentMatchesName = false,
					includeInlayFunctionParameterTypeHints = true,
					includeInlayVariableTypeHints = true,
					includeInlayPropertyDeclarationTypeHints = true,
					includeInlayFunctionLikeReturnTypeHints = true,
					includeInlayEnumMemberValueHints = true,
				},
			},
			javascript = {
				inlayHints = {
					includeInlayParameterNameHints = "all",
					includeInlayParameterNameHintsWhenArgumentMatchesName = false,
					includeInlayFunctionParameterTypeHints = true,
					includeInlayVariableTypeHints = true,
					includeInlayPropertyDeclarationTypeHints = true,
					includeInlayFunctionLikeReturnTypeHints = true,
					includeInlayEnumMemberValueHints = true,
				},
			},
		},
	},
}

-- Install tools via Mason automatically
local ensure_installed = vim.tbl_keys(servers)

require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

require("mason-lspconfig").setup({
	handlers = {
		function(server_name)
			local config = servers[server_name] or {}
			config.capabilities = vim.tbl_deep_extend("force", {}, capabilities, config.capabilities or {})

			vim.lsp.config(server_name, config)
			vim.lsp.enable(server_name)
		end,
	},
})
