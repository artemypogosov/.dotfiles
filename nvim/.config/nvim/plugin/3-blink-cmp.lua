local helpers = require("helpers")

-- Performant, batteries-included completion plugin
helpers.add({
	"rafamadriz/friendly-snippets",
	"Kaiser-Yang/blink-cmp-avante",
	{ "saghen/blink.cmp", version = vim.version.range("1.x") },
})

---@module 'blink.cmp'
---@type blink.cmp.Config
require("blink.cmp").setup({
	-- 'default' (recommended) for mappings similar to built-in completions (C-y to accept)
	-- 'super-tab' for mappings similar to vscode (tab to accept)
	-- 'enter' for enter to accept
	-- 'none' for no mappings
	--
	-- All presets have the following mappings:
	-- C-space: Open menu or open docs if already open
	-- C-n/C-p or Up/Down: Select next/previous item
	-- C-e: Hide menu
	-- C-k: Toggle signature help (if signature.enabled = true)
	--
	-- See :h blink-cmp-config-keymap for defining your own keymap
	keymap = {
		preset = "super-tab",
		["<Enter>"] = { "accept", "fallback" },
	},

	appearance = {
		-- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
		-- Adjusts spacing to ensure icons are aligned
		nerd_font_variant = "mono",
	},

	-- (Default) Only show the documentation popup when manually triggered
	completion = {
		ghost_text = {
			enabled = false,
		},
		documentation = { auto_show = true, auto_show_delay_ms = 500 },
		menu = {
			draw = {
				columns = {
					{ "kind_icon", gap = 1, "label" },
				},
			},
		},
	},
	signature = { enabled = false },

	-- Default list of enabled providers defined so that you can extend it
	-- elsewhere in your config, without redefining it, due to `opts_extend`
	sources = {
		default = { "avante", "lsp", "path", "snippets" },
		-- Place your providers here
		providers = {
			avante = {
				module = "blink-cmp-avante",
				name = "Avante",
				opts = {
					-- options for blink-cmp-avante
				},
			},
			-- How to create a custom snippet:
			-- 1. mkdir -p ~/.config/nvim/snippets
			-- 2. e.g., ~/.config/nvim/snippets/javascript.json
			-- 3. {
			--   "Console Table": {
			--     "prefix": "ct",
			--     "body": [
			--       "console.table(${1:data});"
			--     ],
			--     "description": "Log table to console"
			--   }
			-- }
			--
			-- 4. Add this under 'module'
			-- opts = {
			-- --Tell blink where your custom JSON snippets live
			-- search_paths = { vim.fn.stdpath("config") .. "/snippets" },
			-- },
			snippets = {
				name = "Snippets",
				module = "blink.cmp.sources.snippets",
				transform_items = function(_, items)
					for _, item in ipairs(items) do
						-- Set filter_text to "q" + original label so typing "qlog" matches it
						local orig_label = item.label
						if not item.filter_text then
							item.filter_text = "q" .. orig_label
						elseif not item.filter_text:find("^q") then
							item.filter_text = "q" .. item.filter_text
						end

						-- Update display label in completion menu to show "qlog"
						if item.label and not item.label:find("^q") then
							item.label = "q" .. item.label
						end

						-- DO NOT touch item.insertText or item.textEdit!
						-- Leaving those intact ensures "qlog" expands to clean "console.log()".
					end
					return items
				end,
			},
		},
	},

	-- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
	-- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
	-- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
	--
	-- See the fuzzy documentation for more information
	fuzzy = { implementation = "prefer_rust_with_warning" },
})
