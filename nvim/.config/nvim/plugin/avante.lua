local helpers = require("helpers")
local wk = require("which-key")

local modes = { "n", "v" }
local avante_loaded = false

local function load_avante_and_run(callback)
	if not avante_loaded then
		-- Use your Neovim like using Cursor AI IDE!
		helpers.add({
			"nvim-lua/plenary.nvim",
			"MunifTanjim/nui.nvim",
			"yetone/avante.nvim",
			"MeanderingProgrammer/render-markdown.nvim",
			"hrsh7th/nvim-cmp",
			"nvim-mini/mini.icons",
			"HakonHarnes/img-clip.nvim",
			"folke/snacks.nvim",
		})

		require("img-clip").setup({
			default = {
				embed_image_as_base64 = false,
				prompt_for_file_name = false,
				drag_and_drop = {
					insert_mode = true,
				},
				use_absolute_path = true,
			},
		})

		require("render-markdown").setup({
			file_types = { "markdown", "Avante" },
		})

		require("avante").setup({
			mode = "legacy",
			instructions_file = "avante.md",
			provider = "gemini",
			providers = {
				openai = {
					endpoint = "https://api.openai.com/v1",
					model = "gpt-4o-mini",
					timeout = 30000,
					context_window = 128000,
					extra_request_body = {
						temperature = 0.75,
					},
				},
				gemini = {
					model = "gemini-3.6-flash",
					timeout = 30000,
					temperature = 0,
					max_tokens = 8192,
				},
			},
			shortcuts = {
				{
					name = "refactor",
					description = "Refactor code with best practices",
					details = "Automatically refactor code to improve readability and follow best practices",
					prompt = "Please refactor this code following best practices, improving readability and maintainability while preserving functionality.",
				},
			},
			behaviour = {
				auto_set_keymaps = false,
			},
			input = {
				provider = "snacks",
			},
		})

		avante_loaded = true
	end

	if callback then
		callback()
	end
end

-- Lazy wrapper for api.ask actions
local function avante_ask(question)
	load_avante_and_run(function()
		require("avante.api").ask({ question = question })
	end)
end

-- Lazy wrapper for standard Avante vim commands (like :AvanteToggle)
local function run_avante_command(cmd)
	load_avante_and_run(function()
		vim.cmd(cmd)
	end)
end

-- Autocmd for package updates (compiling make if needed)
vim.api.nvim_create_autocmd("PackChanged", {
	callback = function(ev)
		local name, kind = ev.data.spec.name, ev.data.kind
		if name == "avante.nvim" and (kind == "install" or kind == "update") then
			vim.system({ "make" }, { cwd = ev.data.path }):wait()
		end
	end,
})

-- Which-key definitions triggering lazy loads on demand
wk.add({
	{ "<leader>a", group = "avante" },
	{ "<leader>a.", group = "actions" },

	{
		"<leader>a.g",
		function()
			avante_ask(
				"Perform grammar correction on the following text. Respond with ONLY the corrected text. No markdown fences, no explanations."
			)
		end,
		desc = "Grammar Correction",
		mode = modes,
	},
	{
		"<leader>a.d",
		function()
			avante_ask(
				"Add a high quality docstring to the following code. Include parameter types, return types, and any errors that might be raised. Respond with ONLY the complete code including the docstring. No markdown fences, no explanations."
			)
		end,
		desc = "Docstring",
		mode = modes,
	},
	{
		"<leader>a.o",
		function()
			avante_ask(
				"Optimize the following code for better performance and readability. Respond with ONLY the optimized code. No markdown fences, no explanations."
			)
		end,
		desc = "Optimize Code",
		mode = modes,
	},
	{
		"<leader>a.f",
		function()
			avante_ask(
				"Fix bugs in the following code. Respond with ONLY the fixed code. No markdown fences, no explanations."
			)
		end,
		desc = "Fix Bugs",
		mode = modes,
	},
	{
		"<leader>a.e",
		function()
			avante_ask("Explain the selected code. Use markdown format with clear sections.")
		end,
		desc = "Explain Code",
		mode = modes,
	},
	{
		"<leader>a.x",
		function()
			avante_ask(
				"Analyze the selected code for any bugs, runtime errors, or logical issues. Explain them clearly and suggest a fix."
			)
		end,
		desc = "Explain Error",
		mode = modes,
	},

	{
		"<leader>aa",
		function()
			run_avante_command("AvanteToggle")
		end,
		desc = "Ask",
		mode = modes,
	},
	{
		"<leader>ae",
		function()
			run_avante_command("AvanteEdit")
		end,
		desc = "Edit",
		mode = modes,
	},
	{
		"<leader>an",
		function()
			run_avante_command("AvanteChatNew")
		end,
		desc = "Chat New",
		mode = { "n" },
	},
	{
		"<leader>ah",
		function()
			run_avante_command("AvanteHistory")
		end,
		desc = "History",
		mode = { "n" },
	},
	{
		"<leader>aC",
		function()
			run_avante_command("AvanteClear")
		end,
		desc = "Clear Chat",
		mode = { "n" },
	},
	{
		"<leader>af",
		function()
			run_avante_command("AvanteFocus")
		end,
		desc = "Focus",
		mode = { "n" },
	},
	{
		"<leader>aR",
		function()
			run_avante_command("AvanteRefresh")
		end,
		desc = "Refresh All Windows",
		mode = { "n" },
	},
	{
		"<leader>aS",
		function()
			run_avante_command("AvanteStop")
		end,
		desc = "Stop Request",
		mode = { "n" },
	},
	{
		"<leader>a?",
		function()
			run_avante_command("AvanteModels")
		end,
		desc = "Switch Model",
		mode = { "n" },
	},
})
