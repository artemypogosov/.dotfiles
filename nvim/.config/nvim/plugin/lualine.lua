local helpers = require("helpers")

-- A blazing fast and easy to configure statusline written in pure lua
helpers.add({ "nvim-lualine/lualine.nvim", "nvim-mini/mini.icons" })

-- local noice = require("noice")
local lualine = require("lualine")
local mini_icons = require("mini.icons")

mini_icons.setup()
mini_icons.mock_nvim_web_devicons()

--- Checks if a Vim macro is currently being recorded and returns a status string.
--- @returns string - "recording @<register>" if a macro is being recorded, otherwise an empty string.
local function get_macro_recording()
	local recording_reg = vim.fn.reg_recording()
	if recording_reg == "" then
		return ""
	else
		return "recording @" .. recording_reg
	end
end

--- Returns current buffer indentation info (e.g. SPC 2 or TAB 2)
local function get_indent_info()
	local space_str = vim.bo.expandtab and "SPC" or "TAB"
	local size = vim.bo.expandtab and vim.bo.shiftwidth or vim.bo.tabstop
	if size == 0 then
		size = vim.bo.tabstop
	end
	return string.format("%s %d", space_str, size)
end

lualine.setup({
	options = {
		component_separators = { left = "⫶", right = "⫶" },
		section_separators = { left = "", right = "" },
		disabled_filetypes = {
			statusline = { "snacks_dashboard", "NvimTree" },
		},
	},
	sections = {
		lualine_a = {},
		lualine_b = {
			{
				"filename",
				path = 4,
			},
			{
				"tabs",
				mode = 1,
				tabs_color = {
					active = "lualine_b_normal",
				},
				cond = function()
					return #vim.api.nvim_list_tabpages() > 1
				end,
			},
			{
				get_macro_recording,
				cond = function()
					return vim.fn.reg_recording() ~= ""
				end,
				color = { fg = "#fb4934" },
			},
		},
		lualine_c = {},
		lualine_x = { "diagnostics", "branch" },
		lualine_y = {
			get_indent_info,
			"filetype",
			{
				function()
					local clients = vim.lsp.get_clients({ bufnr = 0 })
					local names = {}
					for _, client in ipairs(clients) do
						if not client.name:lower():find("copilot") then
							table.insert(names, client.name)
						end
					end
					return #names > 0 and table.concat(names, ", ") or "No LSP"
				end,
				icon = "",
			},
		},
		lualine_z = { "searchcount" },
	},
})
