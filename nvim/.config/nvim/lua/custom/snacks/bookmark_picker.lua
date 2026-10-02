local Snacks = require("snacks")

local M = {}

--- Retrieve and format the annotation text using configured keywords and signs
--- @param annotation string The input annotation string
--- @return string The formatted annotation string with symbol prefix
local function get_text(annotation)
	local config = require("bookmarks.config").config
	local pref = annotation:sub(1, 2)
	local ret = config.keywords[pref]
	if ret == nil then
		ret = config.signs.ann.text .. " "
	end
	return ret .. annotation
end

--- Collect and format all cached bookmarks into a flat list of items
--- @return table[] A list of bookmark tables containing file paths, line numbers, and text
local function get_bookmarks()
	local config = require("bookmarks.config").config
	local allmarks = config.cache.data or {}
	local items = {}

	for filename, marks in pairs(allmarks) do
		for lnum, v in pairs(marks) do
			table.insert(items, {
				file = filename,
				lnum = tonumber(lnum),
				text = v.a and get_text(v.a) or v.m,
			})
		end
	end
	return items
end

function M.open()
	Snacks.picker({
		title = "Bookmarks",

		layout = { preset = "select", layout = { height = 0.25 } },
		finder = get_bookmarks,

		format = function(item)
			return {
				{ tostring(item.lnum), "Comment" },
				{ ": ", "" },
				{ item.text or "", "SnacksPickerDesc" },
				{ " ", "" },
				{ vim.fn.fnamemodify(item.file, ":~:."), "SnacksPickerLabel" },
			}
		end,

		actions = {
			confirm = function(picker, item)
				picker:close()
				vim.cmd("edit " .. vim.fn.fnameescape(item.file))
				vim.api.nvim_win_set_cursor(0, { item.lnum, 0 })
			end,
		},
	})
end

return M
