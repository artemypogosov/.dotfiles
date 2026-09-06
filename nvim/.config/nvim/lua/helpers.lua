--- @brief General Configuration Helpers
--- Provides vanilla engine modifications, maintenance utilities,
--- context-aware search tools, and lifecycle controllers for Neovim.

local M = {}

--- @section Package & Macro Utilities

-- Plugin Bootstrapping
function M.add(plugins, host)
	local hosts = {
		github = "https://github.com/",
		gitlab = "https://gitlab.com/",
		codeberg = "https://codeberg.org/",
	}

	local base_url = hosts[host] or (host and host .. "/") or hosts.github
	local full_specs = {}

	for i, item in ipairs(plugins) do
		if type(item) == "table" then
			-- Shallow copy to preserve Lua userdata/objects like version range
			local spec = {}
			for k, v in pairs(item) do
				spec[k] = v
			end

			local raw_src = spec.src or spec[1]
			if raw_src and not raw_src:find("^https?://") then
				spec.src = base_url:gsub("/+$", "") .. "/" .. raw_src:gsub("^/+", "")
			else
				spec.src = raw_src
			end

			spec[1] = nil
			full_specs[i] = spec
		elseif type(item) == "string" then
			if item:find("^https?://") then
				full_specs[i] = item
			else
				full_specs[i] = base_url:gsub("/+$", "") .. "/" .. item:gsub("^/+", "")
			end
		end
	end

	return vim.pack.add(full_specs)
end

function M.prefill(command)
	return function()
		vim.api.nvim_feedkeys(":" .. command .. " ", "n", true)
	end
end

---Запускає Vim-команду та виконує колбек після неї.
---@param command string Клієнтська команда (наприклад, "wall")
---@param after function|nil Функція, яка запуститься після виконання команди
function M.execute(command, after)
	return function()
		vim.cmd(command)
		if after then
			after()
		end
	end
end

--- @section Filesystem Maintenance

--- @block Delete temp files
function M.delete_recent_files()
	if vim.fn.input("Clear recent files? (yes/no): "):lower() == "yes" then
		vim.fn.jobstart("rm -f ~/.local/state/nvim/shada/main.shada", {
			on_exit = function()
				vim.notify("Recent files cleared!", 2, { title = "System" })
			end,
		})
	end
end

function M.delete_scratch_files()
	if vim.fn.input("Delete all scratch files? (yes/no): "):lower() == "yes" then
		vim.fn.jobstart("rm -rf ~/.local/share/nvim/scratch/*", {
			on_exit = function()
				vim.notify("Scratch files deleted!", 2, { title = "Scratch" })
			end,
		})
	end
end

--- @section Text Modification

--- @block Contextual Search & Replace
function M.search_replace()
	local mode = vim.api.nvim_get_mode().mode
	local range = "%"
	local search = ""

	if mode:match("[vV\22]") then
		-- VISUAL MODE: Target selection, empty search
		range = "'<,'>"
		search = ""
	else
		-- NORMAL MODE: Target whole file, grab word under cursor (The "Point" logic)
		range = "%"
		search = vim.fn.expand("<cword>")
	end

	local movement = (search == "") and "<Left><Left><Left><Left>" or "<Left><Left><Left>"

	local cmd = ":<C-u>" .. range .. "s#" .. search .. "##gI" .. movement
	local keys = vim.api.nvim_replace_termcodes(cmd, true, false, true)
	vim.api.nvim_feedkeys(keys, "n", false)
end

---Displays a bottom menu to choose a buffer component under the cursor,
---then pre-fills the command-line with a global search-and-replace template.
function M.search_replace_menu()
	local options = {
		{ key = "w", cmd = vim.fn.expand("<cword>") },
		{ key = "W", cmd = vim.fn.expand("<cWORD>") },
		{ key = "e", cmd = vim.fn.expand("<cexpr>") },
		{ key = "f", cmd = vim.fn.expand("<cfile>") },
	}

	-- Generate scannable text lines for the target items
	local lines = {}
	for _, opt in ipairs(options) do
		table.insert(lines, string.format("[%s]: %s", opt.key, opt.cmd))
	end

	-- Initialize an unlisted scratch buffer for the interface
	local buf = vim.api.nvim_create_buf(false, true)
	vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

	-- Open a minimal full-width row window at the very bottom edge of the screen
	local win = vim.api.nvim_open_win(buf, false, {
		relative = "editor",
		row = vim.o.lines - #lines - 3,
		col = 0,
		width = vim.o.columns,
		height = #lines,
		style = "minimal",
		border = "single",
	})

	-- Force an immediate UI redraw so the menu text prints before waiting for input
	vim.cmd("redraw")

	-- Capture user input keypress cleanly
	local char = vim.fn.nr2char(vim.fn.getchar())

	-- Close floating window layout immediately before invoking command-line state
	if vim.api.nvim_win_is_valid(win) then
		vim.api.nvim_win_close(win, true)
	end

	-- Match user selection and feed the terminal keystroke sequence
	for _, opt in ipairs(options) do
		if char == opt.key then
			local cmd = ":%s#" .. opt.cmd .. "##gI<Left><Left><Left>"
			local keys = vim.api.nvim_replace_termcodes(cmd, true, false, true)
			vim.api.nvim_feedkeys(keys, "n", false)
			return
		end
	end
end

--- @section Project Environment Management

function M.quit(message)
	-- Використовуємо нативний метод вибору Neovim
	vim.ui.select({ "Yes", "No" }, {
		prompt = message,
	}, function(choice)
		-- Якщо користувач обрав перший варіант ("Yes") — виходимо
		if choice == "Yes" then
			vim.cmd("qa")
		end
	end)
end

---Prompts the user via an interactive UI input to name and save the current session.
---Provides a completely empty input field for manual text entry.
function M.prompt_session_save()
	-- Open an interactive input field with no pre-filled text
	vim.ui.input({
		prompt = "Save session as: ",
		default = "",
	}, function(input)
		-- Guard against empty strings or canceling out of the prompt (Esc/Ctrl-C)
		if not input or vim.trim(input) == "" then
			vim.notify("Session save canceled", vim.log.levels.WARN, { title = "Sessions" })
			return
		end

		-- Write the session using the sanitized user input string
		local target_name = vim.trim(input)
		require("mini.sessions").write(target_name)

		vim.notify("Session saved: " .. target_name, vim.log.levels.INFO, { title = "Sessions" })
	end)
end

-------------------------------------------------------------
-------------------------------------------------------------

-------------------------------------------------------------
-------------------------------------------------------------

function M.toggle_background()
	vim.o.background = vim.o.background == "dark" and "light" or "dark"
end

function M.todo_jump(method)
	method({ keywords = { "TODO", "FIXME", "FIX" } })
end
--- @brief Plugins Abstraction Layer
--- Wraps proxy interfaces and dynamic lazy loaders around external plugins
--- like Snacks.nvim and Bookmarks.nvim to power clean global mappings.

local snacks = setmetatable({}, {
	__index = function(_, key)
		return require("snacks")[key]
	end,
})

local bookmarks = setmetatable({}, {
	__index = function(_, key)
		return require("bookmarks")[key]
	end,
})

local project_picker = setmetatable({}, {
	__index = function(_, key)
		return require("custom.snacks.project_picker")[key]
	end,
})

--- @section Snacks Picker Integrations

--- @block File Discovery Utilities
function M.find_files(opts)
	opts = opts or {}
	snacks.picker.files(opts)
end

function M.find_files_in_current_dir()
	local file = vim.api.nvim_buf_get_name(0)
	local cwd

	if file == "" then
		vim.notify("Buffer has no file path, fallback to 'pwd'")
		cwd = vim.fn.getcwd()
	else
		cwd = vim.fn.fnamemodify(file, ":h")
	end

	snacks.picker.files({ title = "Find file from here", cwd = cwd })
end

function M.find_recent_files()
	snacks.picker.recent()
end

--- @block Buffer & Context Pickers
function M.switch_buffers()
	snacks.picker.buffers()
end

function M.search_buffer()
	snacks.picker.lines({ title = "Search buffer" })
end

function M.search_opened_buffers()
	snacks.picker.grep_buffers({ title = "Search opened buffer" })
end

--- @block Project Search Controllers
function M.search_project()
	snacks.picker.grep()
end

function M.switch_project()
	project_picker.open()
end

--- @section Core Snacks Ecosystem Modules

--- @block Buffer Life Cycle
function M.kill_buffer()
	snacks.bufdelete()
end

function M.kill_all_buffers_except_current()
	local current = vim.api.nvim_get_current_buf()

	for _, buf in ipairs(vim.api.nvim_list_bufs()) do
		if buf ~= current and vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].buflisted then
			snacks.bufdelete(buf)
		end
	end
end

--- @block Scratchpads & Remote Tools
function M.git_browse()
	snacks.gitbrowse()
end

--- @block File Explorer & Rendering
function M.snacks_explorer_focus()
	local picker = snacks.picker.get({ source = "explorer" })[1]
	if picker then
		picker:focus()
	end
end

-- Holds the local state (starts disabled by default)
local indent_enabled = true

function M.indent_lines()
	indent_enabled = not indent_enabled

	if indent_enabled then
		snacks.indent.enable()
	else
		snacks.indent.disable()
	end
end

--- @section Bookmark Management

--- @block Maintenance & Sanitization
function M.delete_all_bookmarks()
	local answer = vim.fn.input("Delete all bookmarks? (yes/no): ")
	if answer:lower() == "yes" then
		bookmarks.bookmark_clean()
	end
end

return M
