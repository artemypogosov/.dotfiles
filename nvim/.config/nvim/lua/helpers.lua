local M = {}

local vim = vim
local bm = require("bookmarks")
local snacks = require("snacks")
local project_picker = require("custom.snacks.project_picker")

---------------------
-- GENERIC HELPERS --
---------------------

function M.prefill_command(command)
  return function()
    vim.api.nvim_feedkeys(":" .. command .. " ", "n", true)
  end
end

function M.execute_command(command, after)
  return function()
    vim.cmd(command)

    if after then
      after()
    end
  end
end

-----------------
-- GIT HELPERS --
-----------------

local function is_git_repo()
  local git_dir = vim.fn.finddir(".git", vim.fn.expand("%:p:h") .. ";")
  return git_dir ~= ""
end

---------------------
-- PROJECT HELPERS --
---------------------

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

function M.switch_buffers()
  snacks.picker.buffers()
end

function M.search_buffer()
  snacks.picker.lines({ title = "Search buffer" })
end

function M.search_opened_buffer()
  snacks.picker.grep_buffers({ title = "Search opened buffer" })
end

function M.switch_project()
  project_picker.open()
end

function M.search_project()
  snacks.picker.grep()
end

function M.project_session_name()
  if is_git_repo() then
    local root = vim.fn.systemlist("git rev-parse --show-toplevel")[1]
    return vim.fn.fnamemodify(root, ":t")
  else
    return vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
  end
end

---------------------
-- CLEANUP HELPERS --
---------------------

function M.delete_recent_files()
  local answer = vim.fn.input("Clear recent files? (yes/no): ")
  if answer:lower() == "yes" then
    vim.cmd("!rm ~/.local/state/nvim/shada/main.shada")
  end
end

function M.delete_scratch_files()
  local answer = vim.fn.input("Delete all scratch files? (yes/no): ")
  if answer:lower() == "yes" then
    vim.cmd("!rm ~/.local/share/nvim/scratch/*")
  end
end

function M.delete_all_bookmarks()
  local answer = vim.fn.input("Delete all bookmarks? (yes/no): ")
  if answer:lower() == "yes" then
    bm.bookmark_clean()
  end
end

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

------------
-- SNACKS --
------------

function M.git_browse()
  snacks.gitbrowse()
end

function M.new_scratch()
  snacks.scratch()
end

function M.select_scratch()
  snacks.scratch.select()
end

function M.snacks_explorer_focus()
  local picker = snacks.picker.get({ source = "explorer" })[1]
  if picker then
    picker:focus()
  end
end

-- 'indent_enabled' must be outside of function (because it holds state)
local indent_enabled = true

function M.indent_lines()
  indent_enabled = not indent_enabled

  if indent_enabled then
    snacks.indent.enable()
  else
    snacks.indent.disable()
  end
end

-------------
-- REPLACE --
-------------

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

function M.search_replace_menu()
  local options = {
    { key = "w", cmd = vim.fn.expand("<cword>") },
    { key = "W", cmd = vim.fn.expand("<cWORD>") },
    { key = "e", cmd = vim.fn.expand("<cexpr>") },
    { key = "f", cmd = vim.fn.expand("<cfile>") },
  }

  -- Create the display text
  local lines = {}
  for _, opt in ipairs(options) do
    table.insert(lines, string.format("[%s]: %s", opt.key, opt.cmd))
  end

  -- Create a scratch buffer for the menu
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

  -- Open a small split at the bottom
  local win = vim.api.nvim_open_win(buf, false, {
    relative = "editor",
    row = vim.o.lines - #lines - 3,
    col = 0,
    width = vim.o.columns,
    height = #lines,
    style = "minimal",
    border = "single",
  })

  -- Wait for a single character input
  vim.schedule(function()
    local char = vim.fn.nr2char(vim.fn.getchar())
    vim.api.nvim_win_close(win, true) -- Close menu immediately after keypress

    for _, opt in ipairs(options) do
      if char == opt.key then
        local cmd = ":%s#" .. opt.cmd .. "##gI<Left><Left><Left>"
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(cmd, true, false, true), "n", false)
        return
      end
    end
  end)
end

------------
-- OTHER --
------------

function M.quit(message)
  local choice = vim.fn.confirm(message, "&Yes\n&No", 2)
  if choice == 1 then
    vim.cmd("qa")
  end
end

function M.toggle_background()
  vim.o.background = vim.o.background == "dark" and "light" or "dark"
end

function M.todo_jump(method)
  method({ keywords = { "TODO", "FIXME", "FIX" } })
end

return M
