local M = {}

local project_picker = require("custom.snacks.project_picker")
local bm = require("bookmarks")
local snacks = require("snacks")
local buf_delete = require("bufdelete")

---------------------
-- GENERIC HELPERS --
---------------------

function M.prefill_command(command)
  return function()
    vim.api.nvim_feedkeys(":" .. command .. " ", "n", true)
  end
end

function M.execute_command(command)
  return function()
    vim.cmd(command)
  end
end

function M.toggle_background()
  vim.o.background = vim.o.background == "dark" and "light" or "dark"
end

-----------------
-- GIT HELPERS --
-----------------

local function is_git_repo()
  local git_dir = vim.fn.finddir(".git", vim.fn.expand("%:p:h") .. ";")
  return git_dir ~= ""
end

function M.find_git_files()
  if is_git_repo() then
    snacks.picker.git_files()
  else
    print("Not a git repository")
  end
end

function M.project_session_name()
  if is_git_repo() then
    local root = vim.fn.systemlist("git rev-parse --show-toplevel")[1]
    return vim.fn.fnamemodify(root, ":t")
  else
    return vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
  end
end

vim.api.nvim_create_user_command("GitsignsBlameToggle", function()
  -- 1. Get current state
  local cur_win = vim.api.nvim_get_current_win()
  local cur_buf = vim.api.nvim_get_current_buf()

  -- Check if this specific buffer has a 'blame_window' associated with it
  -- OR if the current window IS the blame window itself
  local tracked_blame_win = vim.t.gitsigns_blame_win
  local source_win = vim.t.gitsigns_source_win

  -- 2. TOGGLE OFF: If blame window is open and valid, close it
  if tracked_blame_win and vim.api.nvim_win_is_valid(tracked_blame_win) then
    vim.api.nvim_win_close(tracked_blame_win, true)

    -- Return focus to the source window if we know where it is
    if source_win and vim.api.nvim_win_is_valid(source_win) then
      vim.api.nvim_set_current_win(source_win)
    end

    -- Clear the tracking
    vim.t.gitsigns_blame_win = nil
    vim.t.gitsigns_source_win = nil
    return
  end

  -- 3. GIT CHECK: Only when trying to OPEN
  if not vim.b.gitsigns_status_dict then
    return
  end

  -- 4. TOGGLE ON: Open Blame
  local original_win = cur_win
  vim.cmd("Gitsigns blame")

  -- Gitsigns opens a new window; we wait a tiny bit to capture its ID
  vim.defer_fn(function()
    local new_win = vim.api.nvim_get_current_win()
    if new_win ~= original_win then
      -- Store the IDs in the Tab scope (vim.t) so they persist
      -- and are accessible from any window in this tab
      vim.t.gitsigns_blame_win = new_win
      vim.t.gitsigns_source_win = original_win
    end
  end, 50) -- 50ms delay to ensure Gitsigns has finished the split
end, {})

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

function M.switch_project()
  project_picker.open()
end

local function find_git_root()
  local cwd = vim.loop.cwd()
  local ok, result = pcall(vim.fn.systemlist, "git -C " .. vim.fn.shellescape(cwd) .. " rev-parse --show-toplevel")
  if ok and result and result[1] and result[1] ~= "" then
    return result[1]
  end
end

local function get_lsp_root()
  local buf = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients({ bufnr = buf })
  for _, client in ipairs(clients) do
    if client.config and client.config.root_dir then
      return client.config.root_dir
    end
  end
end

local function get_root()
  return find_git_root() or get_lsp_root() or vim.loop.cwd()
end

function M.search_project()
  snacks.picker.grep({
    args = { "--hidden" }, -- include hidden files (dotfiles)
  })
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

local function count_normal_buffers()
  local count = 0

  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].buflisted and vim.bo[buf].buftype == "" then
      count = count + 1
    end
  end

  return count
end

function M.kill_buffer()
  local current = vim.api.nvim_get_current_buf()

  -- If this is the last normal buffer, refuse
  if count_normal_buffers() <= 1 and vim.bo[current].buflisted and vim.bo[current].buftype == "" then
    vim.notify("Cannot kill the last buffer", vim.log.levels.WARN, { title = "Buffer" })
    return
  end

  buf_delete.bufdelete(current, true)
end

function M.kill_all_buffers_except_current()
  local current = vim.api.nvim_get_current_buf()

  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if buf ~= current and vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].buflisted then
      buf_delete.bufdelete(buf, true)
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

local indent_enabled = true

function M.indent_lines()
  indent_enabled = not indent_enabled

  if indent_enabled then
    snacks.indent.enable()
  else
    snacks.indent.disable()
  end
end

function M.snacks_explorer_focus()
  local picker = snacks.picker.get({ source = "explorer" })[1]
  if picker then
    picker:focus()
  end
end

function M.todo_jump(method)
  method({ keywords = { "TODO", "FIXME", "FIX" } })
end

return M
