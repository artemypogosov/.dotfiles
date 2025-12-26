local M = {}

local workspaces_picker = require("custom.snacks.workspaces_picker")
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
  if vim.o.background == "dark" then
    vim.o.background = "light"
  else
    vim.o.background = "dark"
  end
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

local blame_win = nil
local blame_buf = nil

vim.api.nvim_create_user_command("GitsignsBlameToggle", function()
  if blame_win and vim.api.nvim_win_is_valid(blame_win) then
    vim.schedule(function()
      local win_count = #vim.api.nvim_list_wins()

      if win_count > 1 and vim.api.nvim_win_is_valid(blame_win) then
        pcall(vim.api.nvim_win_close, blame_win, true)
      elseif blame_buf and vim.api.nvim_buf_is_valid(blame_buf) then
        pcall(vim.api.nvim_buf_delete, blame_buf, { force = true })
      end

      blame_win = nil
      blame_buf = nil
    end)

    return
  end

  vim.cmd("Gitsigns blame")
  blame_win = vim.api.nvim_get_current_win()
  blame_buf = vim.api.nvim_get_current_buf()
end, {})

---------------------
-- PROJECT HELPERS --
---------------------

function M.find_recent_files()
  snacks.picker.recent()
end

function M.switch_project()
  workspaces_picker.open()
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

return M
