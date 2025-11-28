local M = {}

local telescope = require("telescope.builtin")
local telescope_global = require("telescope")
local bm = require("bookmarks")
local Snacks = require("snacks")

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
    telescope.git_files()
  else
    print("Not a git repository")
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
  telescope.oldfiles({
    previewer = false,
    layout_config = { width = 80, height = 15 },
  })
end

function M.switch_project()
  telescope_global.extensions.workspaces.workspaces({
    layout_config = {
      width = 80,
      height = 15,
      prompt_position = "top",
    },
  })
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
  telescope.live_grep({
    cwd = get_root(),
    prompt_title = "Live Grep (" .. get_root() .. ")",
    additional_args = function()
      return { "--hidden", "--glob", "!.git/*" }
    end,
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

function M.kill_all_buffers()
  local answer = vim.fn.input("Delete all buffers? (y/n): ")
  if answer:lower() == "y" then
    vim.cmd("%bd")
  end
end

------------
-- SNACKS --
------------

function M.git_browse()
  Snacks.gitbrowse()
end

function M.new_scratch()
  Snacks.scratch()
end

function M.select_scratch()
  Snacks.scratch.select()
end

local indent_enabled = true

function M.indent_lines()
  indent_enabled = not indent_enabled

  if indent_enabled then
    Snacks.indent.enable()
  else
    Snacks.indent.disable()
  end
end

return M
