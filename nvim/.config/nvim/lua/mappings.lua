local wk = require("which-key")
local bookmarks = require("bookmarks")
local confirm_quit = require("confirm-quit")
local snacks = require("snacks")
local man_picker = require("custom.snacks.man_picker")
local bookmark_picker = require("custom.snacks.bookmark_picker")
local session = require("mini.sessions")
local helpers = require("helpers")

wk.add({
  ------------
  --- FILE ---
  ------------
  { "<leader>f", group = "file" },

  { "<leader>ff", helpers.execute_command("Oil"), desc = "Find file", mode = "n" },
  {
    "<leader>.",
    helpers.execute_command("Oil"),
    desc = "Find file",
    mode = "n",
  },
  { "<leader>fr", helpers.find_recent_files, desc = "Recent files", mode = "n" },
  {
    "<leader>fp",
    function()
      snacks.dashboard.pick("files", { cwd = vim.fn.stdpath("config") })
    end,
    desc = "Recent files",
    mode = "n",
  },
  {
    "<leader>fd",
    helpers.execute_command("lua Snacks.picker.files()"),
    desc = "Find file from here",
    mode = "n",
  },
  {
    "<leader><leader>",
    helpers.execute_command("lua Snacks.picker.files()"),
    desc = "Find files in project",
    mode = "n",
  },
  { "<leader>fZ", helpers.delete_recent_files, desc = "Delete recent files", mode = "n" },

  ---------------
  --- PROJECT ---
  ---------------
  { "<leader>p", group = "project" },

  { "<leader>pp", helpers.switch_project, desc = "Switch project", mode = "n" },
  { "<leader>pl", helpers.execute_command("WorkspacesList"), desc = "List workspaces", mode = "n" },
  { "<leader>pL", helpers.execute_command("WorkspacesListDirs"), desc = "List projects' dirs", mode = "n" },
  { "<leader>pa", helpers.prefill_command("WorkspacesAdd"), desc = "Add project", mode = "n" },
  { "<leader>pA", helpers.prefill_command("WorkspacesAddDir"), desc = "Add project dir", mode = "n" },
  { "<leader>pr", helpers.prefill_command("WorkspacesRename"), desc = "Rename project", mode = "n" },
  { "<leader>pd", helpers.prefill_command("WorkspacesRemove"), desc = "Remove project", mode = "n" },
  { "<leader>pD", helpers.prefill_command("WorkspacesRemoveDir"), desc = "Remove project dir", mode = "n" },

  ---------------
  --- WINDOWS ---
  ---------------
  { "<leader>w", group = "window" },

  { "<leader>ww", "<C-\\><C-n><C-w>w", desc = "Next window", mode = { "n", "t" } },
  { "<leader>wh", "<C-\\><C-n><C-w>h", desc = "Switch left", mode = { "n", "t" } },
  { "<leader>wl", "<C-\\><C-n><C-w>l", desc = "Switch right", mode = { "n", "t" } },
  { "<leader>wj", "<C-\\><C-n><C-w>j", desc = "Switch down", mode = { "n", "t" } },
  { "<leader>wk", "<C-\\><C-n><C-w>k", desc = "Switch up", mode = { "n", "t" } },

  { "<leader>ws", "<C-\\><C-n><C-w>s", desc = "Split H", mode = { "n", "t" } },
  { "<leader>wv", "<C-\\><C-n><C-w>v", desc = "Split V", mode = { "n", "t" } },

  { "<leader>wH", "<C-\\><C-n><C-w>H", desc = "Move left", mode = { "n", "t" } },
  { "<leader>wL", "<C-\\><C-n><C-w>L", desc = "Move right", mode = { "n", "t" } },
  { "<leader>wJ", "<C-\\><C-n><C-w>J", desc = "Move down", mode = { "n", "t" } },
  { "<leader>wK", "<C-\\><C-n><C-w>K", desc = "Move up", mode = { "n", "t" } },
  { "<leader>wx", "<C-\\><C-n><C-w>x", desc = "Swap with next", mode = { "n", "t" } },

  { "<leader>wc", "<C-\\><C-n><C-w>c", desc = "Close", mode = { "n", "t" } },
  { "<leader>wC", "<C-\\><C-n><C-w>o", desc = "Close other windows", mode = { "n", "t" } },

  { "<leader>wM", helpers.execute_command("MaximizerToggle"), desc = "Maximize window", mode = { "n", "t" } },
  { "<leader>wmh", helpers.execute_command("resize 40"), desc = "Increase height", mode = { "n", "t" } },
  { "<leader>wmw", helpers.execute_command("vertical resize 110"), desc = "Increase width", mode = { "n", "t" } },
  { "<leader>w=", "<C-\\><C-n><C-w>=", desc = "=", mode = { "n", "t" } },

  --------------
  --- TOGGLE ---
  --------------
  { "<leader>t", group = "toggle" },

  { "<leader>tt", helpers.execute_command("lua Snacks.explorer()"), desc = "Toggle sidebar", mode = "n" },
  { "<leader>tf", helpers.snacks_explorer_focus, desc = "Focus  sidebar", mode = "n" },

  { "<leader>ti", helpers.indent_lines, desc = "Indent lines", mode = "n" },
  { "<leader>tF", helpers.execute_command("NvimTreeFindFile"), desc = "Point file in structure", mode = "n" },
  { "<leader>tT", helpers.toggle_background, desc = "Toggle dark/light theme", mode = "n" },
  { "<leader>tx", snacks.terminal.toggle, desc = "Toggle terminal", mode = { "n", "t" } },
  { "<leader>tX", snacks.terminal.open, desc = "New terminal", mode = { "n", "t" } },
  { "<leader>tz", helpers.execute_command("ZenMode"), desc = "Zen mode", mode = "n" },

  ------------------
  --- WORKSPACES ---
  ------------------
  { "<leader><Tab>", group = "workspace" },

  { "<leader><Tab>n", helpers.execute_command("tabnew"), desc = "New workspace", mode = "n" },
  { "<leader><Tab>k", helpers.execute_command("tabclose"), desc = "Delete workspace", mode = "n" },
  { "<leader><Tab>]", helpers.execute_command("tabnext"), desc = "Next workspace", mode = "n" },
  { "<leader><Tab>[", helpers.execute_command("tabNext"), desc = "Prev workspace", mode = "n" },
  { "<leader><Tab>r", helpers.prefill_command("LualineRenameTab"), desc = "Rename workspace", mode = "n" },

  ---------------
  --- BUFFERS ---
  ---------------
  { "<leader>b", group = "buffer" },

  {
    "<leader>bb",
    helpers.execute_command("lua Snacks.picker.buffers()"),
    desc = "Switch buffer",
    mode = "n",
  },
  {
    "<leader>,",
    helpers.execute_command("lua Snacks.picker.buffers()"),
    desc = "Switch buffer",
    mode = "n",
  },

  { "<leader>bn", helpers.execute_command("enew"), desc = "New buffer", mode = "n" },
  { "<leader>bh", helpers.execute_command("new"), desc = "New horizontal buffer", mode = "n" },
  { "<leader>bv", helpers.execute_command("vnew"), desc = "New vertical buffer", mode = "n" },
  { "<leader>b]", helpers.execute_command("bnext"), desc = "Next buffer", mode = "n" }, -- tab,
  { "<leader>b[", helpers.execute_command("bprev"), desc = "Prev buffer", mode = "n" }, -- shift + tab,
  { "<leader>bk", helpers.kill_buffer, desc = "Kill buffer", mode = "n" },
  { "<leader>bO", helpers.kill_all_buffers_except_current, desc = "Kill all buffers except current", mode = "n" },

  --  { "<leader>bx", Snacks.scratch, desc = "New scratch", mode = "n" },
  { "<leader>bZ", helpers.delete_scratch_files, desc = "Delete all scratch files", mode = "n" },
  {
    "<leader>bX",
    helpers.execute_command("lua Snacks.scratch.select()"),
    desc = "Select scratch",
    mode = "n",
  },

  ------------------
  --- SUBSTITUTE ---
  ------------------
  { "<leader>r", group = "replace" },

  { "<leader>rr", "<CMD>SearchReplaceSingleBufferOpen<CR>", desc = "Replace", mode = "n" },
  { "<leader>rp", "<CMD>SearchReplaceSingleBufferCWord<CR>", desc = "Replace in place [CWord]", mode = "n" },
  { "<leader>rP", "<CMD>SearchReplaceSingleBufferCWORD<CR>", desc = "Replace in place [CWORD]", mode = "n" },

  { "<leader>rb", group = "all open buffers" },

  { "<leader>rbr", "<CMD>SearchReplaceMultiBufferOpen<CR>", desc = "Replace in all open files", mode = "n" },
  {
    "<leader>rbp",
    "<CMD>SearchReplaceMultiBufferCWord<CR>",
    desc = "Replace in place [CWord] in all open files",
    mode = "n",
  },
  {
    "<leader>rbP",
    "<CMD>SearchReplaceMultiBufferCWORD<CR>",
    desc = "Replace in place [CWORD] in all open files",
    mode = "n",
  },

  --------------
  --- SEARCH ---
  --------------
  { "<leader>s", group = "search" },

  {
    "<leader>ss",
    helpers.execute_command("lua Snacks.picker.lines()"),
    desc = "Search in buffer",
    mode = "n",
  },
  {
    "<leader>sb",
    helpers.execute_command("lua Snacks.picker.lines()"),
    desc = "Search in buffer",
    mode = "n",
  },

  {
    "<leader>sB",
    helpers.execute_command("lua Snacks.picker.grep_buffers()"),
    desc = "Seach all open buffers",
    mode = "n",
  },

  { "<leader>sp", helpers.search_project, desc = "Search in project", mode = "n" },

  { "<leader>st", group = "list TODO" },
  {
    "<leader>stt",
    helpers.execute_command("lua Snacks.picker.todo_comments({ keywords = { 'TODO' } })"),
    desc = "List all TODOS",
    mode = "n",
  },
  {
    "<leader>stf",
    helpers.execute_command("lua Snacks.picker.todo_comments({ keywords = { 'FIX', 'FIXME' } })"),
    desc = "List all FIXME",
    mode = "n",
  },
  -----------
  --- GIT ---
  -----------
  { "<leader>g", group = "git" },

  { "<leader>gs", helpers.execute_command("Gitsigns stage_hunk"), desc = "Stage hunk", mode = "n" },
  { "<leader>gr", helpers.execute_command("Gitsigns reset_hunk"), desc = "Reset hunk", mode = "n" },
  { "<leader>gv", helpers.execute_command("Gitsigns select_hunk"), desc = "Select hunk", mode = "n" },

  { "<leader>g[", helpers.execute_command("Gitsigns nav_hunk prev"), desc = "Prev hunk", mode = "n" },
  { "<leader>g]", helpers.execute_command("Gitsigns nav_hunk next"), desc = "Next hunk", mode = "n" },

  { "<leader>gp", helpers.execute_command("Gitsigns preview_hunk_inline"), desc = "Preview hunk [inline]", mode = "n" },
  { "<leader>gP", helpers.execute_command("Gitsigns preview_hunk"), desc = "Previw hunk [popup]", mode = "n" },

  { "<leader>gd", helpers.prefill_command("Gitsigns diffthis"), desc = "Diff with revision", mode = "n" },
  {
    "<leader>gl",
    function()
      local file = vim.fn.expand("%:p")
      local root = vim.fs.root(file, { ".git" })
      if not root then
        vim.notify("Not inside a git repository", vim.log.levels.WARN)
        return
      end

      local cwd = vim.fn.getcwd()
      vim.cmd("lcd " .. root)
      snacks.picker.git_log()
      vim.cmd("lcd " .. cwd)
    end,
    desc = "Branch log",
    mode = "n",
  },
  {
    "<leader>g.",
    helpers.execute_command("lua Snacks.picker.git_log_file()"),
    desc = "File log",
    mode = "n",
  },

  { "<leader>gg", helpers.execute_command("Neogit kind=replace"), desc = "Git status", mode = "n" },
  { "<leader>gL", helpers.execute_command("Neogit log"), desc = "Neogit: log", mode = "n" },
  { "<leader>gB", helpers.execute_command("Neogit branch"), desc = "Neogit: switch branch", mode = "n" },

  {
    "<leader>go",
    function()
      snacks.gitbrowse()
    end,
    desc = "Git Browse",
  },

  { "<M-a>", helpers.execute_command("GitsignsBlameToggle"), desc = "Git side annotations", mode = "n" },
  { "<M-A>", helpers.execute_command("Gitsigns blame_line"), desc = "Git popup annotations", mode = "n" },

  -----------
  --- LSP ---
  -----------
  { "<leader>c", group = "code" },
  -- All mappings are located in plugins/lsp.lua

  ----------
  --- AI ---
  ----------
  { "<leader>a", group = "AI" },

  -----------------
  --- BOOKMARKS ---
  -----------------
  { "<leader>m", group = "bookmarks" },

  { "<leader>mm", bookmarks.bookmark_toggle, desc = "Set bookmark", mode = "n" },
  { "<leader>mc", helpers.delete_all_bookmarks, desc = "Clean all bookmarks", mode = "n" },
  -- TODO: no analog in Snacks, write my own solution
  {
    "<leader>mf",
    bookmark_picker.open,
    desc = "Show bookmarks",
    mode = "n",
  },
  { "mn", bookmarks.bookmark_next, desc = "Next bookmark", mode = "n" },
  { "mp", bookmarks.bookmark_prev, desc = "Prev bookmark", mode = "n" },

  -------------
  --- TO-DO ---
  -------------

  ----------------
  --- COMMENTS ---
  ----------------
  { "<C-/>", ":norm gcc<CR>", desc = "Toggle comment", mode = "n" },
  { "<C-/>", ":norm gcc<CR>", desc = "Toggle multiline comment", mode = "v" },

  ---------------
  --- SESSION ---
  ---------------
  { "<leader>q", group = "session" },

  { "<leader>qq", "<CMD>qa<CR>", desc = "Quit", mode = "n" },
  { "<leader>qQ", confirm_quit.confirm_quit, desc = "Quit + confirm", mode = "n" },

  {
    "<leader>qs",
    function()
      session.write(helpers.project_session_name())
    end,
    desc = "Save session",
    mode = "n",
  },
  {
    "<leader>ql",
    function()
      session.select("read")
    end,
    desc = "Load session",
    mode = "n",
  },
  {
    "<leader>qD",
    function()
      session.select("delete")
    end,
    desc = "Delete session",
    mode = "n",
  },

  ------------
  --- HELP ---
  ------------
  { "<leader>h", group = "help" },

  { "<leader>hL", helpers.execute_command("Lazy"), desc = "Lazy", mode = "n" },
  { "<leader>hM", helpers.execute_command("Mason"), desc = "Mason", mode = "n" },

  { "<leader>hm", man_picker.open, desc = "Man pages", mode = "n" },
  -- Handy for checking 'filetype' which is needed for plugins/formatter.lua
  { "<leader>hf", ":lua print('Filetype:', vim.bo.filetype)<CR>", desc = "Print filetype", mode = "n" },
  { "<leader>hs", ":set spell<CR>", desc = "Enable spellcheck", mode = "n" },
  { "<leader>hS", ":set nospell<CR>", desc = "Disable spellcheck", mode = "n" },
  { "<leader>hr", helpers.execute_command("lua Snacks.picker.spelling()"), desc = "Spelling suggestions", mode = "n" },

  -------------
  --- OTHER ---
  -------------

  { "<C-s>", "<cmd>w<CR>", desc = "Save current buffer [n]", mode = "n" },
  { "<C-s>", "<Esc><cmd>w<CR>", desc = "Save current buffer [i]", mode = "i" },
  { "<C-c>", "<cmd>%y+<CR>", desc = "Copy entire buffer", mode = "n" },
  { "<Esc>", "<cmd>noh<CR>", desc = "Clean seach highlights", mode = "n" },
  { "<leader>M", "<cmd>messages<CR>", desc = "Messages", mode = "n" },

  -- Map ZZ to ConfirmQuit (saves and quits with confirmation)
  { "ZZ", confirm_quit.confirm_quit, desc = "Confirm before ZZ (wq)" },
})
