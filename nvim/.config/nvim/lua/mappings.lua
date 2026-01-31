local wk = require("which-key")
local bookmarks = require("bookmarks")
local quit = require("confirm-quit")
local snacks = require("snacks")
local man_picker = require("custom.snacks.man_picker")
local bookmark_picker = require("custom.snacks.bookmark_picker")
local session = require("mini.sessions")
local helpers = require("helpers")
local todo_comments = require("todo-comments")
local grug_far = require("grug-far")

wk.add({
  ------------
  --- FILE ---
  ------------
  { "<leader>f", group = "file" },

  { "<leader>ff", helpers.execute_command("Oil"), desc = "Find file", mode = "n" },
  { "<leader>.", helpers.execute_command("Oil"), desc = "Find file", mode = "n" },
  { "<leader>fr", helpers.find_recent_files, desc = "Recent files", mode = "n" },
  {
    "<leader>fp",
    function()
      helpers.find_files({ title = "Find file in config", cwd = vim.fn.stdpath("config") })
    end,
    desc = "Find file in config",
    mode = "n",
  },
  { "<leader>fd", helpers.find_files_in_current_dir, desc = "Find file from here", mode = "n" },
  { "<leader><leader>", helpers.find_files, desc = "Find files in project", mode = "n" },
  { "<leader>fZ", helpers.delete_recent_files, desc = "Delete recent files", mode = "n" },

  ---------------
  --- BUFFERS ---
  ---------------
  { "<leader>b", group = "buffer" },

  { "<leader>bb", helpers.switch_buffers, desc = "Switch buffer", mode = "n" },
  { "<leader>,", helpers.switch_buffers, desc = "Switch buffer", mode = "n" },

  { "<leader>bn", helpers.execute_command("enew"), desc = "New buffer", mode = "n" },
  { "<leader>bh", helpers.execute_command("new"), desc = "New horizontal buffer", mode = "n" },
  { "<leader>bv", helpers.execute_command("vnew"), desc = "New vertical buffer", mode = "n" },
  { "<leader>b]", helpers.execute_command("bnext"), desc = "Next buffer", mode = "n" },
  { "<leader>b[", helpers.execute_command("bprev"), desc = "Prev buffer", mode = "n" },
  { "<leader>bk", helpers.kill_buffer, desc = "Kill buffer", mode = "n" },
  { "<leader>bO", helpers.kill_all_buffers_except_current, desc = "Kill all buffers except current", mode = "n" },

  { "<leader>bZ", helpers.delete_scratch_files, desc = "Delete all scratch files", mode = "n" },

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
  {
    "<leader>tt",
    function()
      snacks.explorer()
    end,
    desc = "Toggle sidebar",
    mode = "n",
  },
  { "<leader>tT", helpers.snacks_explorer_focus, desc = "Focus  sidebar", mode = "n" },
  { "<leader>ti", helpers.indent_lines, desc = "Indent lines", mode = "n" },
  { "<leader>tz", helpers.execute_command("ZenMode"), desc = "Zen mode", mode = "n" },

  ------------
  --- OPEN ---
  ------------
  { "<leader>o", group = "open" },
  { "<leader>ot", snacks.terminal.toggle, desc = "Toggle terminal", mode = { "n", "t" } },
  { "<leader>oT", snacks.terminal.open, desc = "New terminal", mode = { "n", "t" } },
  { "<leader>oq", "<cmd>Trouble quickfix toggle<cr>", desc = "Quickfix List", mode = { "n", "t" } },

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
  --- REPLACE ---
  ---------------
  { "<leader>r", group = "replace" },

  { "<leader>rr", "<CMD>SearchReplaceSingleBufferOpen<CR>", desc = "Replace", mode = { "n" } },
  { "<leader>rp", "<CMD>SearchReplaceSingleBufferCWord<CR>", desc = "Replace in place", mode = "n" },
  { "<leader>r?", "<CMD>SearchReplaceSingleBufferSelections<CR>", desc = "Show all values", mode = "n" },

  { "<leader>rR", "<CMD>GrugFar<CR>", desc = "Replace in project", mode = "n" },
  {
    "<leader>rf",
    function()
      grug_far.open({ prefills = { paths = vim.fn.expand("%") } })
    end,
    desc = "Replace in buffer [grug]",
    mode = "n",
  },
  {
    "<leader>rP",
    function()
      grug_far.open({ prefills = { search = vim.fn.expand("<cword>") } })
    end,
    desc = "Replace in place [project]",
    mode = "n",
  },
  {
    "<leader>rv",
    function()
      grug_far.open({ visualSelectionUsage = "operate-within-range" })
    end,
    desc = "Replace in range",
    mode = "v",
  },

  --------------
  --- SEARCH ---
  --------------
  { "<leader>s", group = "search" },

  { "<leader>ss", helpers.search_buffer, desc = "Search in buffer", mode = "n" },
  { "<leader>sb", helpers.search_buffer, desc = "Search in buffer", mode = "n" },
  { "<leader>sB", snacks.picker.grep_buffers, desc = "Seach all open buffers", mode = "n" },
  {
    "<leader>sw",
    function()
      snacks.picker.grep_word()
    end,
    desc = "Search word at point/selection",
    mode = { "n", "x" },
  },
  { "<leader>sp", helpers.search_project, desc = "Search in project", mode = "n" },
  {
    "<leader>sc",
    function()
      snacks.picker.command_history()
    end,
    desc = "Command History",
  },
  {
    "<leader>sh",
    function()
      snacks.picker.help()
    end,
    desc = "Help Pages",
  },
  {
    "<leader>sj",
    function()
      snacks.picker.jumps()
    end,
    desc = "Jumps",
  },
  {
    "<leader>sk",
    function()
      snacks.picker.keymaps()
    end,
    desc = "Keymaps",
  },
  {
    "<leader>sm",
    function()
      snacks.picker.marks()
    end,
    desc = "Marks",
  },
  { "<leader>sM", man_picker.open, desc = "Man pages", mode = "n" },
  {
    "<leader>su",
    function()
      snacks.picker.undo()
    end,
    desc = "Undo History",
  },
  {
    "<leader>s'",
    function()
      snacks.picker.registers()
    end,
    desc = "Registers",
  },
  { "<leader>st", group = "list TODO" },
  {
    "<leader>stt",
    function()
      snacks.picker.todo_comments({ keywords = { "TODO" }, hidden = true })
    end,
    desc = "List all TODOS",
    mode = "n",
  },
  {
    "<leader>stf",
    function()
      snacks.picker.todo_comments({ keywords = { "FIXME", "FIX" }, hidden = true })
    end,
    desc = "List all FIXME",
    mode = "n",
  },
  {
    "<leader>stw",
    function()
      snacks.picker.todo_comments({ keywords = { "WARN", "WARNING" }, hidden = true })
    end,
    desc = "List all WARNING",
    mode = "n",
  },

  -----------
  --- GIT ---
  -----------
  { "<leader>g", group = "git" },

  { "<leader>gg", helpers.execute_command("Neogit kind=replace"), desc = "Git status", mode = "n" },

  { "<leader>gs", helpers.execute_command("Gitsigns stage_hunk"), desc = "Stage hunk", mode = "n" },
  { "<leader>gS", helpers.execute_command("Gitsigns stage_buffer"), desc = "Stage file", mode = "n" },
  { "<leader>gr", helpers.execute_command("Gitsigns reset_hunk"), desc = "Reset hunk", mode = "n" },
  { "<leader>gR", helpers.execute_command("Gitsigns reset_buffer"), desc = "Reset file", mode = "n" },
  { "<leader>gv", helpers.execute_command("Gitsigns select_hunk"), desc = "Select hunk", mode = "n" },

  { "<leader>g[", helpers.execute_command("Gitsigns nav_hunk prev"), desc = "Prev hunk", mode = "n" },
  { "<leader>g]", helpers.execute_command("Gitsigns nav_hunk next"), desc = "Next hunk", mode = "n" },

  {
    "<leader>gp",
    helpers.execute_command("Gitsigns preview_hunk_inline"),
    desc = "Preview hunk [inline]",
    mode = "n",
  },
  { "<leader>gP", helpers.execute_command("Gitsigns preview_hunk"), desc = "Previw hunk [popup]", mode = "n" },

  { "<leader>gd", helpers.execute_command("Gitsigns diffthis"), desc = "Diff this file", mode = "n" },
  { "<leader>gc", helpers.execute_command("Gitsigns setqflist all"), desc = "Trouble changes", mode = "n" },
  {
    "<leader>gC",
    function()
      snacks.picker.git_diff()
    end,
    desc = "Snacks changes",
    mode = "n",
  },
  { "<leader>gw", helpers.execute_command("Gitsigns toggle_word_diff"), desc = "Toggle word diff", mode = "n" },
  {
    "<leader>gl",
    function()
      snacks.picker.git_log_file()
    end,
    desc = "File log",
    mode = "n",
  },
  {
    "<leader>gL",
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
    "<leader>gb",
    function()
      snacks.picker.git_branches()
    end,
    desc = "Switch branch",
    mode = "n",
  },

  {
    "<leader>go",
    function()
      snacks.gitbrowse()
    end,
    desc = "Git Browse",
  },

  { "<M-a>", helpers.execute_command("GitsignsBlameToggle"), desc = "Git side annotations", mode = "n" },
  { "<M-A>", helpers.execute_command("Gitsigns blame_line"), desc = "Git popup annotations", mode = "n" },

  -----------------------------
  --- RESOLVE GIT CONFLICTS ---
  -----------------------------

  { "<leader>d", group = "resolve conflicts" },

  { "<leader>dd", helpers.execute_command("DiffviewOpen"), desc = "Open Diff View", mode = "n" },
  { "<leader>dh", helpers.execute_command("DiffviewFileHistory %"), desc = "Diff history of current file", mode = "n" },
  { "<leader>dH", helpers.execute_command("DiffviewFileHistory"), desc = "Diff history all files", mode = "n" },
  -- Run this command from the PR branch
  {
    "<leader>dP",
    helpers.execute_command("DiffviewOpen origin/HEAD...HEAD --imply-local"),
    desc = "Review PR",
    mode = "n",
  },

  { "<leader>d1", helpers.execute_command("DiffviewOpen HEAD~1"), desc = "Diff HEAD~1", mode = "n" },
  { "<leader>d2", helpers.execute_command("DiffviewOpen HEAD~2"), desc = "Diff HEAD~2", mode = "n" },
  { "<leader>d3", helpers.execute_command("DiffviewOpen HEAD~3"), desc = "Diff HEAD~3", mode = "n" },

  -----------
  --- LSP ---
  -----------
  { "<leader>c", group = "code" },
  -- All mappings are located in plugins/lsp.lua and partially in trouble.lua

  -----------------------------
  --- TROUBLE [DIAGNOSTICS] ---
  -----------------------------
  { "<leader>x", group = "trouble" },
  -- All mappings are located in plugins/trouble.lua

  ----------
  --- AI ---
  ----------

  -- Avante

  { "<leader>a", group = "Avante" },

  { "<leader>aa", "<cmd>AvanteToggle<CR>", desc = "Ask", mode = { "n", "v" } },
  { "<leader>ae", "<cmd>AvanteEdit<CR>", desc = "Edit", mode = { "n", "v" } },
  { "<leader>an", "<cmd>AvanteChatNew<CR>", desc = "Chat New", mode = { "n" } },
  { "<leader>ah", "<cmd>AvanteHistory<CR>", desc = "History", mode = { "n" } },
  { "<leader>aC", "<cmd>AvanteClear<CR>", desc = "Clear Chat", mode = { "n" } },
  { "<leader>af", "<cmd>AvanteFocus<CR>", desc = "Focus", mode = { "n" } },
  { "<leader>aR", "<cmd>AvanteRefresh<CR>", desc = "Refresh All Windows", mode = { "n" } },
  { "<leader>aS", "<cmd>AvanteStop<CR>", desc = "Stop Request", mode = { "n" } },
  { "<leader>a?", "<cmd>AvanteModels<CR>", desc = "Switch Model", mode = { "n" } },

  -- ChatGPT

  -- edit_with_instructions = {
  --   keymaps = {
  --     toggle_help = "<C-h>", (list of all keys)
  -- },
  { "<leader>;", group = "ChatGPT" },

  { "<leader>;c", "<cmd>ChatGPT<CR>", desc = "ChatGPT", mode = { "n", "v" } },
  { "<leader>;i", "<cmd>ChatGPTEditWithInstruction<CR>", desc = "Edit with instruction", mode = { "n", "v" } },

  { "<leader>;g", "<cmd>ChatGPTRun grammar_correction<CR>", desc = "Grammar Correction", mode = { "n", "v" } },
  { "<leader>;d", "<cmd>ChatGPTRun docstring<CR>", desc = "Docstring", mode = { "n", "v" } },
  { "<leader>;t", "<cmd>ChatGPTRun add_tests<CR>", desc = "Add Tests", mode = { "n", "v" } },
  { "<leader>;o", "<cmd>ChatGPTRun optimize_code<CR>", desc = "Optimize Code", mode = { "n", "v" } },
  { "<leader>;s", "<cmd>ChatGPTRun summarize<CR>", desc = "Summarize", mode = { "n", "v" } },
  { "<leader>;f", "<cmd>ChatGPTRun fix_bugs<CR>", desc = "Fix Bugs", mode = { "n", "v" } },
  { "<leader>;e", "<cmd>ChatGPTRun explain_code<CR>", desc = "Explain Code", mode = { "n", "v" } },
  { "<leader>;x", "<cmd>ChatGPTRun explain_diagnostic<CR>", desc = "Explain Error", mode = { "n", "v" } },

  -- Copilot
  {
    "<leader>cc",
    helpers.execute_command("Copilot enable", function()
      vim.notify("Copilot: enabled", vim.log.levels.INFO)
    end),
    desc = "Copilot: enable",
    mode = { "n", "v" },
  },
  {
    "<leader>cC",
    helpers.execute_command("Copilot disable", function()
      vim.notify("Copilot: disabled", vim.log.levels.INFO)
    end),
    desc = "Copilot: disable",
    mode = { "n", "v" },
  },

  -----------------
  --- BOOKMARKS ---
  -----------------
  { "<leader>m", group = "bookmarks" },

  { "<leader>ms", bookmarks.bookmark_toggle, desc = "Set bookmark", mode = "n" },
  { "<leader>md", helpers.delete_all_bookmarks, desc = "Delete all bookmarks", mode = "n" },
  {
    "<leader>mf",
    bookmark_picker.open,
    desc = "Find bookmarks",
    mode = "n",
  },
  { "b]", bookmarks.bookmark_next, desc = "Next bookmark", mode = "n" },
  { "b[", bookmarks.bookmark_prev, desc = "Prev bookmark", mode = "n" },

  ---------------
  --- SESSION ---
  ---------------
  { "<leader>q", group = "session" },

  { "<leader>qq", "<CMD>qa<CR>", desc = "Quit", mode = "n" },
  { "<leader>qQ", quit.confirm_quit, desc = "Quit + confirm", mode = "n" },

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

  { "<leader>ht", helpers.toggle_background, desc = "Toggle dark/light theme", mode = "n" },
  { "<leader>hs", ":set spell<CR>", desc = "Enable spellcheck", mode = "n" },
  { "<leader>hS", ":set nospell<CR>", desc = "Disable spellcheck", mode = "n" },
  {
    "<leader>h=",
    helpers.execute_command("lua Snacks.picker.spelling()"),
    desc = "Spelling suggestions",
    mode = "n",
  },
  -- Handy for checking 'filetype' which is needed for plugins/formatter.lua
  { "<leader>hf", ":lua print('Filetype:', vim.bo.filetype)<CR>", desc = "Print filetype", mode = "n" },
  { "<leader>hh", helpers.execute_command("Noice"), desc = "Messages history", mode = "n" },

  ----------------
  --- COMMENTS ---
  ----------------
  { "<C-/>", ":norm gcc<CR>", desc = "Toggle comment", mode = "n" },
  { "<C-/>", ":norm gcc<CR>", desc = "Toggle multiline comment", mode = "v" },

  -------------
  --- OTHER ---
  -------------
  { "<C-s>", "<cmd>w<CR>", desc = "Save current buffer [n]", mode = "n" },
  { "<C-s>", "<Esc><cmd>w<CR>", desc = "Save current buffer [i]", mode = "i" },
  { "<C-c>", "<cmd>%y+<CR>", desc = "Copy entire buffer", mode = "n" },
  { "<Esc>", "<cmd>noh<CR>", desc = "Clean seach highlights", mode = "n" },
  { "<leader>`", "<cmd>messages<CR>", desc = "Messages", mode = "n" },
  { "<leader>/", helpers.search_project, desc = "Search in project", mode = "n" },
  {
    "<C-l>",
    'copilot#Accept("\\<CR>")',
    desc = "Copilot Accept",
    expr = true,
    replace_keycodes = false,
    mode = "i",
  },
  { "ZQ", quit.confirm_quit, desc = "Confirm before ZQ" },
  {
    "ZZ",
    function()
      if vim.bo.modified then
        vim.cmd("write")
      end

      quit.confirm_quit()
    end,
    desc = "Confirm before ZZ",
  },

  {
    "]t",
    function()
      helpers.todo_jump(todo_comments.jump_next)
    end,
    desc = "Next todo comment",
    mode = "n",
  },
  {
    "[t",
    function()
      helpers.todo_jump(todo_comments.jump_prev)
    end,
    desc = "Prev todo comment",
    mode = "n",
  },
  -- Window sizing
  { "<C-Up>", "<cmd>resize +2<CR>", mode = "n", desc = "Increase window height" },
  { "<C-Down>", "<cmd>resize -2<CR>", mode = "n", desc = "Decrease window height" },
  { "<C-Right>", "<cmd>vertical resize +2<CR>", mode = "n", desc = "Increase window width" },
  { "<C-Left>", "<cmd>vertical resize -2<CR>", mode = "n", desc = "Decrease window width" },
})
