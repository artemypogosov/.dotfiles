local wk = require("which-key")
local bm = require("bookmarks")
local telescope = require("telescope.builtin")
local telescope_global = require("telescope")
local confirm_quit = require("confirm-quit")
local workspaces_extension = require("telescope._extensions.workspaces")

----------------------
--- HELP FUNCTIONS ---
----------------------
local function prefill_command(command)
  return function()
    vim.api.nvim_feedkeys(":" .. command .. " ", "n", true)
  end
end

local function execute_command(command)
  return function()
    vim.cmd(command)
  end
end

function ToggleBackground()
  local o = vim.o
  if o.background == "dark" then
    o.background = "light"
  else
    o.background = "dark"
  end
end

local function is_git_repo()
  -- Check if the current directory contains a .git folder
  local git_dir = vim.fn.finddir(".git", vim.fn.expand("%:p:h") .. ";")
  return git_dir ~= ""
end

local function find_git_files()
  if is_git_repo() then
    telescope.git_files()
  else
    print("Not a git repository")
  end
end

local function find_recent_files()
  telescope.oldfiles({
    previewer = false,
    layout_config = { width = 0.4, height = 0.3 },
  })
end

local function switch_project()
  telescope_global.extensions.workspaces.workspaces({
    layout_config = {
      width = 0.355,
      height = 0.27,
      prompt_position = "top",
    },
  })
end

local function search_project()
  telescope.live_grep({
    additional_args = function()
      return { "--hidden", "--glob", "!.git/*" }
    end,
  })
end

local function delete_recent_files()
  local answer = vim.fn.input("Clear recent files? (yes/no): ")
  if answer:lower() == "yes" then
    vim.cmd("!rm ~/.local/state/nvim/shada/main.shada")
  end
end

local function delete_all_bookmarks()
  local answer = vim.fn.input("Delete all bookmarks? (yes/no): ")
  if answer:lower() == "yes" then
    bm.bookmark_clean()
  end
end

local function kill_all_buffers()
  local answer = vim.fn.input("Delete all buffers? (y/n): ")
  if answer:lower() == "y" then
    vim.cmd("%bd")
  end
end

----------------
--- MAPPINGS ---
----------------
wk.add({
  ------------
  --- FILE ---
  ------------
  { "<leader>f", group = "file" },

  {
    "<leader>ff",
    execute_command("Telescope file_browser previewer=false"),
    desc = "Find file",
    mode = "n",
  },
  {
    "<leader>.",
    execute_command("Telescope file_browser previewer=false"),
    desc = "Find file",
    mode = "n",
  },
  { "<leader>fr", find_recent_files, desc = "Recent files", mode = "n" },
  {
    "<leader>fd",
    execute_command("Telescope find_files previewer=false"),
    desc = "Find file from here",
    mode = "n",
  },
  { "<leader><leader>", find_git_files, desc = "Find files in project", mode = "n" },
  { "<leader>fZ", delete_recent_files, desc = "Delete recent files", mode = "n" },

  ---------------
  --- PROJECT ---
  ---------------
  { "<leader>p", group = "project" },

  { "<leader>pp", switch_project, desc = "Switch project", mode = "n" },
  { "<leader>pl", execute_command("WorkspacesList"), desc = "List workspaces", mode = "n" },
  { "<leader>pL", execute_command("WorkspacesListDirs"), desc = "List projects' dirs", mode = "n" },

  { "<leader>pa", prefill_command("WorkspacesAdd"), desc = "Add project", mode = "n" },
  { "<leader>pA", prefill_command("WorkspacesAddDir"), desc = "Add project dir", mode = "n" },
  { "<leader>pr", prefill_command("WorkspacesRename"), desc = "Rename project", mode = "n" },
  { "<leader>pd", prefill_command("WorkspacesRemove"), desc = "Remove project", mode = "n" },
  { "<leader>pD", prefill_command("WorkspacesRemoveDir"), desc = "Remove project dir", mode = "n" },

  ---------------
  --- WINDOWS ---
  ---------------
  { "<leader>w", group = "window" },

  { "<leader>ww", "<C-w>w", desc = "Next window", mode = "n" },
  { "<leader>wh", "<C-w>h", desc = "Switch left", mode = "n" },
  { "<leader>wl", "<C-w>l", desc = "Switch right", mode = "n" },
  { "<leader>wj", "<C-w>j", desc = "Switch down", mode = "n" },
  { "<leader>wk", "<C-w>k", desc = "Switch up", mode = "n" },

  { "<leader>ws", "<C-w>s", desc = "Split H", mode = "n" },
  { "<leader>wv", "<C-w>v", desc = "Split V", mode = "n" },

  { "<leader>wH", "<C-w>H", desc = "Move left", mode = "n" },
  { "<leader>wL", "<C-w>L", desc = "Move right", mode = "n" },
  { "<leader>wJ", "<C-w>J", desc = "Move down", mode = "n" },
  { "<leader>wK", "<C-w>K", desc = "Move up", mode = "n" },
  { "<leader>wx", "<C-w>x", desc = "Swap with next", mode = "n" },

  { "<leader>wc", "<C-w>c", desc = "Close", mode = "n" },
  { "<leader>wC", "<C-w>o", desc = "Close other windows", mode = "n" },

  { "<leader>wm", execute_command("MaximizerToggle"), desc = "Maximize window", mode = "n" },
  { "<leader>wih", execute_command("resize 40"), desc = "Increase height", mode = "n" },
  { "<leader>wiw", execute_command("vertical resize 150"), desc = "Increase width", mode = "n" },
  { "<leader>w=", "<C-w>=", desc = "=", mode = "n" },

  --------------
  --- TOGGLE ---
  --------------
  { "<leader>tt", execute_command("NvimTreeToggle ."), desc = "Toggle sidebar", mode = "n" },
  { "<leader>tf", execute_command("NvimTreeFocus"), desc = "Focus  sidebar", mode = "n" },
  {
    "<leader>tF",
    execute_command("NvimTreeFindFile"),
    desc = "Point file in structure",
    mode = "n",
  },
  { "<leader>tz", execute_command("ZenMode"), desc = "Zen mode", mode = "n" },

  ------------------
  --- WORKSPACES ---
  ------------------
  { "<leader><Tab>", group = "workspace" },

  { "<leader><Tab>n", execute_command("tabnew"), desc = "New workspace", mode = "n" },
  { "<leader><Tab>k", execute_command("tabclose"), desc = "Delete workspace", mode = "n" },
  { "<leader><Tab>]", execute_command("tabnext"), desc = "Next workspace", mode = "n" },
  { "<leader><Tab>[", execute_command("tabNext"), desc = "Prev workspace", mode = "n" },
  { "<leader><Tab>r", prefill_command("LualineRenameTab"), desc = "Rename workspace", mode = "n" },

  --------------
  --- BUFFER ---
  --------------
  { "<leader>b", group = "buffer" },

  {
    "<leader>bb",
    execute_command("Telescope buffers previewer=false"),
    desc = "Switch buffer",
    mode = "n",
  },
  {
    "<leader>,",
    execute_command("Telescope buffers previewer=false"),
    desc = "Switch buffer",
    mode = "n",
  },

  { "<leader>bn", execute_command("enew"), desc = "New buffer", mode = "n" },
  { "<leader>bh", execute_command("new"), desc = "New horizontal buffer", mode = "n" },
  { "<leader>bv", execute_command("vnew"), desc = "New vertical buffer", mode = "n" },
  { "<leader>b]", execute_command("bnext"), desc = "Next buffer", mode = "n" }, -- tab,
  { "<leader>b[", execute_command("bprev"), desc = "Prev buffer", mode = "n" }, -- shift + tab,
  { "<leader>bk", execute_command("bd"), desc = "Kill buffer", mode = "n" },

  { "<leader>bD", execute_command("%bd|e#"), desc = "Kill all buffers except current", mode = "n" },
  { "<leader>bK", kill_all_buffers, desc = "Kill all buffers", mode = "n" },

  --------------
  --- SEARCH ---
  --------------
  { "<leader>s", group = "search" },

  {
    "<leader>ss",
    execute_command("Telescope current_buffer_fuzzy_find previewer=false"),
    desc = "Search in buffer",
    mode = "n",
  },
  {
    "<leader>sb",
    execute_command("Telescope current_buffer_fuzzy_find previewer=false"),
    desc = "Search in buffer",
    mode = "n",
  },
  { "<leader>sp", search_project, desc = "Search in project", mode = "n" },

  -----------
  --- GIT ---
  -----------
  { "<leader>g", group = "git" },

  { "<leader>gl", execute_command("Telescope git_commits"), desc = "Branch log", mode = "n" },
  {
    "<leader>g.",
    execute_command("Telescope git_bcommits path=%:p:h"),
    desc = "File log",
    mode = "n",
  },

  { "<leader>gg", execute_command("Neogit kind=replace"), desc = "Git status", mode = "n" },
  { "<leader>gL", execute_command("Neogit log"), desc = "Neogit: log", mode = "n" },
  { "<leader>gB", execute_command("Neogit branch"), desc = "Neogit: switch branch", mode = "n" },

  { "<M-a>", execute_command("BlameToggle"), desc = "Git annotations", mode = "n" },

  -----------
  --- LSP ---
  -----------
  { "<leader>c", group = "code" },
  -- All mappings are located in plugins/lsp.lua

  -----------------
  --- BOOKMARKS ---
  -----------------
  { "<leader>m", group = "bookmarks" },

  { "<leader>mm", bm.bookmark_toggle, desc = "Set bookmark", mode = "n" },
  { "<leader>mc", delete_all_bookmarks, desc = "Clean all bookmarks", mode = "n" },
  {
    "<leader>mf",
    ":Telescope bookmarks list previewer=false<CR>",
    desc = "Show bookmarks",
    mode = "n",
  },
  { "mn", bm.bookmark_next, desc = "Next bookmark", mode = "n" },
  { "mp", bm.bookmark_prev, desc = "Prev bookmark", mode = "n" },

  -------------
  --- TO-DO ---
  -------------
  { "<C-t>t", ":TodoTelescope keywords=TODO<CR>", desc = "List all TODOS", mode = "n" },
  { "<C-t>f", ":TodoTelescope keywords=FIX,FIXME<CR>", desc = "List all FIXME", mode = "n" },

  ----------------
  --- COMMENTS ---
  ----------------
  { "<C-/>", ":norm gccj<CR>", desc = "Toggle comment", mode = "n" },
  { "<C-/>", ":norm gcc<CR>", desc = "Toggle multiline comment", mode = "v" },

  ------------
  --- HELP ---
  ------------
  { "<leader>hL", execute_command("Lazy"), desc = "Lazy", mode = "n" },
  { "<leader>hM", execute_command("Mason"), desc = "Mason", mode = "n" },

  { "<leader>hm", execute_command("Telescope man_pages"), desc = "Man pages", mode = "n" },
  { "<leader>hs", ":set spell<CR>", desc = "Enable spellcheck", mode = "n" },
  { "<leader>hS", ":set nospell<CR>", desc = "Disable spellcheck", mode = "n" },
  { "<leader>hr", ":Telescope spell_suggest<CR>", desc = "Spelling suggestions", mode = "n" },

  -------------
  --- OTHER ---
  -------------
  { "<leader>tT", ToggleBackground, desc = "Toggle dark/light theme", mode = "n" },
  { "<C-s>", "<cmd>w<CR>", desc = "Save current buffer [n]", mode = "n" },
  { "<C-s>", "<Esc><cmd>w<CR>", desc = "Save current buffer [i]", mode = "i" },
  { "<C-c>", "<cmd>%y+<CR>", desc = "Copy entire buffer", mode = "n" },
  { "<Esc>", "<cmd>noh<CR>", desc = "Clean seach highlights", mode = "n" },

  { "<leader>bx", execute_command("lua Snacks.scratch()"), desc = "New scratch", mode = "n" },
  {
    "<leader>bX",
    execute_command("lua Snacks.scratch.select()"),
    desc = "Select scratch",
    mode = "n",
  },

  { "<leader>w", "<C-\\><C-n><C-w>", desc = "Terminal: window command", mode = "t" },
  {
    "<leader>ot",
    execute_command("ToggleTerm size=20 direction=horizontal name=Terminal"),
    desc = "Toggle terminal",
    mode = "n",
  },
  {
    "<leader>ot",
    execute_command("ToggleTerm"),
    desc = "Toggle terminal",
    mode = "t",
  },
})

-- Map ZZ to ConfirmQuit (saves and quits with confirmation)
vim.keymap.set("n", "ZZ", function()
  confirm_quit.confirm_quit()
end, { desc = "Confirm before ZZ (wq)" })

-- Map ZQ to ConfirmQuit! (force quit with confirmation)
-- vim.keymap.set("n", "ZQ", function()
-- confirm_quit.confirm_quit()
-- end, { desc = "Confirm before ZQ (q!)" })
