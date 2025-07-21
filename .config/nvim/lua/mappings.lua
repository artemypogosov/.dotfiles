require "nvchad.mappings"


local base46 = require("base46")
local bm = require "bookmarks"
local telescope = require('telescope.builtin')

-----------------
--- FUNCTIONS ---
-----------------
local function is_git_repo()
  -- Check if the current directory contains a .git folder
  local git_dir = vim.fn.finddir('.git', vim.fn.expand('%:p:h') .. ';')
  return git_dir ~= ''
end

local function find_git_files()
  if is_git_repo() then
    telescope.git_files()
  else
    print("Not a git repository")
  end
end

local function delete_recent_files()
  local answer = vim.fn.input("Clear recent files? (yes/no): ")
  if answer:lower() == "yes" then
    vim.cmd("!rm ~/.local/state/nvim/shada/main.shada")
  end
end

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

local function kill_all_buffers()
  local answer = vim.fn.input("Delete all buffers? (y/n): ")
  if answer:lower() == "y" then
    vim.cmd("%bd")
  end
end

local function search_project()
  require("telescope.builtin").live_grep({
    additional_args = function()
      return { "--hidden", "--glob", "!.git/*" }
    end
  })
end

----------------
--- MAPPINGS ---
----------------
local wk = require("which-key")
wk.add({
  ------------
  --- FILE ---
  ------------
  { "<leader>f", group = "file" },

  { "<leader>ff", execute_command("Telescope file_browser previewer=false"), desc = "Find file", mode = "n" },
  { "<leader>.",  execute_command("Telescope file_browser previewer=false"), desc = "Find file", mode = "n" },
  { "<leader>fr", execute_command("Telescope oldfiles previewer=false"), desc = "Recent files", mode = "n" },

  { "<leader>fd", execute_command("Telescope find_files previewer=false"), desc = "Find file in dir", mode = "n" },
  { "<leader><leader>", find_git_files, desc = "Find file from here", mode = "n" },

  { "<leader>fZ", delete_recent_files, desc = "Delete recent files list", mode = "n" },

  ---------------
  --- PROJECT ---
  ---------------
  { "<leader>p", group = "project" },

  { "<leader>pp", execute_command("Telescope workspaces"), desc = "Switch project", mode = "n" },
  { "<leader>pl", execute_command("WorkspacesList"), desc = "List workspaces", mode = "n" },
  { "<leader>pL", execute_command("WorkspacesListDirs"), desc = "List projects' dirs", mode = "n" },

  { "<leader>pa", prefill_command("WorkspacesAdd"), desc = "Add project", mode = "n" },
  { "<leader>pA", prefill_command("WorkspacesAddDir"), desc = "Add projects dir", mode = "n" },
  { "<leader>pr", prefill_command("WorkspacesRename"), desc = "Rename project", mode = "n" },
  { "<leader>pd", prefill_command("WorkspacesRemove"), desc = "Remove project", mode = "n" },
  { "<leader>pD", prefill_command("WorkspacesRemoveDir"), desc = "Remove dir", mode = "n" },

  --------------
  --- WINDOW ---
  --------------
  { "<leader>w", group = "window" },

  { "<leader>ww", "<C-w>w",  desc = "Next window",  mode = "n" },
  { "<leader>wh", "<C-w>h",  desc = "Switch left",  mode = "n" },
  { "<leader>wl", "<C-w>l",  desc = "Switch right", mode = "n" },
  { "<leader>wj", "<C-w>j",  desc = "Switch down",  mode = "n" },
  { "<leader>wk", "<C-w>k",  desc = "Switch up",    mode = "n" },

  { "<leader>ws", "<C-w>s",  desc = "Split H", mode = "n" },
  { "<leader>wv", "<C-w>v",  desc = "Split V", mode = "n" },

  { "<leader>wH", "<C-w>H",  desc = "Move left",      mode = "n" },
  { "<leader>wL", "<C-w>L",  desc = "Move right",     mode = "n" },
  { "<leader>wJ", "<C-w>J",  desc = "Move down",      mode = "n" },
  { "<leader>wK", "<C-w>K",  desc = "Move up",        mode = "n" },
  { "<leader>wx", "<C-w>x",  desc = "Swap with next", mode = "n" },

  { "<leader>wc", "<C-w>c",  desc = "Close",       mode = "n" },
  { "<leader>wC", "<C-w>o",  desc = "Close other windows", mode = "n" },

  { "<leader>wm",  execute_command("MaximizerToggle"),      desc = "Maximize window", mode = "n" },
  { "<leader>wih", execute_command("resize 40"),            desc = "Increase height", mode = "n" },
  { "<leader>wiw", execute_command("vertical resize 150"),  desc = "Increase width",  mode = "n" },
  { "<leader>w=", "<C-w>=",  desc = "=",       mode = "n" },

  --------------
  --- TOGGLE ---
  --------------
  { "<leader>th", execute_command("Nvdash"),         desc = "Nvdash",         mode = "n" },
  { "<leader>tt", execute_command("NvimTreeToggle"), desc = "Toggle sidebar", mode = "n" },
  { "<leader>tf", execute_command("NvimTreeFocus"),  desc = "Focus  sidebar", mode = "n" },
  { "<leader>tz", execute_command("ZenMode"),        desc = "Zen mode",       mode = "n" },

  -----------------
  --- WORKSPACE ---
  -----------------
  { "<leader><Tab>n", execute_command("tabnew"),    desc = "New workspace",    mode = "n"  },
  { "<leader><Tab>k", execute_command("tabclose"),  desc = "Delete workspace", mode = "n" },
  { "<leader><Tab>]", execute_command("tabnext"),   desc = "Next workspace",   mode = "n" },
  { "<leader><Tab>[", execute_command("tabNext"),   desc = "Prev workspace",   mode = "n" },

  --------------
  --- BUFFER ---
  --------------
  { "<leader>bb", execute_command("Telescope buffers previewer=false"), desc = "Switch buffer", mode = "n" },
  { "<leader>,",  execute_command("Telescope buffers previewer=false"), desc = "Switch buffer", mode = "n" },

  { "<leader>bN", execute_command("enew"),    desc = "New buffer",            mode = "n" },
  { "<leader>bH", execute_command("new"),     desc = "New horizontal buffer", mode = "n" },
  { "<leader>bV", execute_command("vnew"),    desc = "New vertical buffer",   mode = "n" },
  { "<leader>b]", execute_command("bnext"),   desc = "Next buffer",           mode = "n" }, -- tab,
  { "<leader>b[", execute_command("bprev"),   desc = "Prev buffer",           mode = "n" }, -- shift + tab,
  { "<leader>bk", execute_command("bd"),      desc = "Kill buffer",           mode = "n" },
  { "<leader>bD", execute_command("%bd|e#"),  desc = "Kill all buffers except current", mode = "n" },
  { "<leader>bK", kill_all_buffers,  desc = "Kill all buffers", mode = "n" },

  --------------
  --- SEARCH ---
  --------------
  { "<leader>ss", execute_command("Telescope current_buffer_fuzzy_find previewer=false"),  desc = "Search in buffer", mode = "n" },
  { "<leader>sb", execute_command("Telescope current_buffer_fuzzy_find previewer=false"),  desc = "Search in buffer", mode = "n" },
  { "<leader>sp", search_project, desc = "Search in project", mode = "n" },

  -----------
  --- GIT ---
  -----------
  { "<leader>gg", execute_command("Neogit kind=replace"), desc = "Git status", mode = "n" },
  { "<leader>gb", execute_command("Neogit branch"),       desc = "Switch branch", mode = "n" },
  { "<leader>gB", execute_command("BlameToggle"),  desc = "Git annotations", mode = "n" },
  { "<leader>gl", execute_command("Neogit log"),          desc = "Git log", mode = "n" },
  { "<leader>gL", execute_command("Telescope git_bcommits path=%:p:h"),  desc = "File commits", mode = "n" },

  -----------
  --- LSP ---
  -----------
  { "<leader>cf", function() require("conform").format({ lsp_fallback = true }) end, mode = "n", desc = "Format buffer" },
  { "<leader>cR",  require "nvchad.lsp.renamer",  desc = "Rename var", mode = "n" },
  { "<leader>cA",  vim.lsp.buf.code_action,       desc = "LSP Action", mode = "n" },

  { "<leader>ct", ":Telescope treesitter<CR>",            desc = "Treesitter", mode = "n" },
  { "<leader>cD", ":Telescope diagnostics bufnr=0<CR>",   desc = "Diagnostic", mode = "n" },
  { "<leader>cd", ":Telescope lsp_definitions<CR>",       desc = "Definitions", mode = "n" },
  { "<leader>cr", ":Telescope lsp_references<CR>",        desc = "References", mode = "n" },
  { "<leader>ci", ":Telescope lsp_implementations<CR>",   desc = "Implementations", mode = "n" },
  { "<leader>ct", ":Telescope lsp_type_definitions<CR>",  desc = "Type definitions", mode = "n" },

  -----------------
  --- BOOKMARKS ---
  -----------------
  { "mm", bm.bookmark_toggle,     desc = "Set bookmark",        mode = "n" },
  { "m]", bm.bookmark_next,       desc = "Next bookmark",       mode = "n" },
  { "m[", bm.bookmark_prev,       desc = "Prev bookmark",       mode = "n" },
  { "mc", bm.bookmark_clean,      desc = "Clean all bookmarks", mode = "n" },
  { "mC", bm.bookmark_clear_all,  desc = "Clear ALL bookmarks", mode = "n" },
  { "mf", ":Telescope bookmarks list previewer=false<CR>",  desc = "Show bookmarks", mode = "n" },

  -------------
  --- TO-DO ---
  -------------
   { "<C-t>t", ":TodoTelescope keywords=TODO<CR>",       desc = "List all TODOS", mode = "n" },
   { "<C-t>f", ":TodoTelescope keywords=FIX,FIXME<CR>",  desc = "List all FIXME", mode = "n" },

  ----------------
  --- COMMENTS ---
  ----------------
   { "<C-/>", ":norm gccj<CR>",  desc = "Toggle comment", mode = "n"},
   { "<C-/>", ":norm gcc<CR>",  desc = "Toggle multiline comment", mode = "v"},

  ------------
  --- HELP ---
  ------------
   { "<leader>hr", ":Telescope spell_suggest<CR>",  desc = "Spelling suggestions", mode = "n" },
   { "<leader>ht",  base46.toggle_theme,     desc = "Toggle theme", mode = "n" },
   { "<leader>hs",  ":set spell<CR>",     desc = "Enable spellcheck", mode = "n" },
   { "<leader>hS",  ":set nospell<CR>",   desc = "Disable spellcheck", mode = "n" },
})

-- ctrl+wd (lsp diagnostic on the cursor) --> not able to bind it to map()

-- KEYMAP REMOVAL
vim.keymap.del("n", "<C-n>")
vim.keymap.del("n", "<leader>b")
vim.keymap.del("n", "<leader>x")
vim.keymap.del("n", "<leader>n")
vim.keymap.del("n", "<leader>/")
vim.keymap.del("n", "<leader>h")
vim.keymap.del("n", "<leader>v")
vim.keymap.del("n", "<leader>e")
vim.keymap.del("n", "<leader>ch")
vim.keymap.del("n", "<leader>cm")
vim.keymap.del("n", "<leader>fo")
vim.keymap.del("n", "<leader>fb")
vim.keymap.del("n", "<leader>fh")
vim.keymap.del("n", "<leader>fm")
vim.keymap.del("n", "<leader>fa")
vim.keymap.del("n", "<leader>fw")
vim.keymap.del("n", "<leader>fz")
vim.keymap.del("n", "<leader>ma")
vim.keymap.del("n", "<leader>pt")
vim.keymap.del("n", "<leader>gt")
