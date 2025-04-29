require "nvchad.mappings"


local base46 = require("base46")
local bm = require "bookmarks"
local telescope = require('telescope.builtin')

local function is_git_repo()
  -- Check if the current directory contains a .git folder
  local git_dir = vim.fn.finddir('.git', vim.fn.expand('%:p:h') .. ';')
  return git_dir ~= ''
end

local wk = require("which-key")
wk.add({
  ----------
  ---FILE---
  ----------
  { "<leader>f", group = "file" },
  --
  { "<leader>ff", ":Telescope file_browser previewer=false<CR>", desc = "Find file", mode = "n" },
  { "<leader>.",  ":Telescope file_browser previewer=false<CR>", desc = "Find file", mode = "n" },
  --
  { "<leader>fF", ":Telescope git_files<CR>", desc = "Find git files in project", mode = "n" },
  { "<leader><leader>", function ()
    if is_git_repo() then
      -- If in a git repository, run the command
      telescope.git_files()
    else
      -- If not in a git repository, show a warning
      print("Not inside a git repository")
    end
  end, desc = "Find git files in project", mode = "n" },
  --
  { "<leader>fd", ":Telescope find_files previewer=false<CR>", desc = "Find file in dir", mode = "n" },
  --
  { "<leader>fr", ":Telescope oldfiles previewer=false <CR>", desc = "Recent files", mode = "n" },
  --
  { "<leader>fZ", function()
    local answer = vim.fn.input("Clear recent files? (yes/no): ")
    if answer:lower() == "yes" then
      vim.cmd("!rm ~/.local/state/nvim/shada/main.shada")
    end
  end, desc = "Delete recent files list", mode = "n" },
  -------------
  ---PROJECT---
  -------------
  { "<leader>p", group = "project" },
  { "<leader>pp", ":Telescope workspaces<CR>",  desc = "Switch project", mode = "n" },
  { "<leader>pa", ":WorkspacesAdd ",  desc = "Add project", mode = "n" },
  { "<leader>pA", ":WorkspacesAddDir ",  desc = "Add projects dir", mode = "n" },
  { "<leader>pr", ":WorkspacesRename ",  desc = "Rename project", mode = "n" },
  { "<leader>pl", ":WorkspacesList<CR>",  desc = "List workspaces", mode = "n" },
  { "<leader>pL", ":WorkspacesListDirs<CR>",  desc = "List projects' dirs", mode = "n" },
  { "<leader>pd", ":WorkspacesRemove ",  desc = "Remove project", mode = "n" },
  { "<leader>pD", ":WorkspacesRemoveDir ",  desc = "Remove dir", mode = "n" },
  ------------
  ---WINDOW---
  ------------
  { "<leader>w", group = "window" },

  { "<leader>ww", "<C-w>w",  desc = "Next window",  mode = "n" },
  { "<leader>w[", "<C-w>h",  desc = "Switch left",  mode = "n" },
  { "<leader>w]", "<C-w>l",  desc = "Switch right", mode = "n" },
  { "<leader>wj", "<C-w>j",  desc = "Switch down",  mode = "n" },
  { "<leader>wk", "<C-w>k",  desc = "Switch up",    mode = "n" },

  { "<leader>ws", "<C-w>s",  desc = "Split H", mode = "n" },
  { "<leader>wv", "<C-w>v",  desc = "Split V", mode = "n" },
  { "<leader>w=", "<C-w>=",  desc = "=",       mode = "n" },

  { "<leader>wH", "<C-w>H",  desc = "Move left",      mode = "n" },
  { "<leader>wL", "<C-w>L",  desc = "Move right",     mode = "n" },
  { "<leader>wJ", "<C-w>J",  desc = "Move down",      mode = "n" },
  { "<leader>wK", "<C-w>K",  desc = "Move up",        mode = "n" },
  { "<leader>wx", "<C-w>x",  desc = "Swap with next", mode = "n" },

  { "<leader>wc", "<C-w>c",  desc = "Close",       mode = "n" },
  { "<leader>wC", "<C-w>o",  desc = "Close other", mode = "n" },

  { "<leader>wm",  ":MaximizerToggle<CR>",      desc = "Maximize window", mode = "n" },
  { "<leader>wih", ":resize 40<CR>",            desc = "Increase height", mode = "n" },
  { "<leader>wiw", ":vertical resize 150<CR>",  desc = "Increase width",  mode = "n" },
  ------------
  ---TOGGLE---
  ------------
  { "<leader>th",  ":Nvdash<CR>",         desc = "Nvdash",         mode = "n" },
  { "<leader>tt",  ":NvimTreeToggle<CR>", desc = "Toggle sidebar", mode = "n" },
  { "<leader>tf",  ":NvimTreeFocus<CR>",  desc = "Focus  sidebar", mode = "n" },
  { "<leader>tz",  ":ZenMode<CR>",        desc = "Zen mode",       mode = "n" },
  ---------------
  ---WORKSPACE---
  ---------------
  { "<leader><Tab>n", ":tabnew<CR>",    desc = "New workspace",    mode = "n"  },
  { "<leader><Tab>k", ":tabclose<CR>",  desc = "Delete workspace", mode = "n" },
  { "<leader><Tab>]", ":tabnext<CR>",   desc = "Next workspace",   mode = "n" },
  { "<leader><Tab>[", ":tabNext<CR>",   desc = "Prev workspace",   mode = "n" },
  ---------------
  ---BUFFER---
  ---------------
  { "<leader>bb", ":Telescope buffers previewer=false <CR>", desc = "Switch buffer", mode = "n" },
  { "<leader>,",  ":Telescope buffers previewer=false <CR>", desc = "Switch buffer", mode = "n" },

  { "<leader>bN", ":enew<CR>",    desc = "New buffer",            mode = "n" },
  { "<leader>bH", ":new<CR>",     desc = "New horizontal buffer", mode = "n" },
  { "<leader>bV", ":vnew<CR>",    desc = "New vertical buffer",   mode = "n" },
  { "<leader>b]", ":bnext<CR>",   desc = "Next buffer",           mode = "n" }, -- tab,
  { "<leader>b[", ":bprev<CR>",   desc = "Prev buffer",           mode = "n" }, -- shift + tab,
  { "<leader>bk", ":bd<CR>",      desc = "Kill buffer",           mode = "n" },
  { "<leader>bD", ":%bd|e#<CR>",  desc = "Kill all buffers except current", mode = "n" },
  { "<leader>bK",
    function()
      local answer = vim.fn.input("Delete all buffers? (y/n): ")
      if answer:lower() == "y" then
        vim.cmd("%bd")
      end
    end,  desc = "Kill all buffers", mode = "n" },
  ------------
  ---SEARCH---
  ------------
  { "<leader>ss", ":Telescope current_buffer_fuzzy_find previewer=false <CR>",  desc = "Search in buffer", mode = "n" },
  { "<leader>sb", ":Telescope current_buffer_fuzzy_find previewer=false <CR>",  desc = "Search in buffer", mode = "n" },
  ---------
  ---GIT---
  ---------
  { "<leader>gg", ":Neogit kind=replace<CR>", desc = "Git status", mode = "n" },
  { "<leader>gb", ":Neogit branch<CR>",       desc = "Switch branch", mode = "n" },
  { "<leader>gB", ":BlameToggle<CR>",  desc = "Git annotations", mode = "n" },
  { "<leader>gl", ":Neogit log<CR>",          desc = "Git log", mode = "n" },
  { "<leader>gL", ":Telescope git_bcommits path=%:p:h<CR>",  desc = "File's commits", mode = "n" },
  ---------
  ---LSP---
  ---------
  { "<leader>cf", function() require("conform").format({ lsp_fallback = true }) end, mode = "n", desc = "Format buffer" },
  { "<leader>cR",  require "nvchad.lsp.renamer",  desc = "Rename var", mode = "n" },
  { "<leader>cA",  vim.lsp.buf.code_action,       desc = "LSP Action", mode = "n" },

  { "<leader>ct", ":Telescope treesitter<CR>",            desc = "Treesitter", mode = "n" },
  { "<leader>cD", ":Telescope diagnostics bufnr=0<CR>",   desc = "Diagnostic", mode = "n" },
  { "<leader>cd", ":Telescope lsp_definitions<CR>",       desc = "Definitions", mode = "n" },
  { "<leader>cr", ":Telescope lsp_references<CR>",        desc = "References", mode = "n" },
  { "<leader>ci", ":Telescope lsp_implementations<CR>",   desc = "Implementations", mode = "n" },
  { "<leader>ct", ":Telescope lsp_type_definitions<CR>",  desc = "Type definitions", mode = "n" },
  ---------------
  ---BOOKMARKS---
  ---------------
  { "mm", bm.bookmark_toggle,     desc = "Set bookmark",        mode = "n" },
  { "m]", bm.bookmark_next,       desc = "Next bookmark",       mode = "n" },
  { "m[", bm.bookmark_prev,       desc = "Prev bookmark",       mode = "n" },
  { "mc", bm.bookmark_clean,      desc = "Clean all bookmarks", mode = "n" },
  { "mC", bm.bookmark_clear_all,  desc = "Clear ALL bookmarks", mode = "n" },
  { "mf", ":Telescope bookmarks list previewer=false<CR>",  desc = "Show bookmarks", mode = "n" },
  -----------
  ---To-Do---
  -----------
   { "<C-t>t", ":TodoTelescope keywords=TODO<CR>",       desc = "List all TODOS", mode = "n" },
   { "<C-t>f", ":TodoTelescope keywords=FIX,FIXME<CR>",  desc = "List all FIXME", mode = "n" },
  --------------
  ---COMMENTS---
  --------------
   { "<C-/>", ":norm gccj<CR>",  desc = "Toggle comment", mode = "n"},
   { "<C-/>", ":norm gcc<CR>",  desc = "Toggle multiline comment", mode = "v"},
  ----------
  ---HELP---
  ----------
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
