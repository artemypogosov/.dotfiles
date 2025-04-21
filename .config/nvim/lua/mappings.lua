require "nvchad.mappings"

local base46 = require("base46")
local bm = require "bookmarks"
local map = vim.keymap.set

-- FIND FILES
map("n", "<leader>ff", ":Telescope file_browser previewer=false<CR>", { desc = "Find file" })
map("n", "<leader>.", ":Telescope file_browser previewer=false<CR>", { desc = "Find file" })
map("n", "<leader>fF", ":Telescope git_files<CR>", { desc = "Find git files in project" })
map("n", "<leader>fd", ":Telescope find_files previewer=false<CR>", { desc = "Find file in dir" })
map("n", "<leader>fr", ":Telescope oldfiles previewer=false <CR>",      { desc = "Recent files" })

map("n", "<leader>fD", function()
  local answer = vim.fn.input("Delete recent files list? (yes/no): ")
  if answer:lower() == "yes" then
    vim.cmd("!rm ~/.local/state/nvim/shada/main.shada")
  end
end, { desc = "Delete recent files list" })

-- PROJECT MANAGEMENT (WorkspacesAdd / WorkspacesAddDir)
map("n", "<leader>pp", ":Telescope workspaces<CR>", { desc = "Find project" })
map("n", "<leader>pa", ":WorkspacesAdd ", { desc = "Add project" })
map("n", "<leader>pA", ":WorkspacesAddDir ", { desc = "Add projects dir" })
map("n", "<leader>pr", ":WorkspacesRename   ", { desc = "Rename project" })
map("n", "<leader>pl", ":WorkspacesList<CR>", { desc = "List workspaces" })
map("n", "<leader>pL", ":WorkspacesListDirs<CR>", { desc = "List projects' dirs" })
map("n", "<leader>pd", ":WorkspacesRemove  ", { desc = "Remove project" })
map("n", "<leader>pD", ":WorkspacesRemoveDir  ", { desc = "Remove dir" })

-- TOGGLE
map("n", "<leader>th",  ":Nvdash<CR>", { desc = "Nvdash" })
map("n", "<leader>tt",  ":NvimTreeToggle<CR>",  { desc = "Toggle sidebar" })
map("n", "<leader>tf",  ":NvimTreeFocus<CR>",   { desc = "Focus  sidebar" })
map("n", "<leader>tz",  ":ZenMode<CR>", { desc = "Zen mode" })

-- WINDOWS
map("n", "<leader>ww", "<C-w>w", { desc = "Next window" })

map("n", "<leader>wh", "<C-w>h", { desc = "Switch left" })
map("n", "<leader>wl", "<C-w>l", { desc = "Switch right" })
map("n", "<leader>wj", "<C-w>j", { desc = "Switch down" })
map("n", "<leader>wk", "<C-w>k", { desc = "Switch up" })

map("n", "<leader>ws", "<C-w>s", { desc = "Split H" })
map("n", "<leader>wv", "<C-w>v", { desc = "Split V" })
map("n", "<leader>w=", "<C-w>=", { desc = "=" })

map("n", "<leader>wH", "<C-w>H", { desc = "Move left" })
map("n", "<leader>wL", "<C-w>L", { desc = "Move right" })
map("n", "<leader>wJ", "<C-w>J", { desc = "Move down" })
map("n", "<leader>wK", "<C-w>K", { desc = "Move up" })
map("n", "<leader>wx", "<C-w>x", { desc = "Swap with next" })

map("n", "<leader>wc", "<C-w>c", { desc = "Close" })
map("n", "<leader>wC", "<C-w>o", { desc = "Close other" })

map("n", "<leader>wm",  ":MaximizerToggle<CR>",     { desc = "Maximize window" })
map("n", "<leader>wih", ":resize 40<CR>",           { desc = "Increase height" })
map("n", "<leader>wiw", ":vertical resize 150<CR>", { desc = "Increase width" })

-- WORKSPACES
map("n", "<leader><Tab>N", ":tabnew<CR>",   { desc = "New workspace"  })
map("n", "<leader><Tab>D", ":tabclose<CR>", { desc = "Kill workspace" })
map("n", "<leader><Tab>]", ":tabnext<CR>",  { desc = "Next workspace" })
map("n", "<leader><Tab>[", ":tabNext<CR>",  { desc = "Prev workspace" })

-- BUFFERS
map("n", "<leader>bb", ":Telescope buffers previewer=false <CR>", { desc = "Switch buffer" })
map("n", "<leader>,", ":Telescope buffers previewer=false <CR>", { desc = "Switch buffer" })

map("n", "<leader>bN", ":enew<CR>", { desc = "New buffer" })
map("n", "<leader>bH", ":new<CR>",  { desc = "New horizontal buffer" })
map("n", "<leader>bV", ":vnew<CR>", { desc = "New vertical buffer" })
map("n", "<leader>b]", ":bnext<CR>", { desc = "Next buffer" }) -- tab
map("n", "<leader>b[", ":bprev<CR>", { desc = "Prev buffer" }) -- shift + tab
map("n", "<leader>bk", ":bd<CR>", { desc = "Kill buffer" })

map("n", "<leader>bK",
    function()
      local answer = vim.fn.input("Delete all buffers? (y/n): ")
      if answer:lower() == "y" then
        vim.cmd("%bd")
      end
    end, { desc = "Kill all buffers" })

map("n", "<leader>bD", ":%bd|e#<CR>", { desc = "Kill all buffers except current" })


-- SEARCH
map("n", "<leader>sb", ":Telescope current_buffer_fuzzy_find previewer=false <CR>", { desc = "Search in buffer" })

-- GIT
map("n", "<leader>gs", ":Telescope git_status<CR>",              { desc = "Git status" })
map("n", "<leader>gb", ":Telescope git_branches<CR>",            { desc = "Git branches" })
map("n", "<leader>gc", ":Telescope git_bcommits path=%:p:h<CR>", { desc = "File's commits" })
map("n", "<leader>gC", ":Telescope git_commits<CR>",             { desc = "Branch commits" })
map("n", "<leader>ga", ":BlameToggle<CR>", { desc = "Git annotations" })

-- LSP
map("n", "<leader>lf", function() require("conform").format { lsp_fallback = true } end, { desc = "Format buffer" })

map("n", "<leader>lR",  require "nvchad.lsp.renamer", { desc = "Rename var" })
map("n", "<leader>lA",  vim.lsp.buf.code_action,      { desc = "LSP Action" })

map("n", "<leader>lt", ":Telescope treesitter<CR>",           { desc = "Treesitter" })
map("n", "<leader>lD", ":Telescope diagnostics bufnr=0<CR>",  { desc = "Diagnostic" })
map("n", "<leader>ld", ":Telescope lsp_definitions<CR>",      { desc = "Definitions" })
map("n", "<leader>lr", ":Telescope lsp_references<CR>",       { desc = "References" })
map("n", "<leader>li", ":Telescope lsp_implementations<CR>",  { desc = "Implementations" })
map("n", "<leader>lt", ":Telescope lsp_type_definitions<CR>", { desc = "Type definitions" })

-- Bookmarks
map("n", "mm", bm.bookmark_toggle,    { desc = "Set bookmark" })
map("n", "m]", bm.bookmark_next,      { desc = "Next bookmark" })
map("n", "m[", bm.bookmark_prev,      { desc = "Prev bookmark" })
map("n", "mc", bm.bookmark_clean,     { desc = "Clean all bookmarks" })
map("n", "mC", bm.bookmark_clear_all, { desc = "Clear ALL bookmarks" })
map("n", "mf", ":Telescope bookmarks list previewer=false<CR>", { desc = "Show bookmarks" })

-- To-Do
map("n", "<C-t>t", ":TodoTelescope keywords=TODO<CR>",      { desc = "List all TODOS" })
map("n", "<C-t>f", ":TodoTelescope keywords=FIX,FIXME<CR>", { desc = "List all FIXME" })

-- COMMENTS
map("n", "<C-/>", ":norm gccj<CR>", { desc = "Toggle comment"})
map("v", "<C-/>", ":norm gcc<CR>", { desc = "Toggle multiline comment"})

-- HELP
map("n", "<leader>hs", ":Telescope spell_suggest<CR>", { desc = "Spelling suggestions" })
map("n", "<leader>ht",  base46.toggle_theme,    { desc = "Toggle theme" })
map("n", "<leader>hs",  ":set spell<CR>",    { desc = "Enable spellcheck" })
map("n", "<leader>hS",  ":set nospell<CR>",  { desc = "Disable spellcheck" })

-- ctrl+wd (lsp diagnostic on the cursor) --> not able to bind it to map()

-- KEYMAP REMOVAL
vim.keymap.del("n", "<leader>b")
vim.keymap.del("n", "<leader>x")
vim.keymap.del("n", "<leader>n")
vim.keymap.del("n", "<leader>/")
vim.keymap.del("n", "<leader>h")
vim.keymap.del("n", "<leader>v")
vim.keymap.del("n", "<C-n>")
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
