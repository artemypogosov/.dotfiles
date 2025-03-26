require "nvchad.mappings"
local base46 = require("base46")

local map = vim.keymap.set

-- FIND FILES
map("n", "<leader>ff", ":Telescope file_browser previewer=false path=%:p:h select_buffer=true<CR>", { desc = "Find file" })
map("n", "<leader>fF", ":Telescope git_files previewer=false<CR>", { desc = "Find file in project" })
map("n", "<leader>fd", ":Telescope find_files follow=true no_ignore=true hidden=true<CR>", { desc = "Find file in dir + inner dirs" })

map("n", "<leader>fr", ":Telescope oldfiles<CR>", { desc = "Recent files" })
map("n", "<leader>fD", ":!rm ~/.local/state/nvim/shada/main.shada<CR>", { desc = "Delete recent files list" })

-- WINDOWS
map("n", "<leader>wh", "<C-w>h", { desc = "Switch left" })
map("n", "<leader>wl", "<C-w>l", { desc = "Switch right" })
map("n", "<leader>wj", "<C-w>j", { desc = "Switch down" })
map("n", "<leader>wk", "<C-w>k", { desc = "Switch up" })
map("n", "<leader>ww", "<C-w>w", { desc = "Next window" })
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

map("n", "<leader>wiw", ":vertical resize 150<CR>", { desc = "Increase width" })
map("n", "<leader>wih", ":resize 40<CR>", { desc = "Increase height" })

-- TABS
map("n", "<leader><Tab>n", ":tabnew<CR>")
map("n", "<leader><Tab>d", ":tabclose<CR>")
map("n", "<leader><Tab>l", ":tabnext<CR>")
map("n", "<leader><Tab>h", ":tabNext<CR>")

-- BUFFERS
map("n", "<leader>bb", ":Telescope buffers<CR>",   { desc = "Switch buffer" })
map("n", "<leader>bn", ":enew<CR>", { desc = "New buffer" })
map("n", "<leader>bh", ":new<CR>",  { desc = "New horizontal buffer" })
map("n", "<leader>bv", ":vnew<CR>", { desc = "New vertical buffer" })
map("n", "<leader>bd",  ":bd<CR>",  { desc = "Kill buffer" })
map("n", "<leader>sb", ":Telescope current_buffer_fuzzy_find previewer=false <CR>", { desc = "Search buffer" })

-- TOGGLE
map("n", "<leader>tt",  ":NvimTreeToggle<CR>",  { desc = "Toggle sidebar" })
map("n", "<leader>tf",  ":NvimTreeFocus<CR>",   { desc = "Focus  sidebar" })
map("n", "<leader>tT",  base46.toggle_theme,    { desc = "Toggle theme" })

-- GIT
map("n", "<leader>gf", ":Telescope git_bcommits path=%:p:h<CR>")
map("n", "<leader>ga", ":Telescope git_commits<CR>")
map("n", "<leader>gs", ":Telescope git_status<CR>")
map("n", "<leader>gb", ":Telescope git_branches<CR>")


-- LSP
map("n", "<leader>lfb", function()
      require("conform").format { lsp_fallback = true }
end, { desc = "Format buffer" })

map("n", "<leader>lr",  require "nvchad.lsp.renamer", { desc = "Rename var" })
map("n", "<leader>lt", ":Telescope treesitter<CR>", { desc = "Treesitter" })
map("n", "<leader>la",  vim.lsp.buf.code_action, { desc = "LSP Action" })
map("n", "<leader>ld", ":Telescope diagnostics bufnr=0<CR>", { desc = "Treesitter" })

--                    | lsp_references
-- difference ???     | lsp_implementations
--                    | lsp_definitions
--                    | lsp_type_definitions


-- FIXME: fix TODOs colors in NvChad

-- To-Do
map("n", "<leader>ftt",  ":TodoTelescope keywords=TODO<CR>",      { desc = "List all TODOS" })
map("n", "<leader>ftf",  ":TodoTelescope keywords=FIX,FIXME<CR>", { desc = "List all FIXME" })
map("n", "<leader>ftpt", ":TodoTelescope cwd=~/Projects/qbp keywords=TODO<CR>",  { desc = "List all TODOS in the project" })
map("n", "<leader>ftpf", ":TodoTelescope cwd=~/Projects/qbp keywords=FIXME<CR>", { desc = "List all FIXME in the project" })

-- COMMENTS
map("n", "<C-/>", ":norm gccj<CR>", { desc = "Toggle comment"})

-- Nvdash
map("n", "<leader>gh", ":Nvdash<CR>", { desc = "Nvdash" })

-- HELP
map("n", "<leader>hs", ":Telescope spell_suggest<CR>", { desc = "Spelling suggestions" })
map("n", "<leader>hh",  ":NvCheatsheet<CR>",   { desc = "Focus  sidebar" })

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
