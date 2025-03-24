require "nvchad.mappings"

local map = vim.keymap.set

map("n", "<leader>ff", ":Telescope file_browser path=%:p:h select_buffer=true<CR>")
map("n", "<leader><Tab>n", ":tabnew<CR>")
map("n", "<leader><Tab>d", ":tabclose<CR>")
map("n", "<leader><Tab>l", ":tabnext<CR>")
map("n", "<leader><Tab>h", ":tabNext<CR>")
