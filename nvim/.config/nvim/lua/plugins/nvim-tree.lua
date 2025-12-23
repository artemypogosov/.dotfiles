return {
  "nvim-tree/nvim-tree.lua",
  version = "*",
  lazy = false,
  cmd = { "NvimTreeToggle", "NvimTreeOpen" },
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    require("nvim-tree").setup({
      update_focused_file = {
        enable = true,
        update_root = true,
      },
      respect_buf_cwd = false,
      view = {
        width = 35,
      },
    })
  end,
}
