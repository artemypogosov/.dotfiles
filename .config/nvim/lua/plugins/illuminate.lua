return {
  "RRethy/vim-illuminate",
  event = "VeryLazy",
  config = function()
    require("illuminate").configure({
      modes_allowlist = { "n" }, -- only enable in normal mode
      min_count_to_highlight = 2,
    })

    vim.api.nvim_set_hl(0, "IlluminatedWordText", { underline = true })
    vim.api.nvim_set_hl(0, "IlluminatedWordRead", { underline = true })
    vim.api.nvim_set_hl(0, "IlluminatedWordWrite", { underline = true })
  end,
}
