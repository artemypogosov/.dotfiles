return {
  "RRethy/vim-illuminate",
  event = "VeryLazy",
  config = function()
    require("illuminate").configure({
      modes_allowlist = { "n" }, -- only enable in normal mode
      min_count_to_highlight = 2,
    })
  end,
}
