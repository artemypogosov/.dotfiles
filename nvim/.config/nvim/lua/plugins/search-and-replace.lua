return {
  "roobert/search-replace.nvim",
  config = function()
    require("search-replace").setup({
      -- optionally override defaults
      default_replace_single_buffer_options = "g",
      default_replace_multi_buffer_options = "gc",
    })
  end,
}
