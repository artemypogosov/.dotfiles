return {
  "FabijanZulj/blame.nvim",
  event = "VeryLazy",
  config = function()
    require("blame").setup({
      focus_blame = false,
      merge_consecutive = true,
    })
  end,
}
