return {
  "natecraddock/workspaces.nvim",
  event = "VeryLazy",
  config = function()
    require("workspaces").setup({
      hooks = {
        open = { "Telescope find_files previewer=false follow=true no_ignore=true hidden=true" },
      },
    })
  end,
}
