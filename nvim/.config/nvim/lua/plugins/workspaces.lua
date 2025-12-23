return {
  "natecraddock/workspaces.nvim",
  event = "VeryLazy",
  config = function()
    require("workspaces").setup({
      hooks = {
        open = { "lua Snacks.picker.files()" },
      },
    })
  end,
}
