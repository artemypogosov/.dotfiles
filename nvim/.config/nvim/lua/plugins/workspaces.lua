return {
  -- A simple plugin to manage workspace directories in neovim
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
