return {
  "natecraddock/workspaces.nvim",
  event = "VeryLazy",
  config = function()
    require("workspaces").setup({
      hooks = {
        open = function()
          -- 1. Get the path of the workspace we just opened
          local path = require("workspaces").path()
          if path then
            -- 2. Change the directory ONLY for the current tab
            vim.cmd("tcd " .. path)
            -- 3. Open the file picker in that specific directory
            require("snacks").picker.files()
          end
        end,
      },
    })
  end,
}
