return {
  -- Neovim plugin for GitHub Copilot
  -- JFYI: There is also "zbirenbaum/copilot.lua" which is written in Lua.
  "github/copilot.vim",
  config = function()
    -- In copilot.vim, you set a global dictionary
    vim.g.copilot_filetypes = {
      ["grug-far"] = false,
      ["grug-far-history"] = false,
      ["grug-far-help"] = false,
    }
  end,
}
