return {
  {
    "nvim-telescope/telescope-file-browser.nvim",
    lazy = true,
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
  },

  {
    'nvim-telescope/telescope-fzf-native.nvim',
    lazy = true,
    event = "BufRead",
    build = 'make',
    dependencies = { "nvim-telescope/telescope.nvim" },
    config = function()
      require("telescope").load_extension("fzf")
    end,
  }
}
