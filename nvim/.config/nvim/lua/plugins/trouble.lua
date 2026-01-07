return {
  "folke/trouble.nvim",
  opts = {
    focus = true,
  },
  cmd = "Trouble",
  keys = {
    {
      "<leader>xx",
      "<cmd>Trouble diagnostics toggle filter.buf=0 win.size=15<cr>",
      desc = "Buffer Diagnostics",
    },
    {
      "<leader>xX",
      "<cmd>Trouble diagnostics toggle win.size=15<cr>",
      desc = "Diagnostics",
    },
    {
      "<leader>xt",
      "<cmd>Trouble todo filter = {tag = {TODO,FIX,FIXME,WARN,WARNING}} win.size=15<cr>",
      desc = "Defs/Refs/Impl...",
    },
    {
      "<leader>cs",
      "<cmd>Trouble symbols toggle focus=true win.size=55<cr>",
      desc = "Symbols",
    },
    {
      "<leader>cl",
      "<cmd>Trouble lsp toggle win.position=right win.size=55<cr>",
      desc = "LSP: Defs/Refs/Impl...",
    },
  },
}
