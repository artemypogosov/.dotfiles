return {
  "folke/trouble.nvim",
  cmd = "Trouble",
  opts = {
    focus = true,
    -- Define default behaviors for specific modes
    modes = {
      quickfix = {
        win = {
          position = "bottom",
          size = 25,
        },
      },
    },
  },
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
      desc = "Todo List",
    },
    {
      "<leader>xq",
      "<cmd>Trouble quickfix toggle<cr>",
      desc = "Quickfix List (Trouble)",
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
