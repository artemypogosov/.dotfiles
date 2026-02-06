return {
  -- A pretty diagnostics, references, telescope results,
  -- quickfix and location list to help you solve all the trouble your code is causing.
  "folke/trouble.nvim",
  cmd = "Trouble",
  opts = {
    focus = true,
    -- Define default behaviors for specific modes
    modes = {
      quickfix = {
        win = {
          position = "bottom",
          size = 20,
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
      "<leader>xs",
      "<cmd>Trouble lsp_document_symbols toggle win.size=15<cr>",
      desc = "LSP: Document symbols",
    },
    {
      "<leader>xr",
      "<cmd>Trouble lsp_references toggle win.size=15<cr>",
      desc = "LSP: References",
    },
    {
      "<leader>xt",
      "<cmd>Trouble todo toggle filter = {tag = {TODO,FIX,FIXME,WARN,WARNING}} win.size=15<cr>",
      desc = "Todo List",
    },
  },
}
