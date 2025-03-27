-- Change a directory when opening a file
vim.api.nvim_create_autocmd("BufEnter", {
  callback = function()
    vim.cmd "silent! cd %:p:h"
  end,
})

vim.api.nvim_create_autocmd("DiagnosticChanged", {
  callback = function()
    local diagnostics = vim.diagnostic.get(0) -- Get diagnostics for the current buffer
    if #diagnostics == 0 then
      vim.schedule(function()
        vim.cmd("lclose") -- Close location list safely
      end)
    else
      vim.diagnostic.setloclist({ open = false }) -- Update loclist but don't reopen it
    end
  end,
})
