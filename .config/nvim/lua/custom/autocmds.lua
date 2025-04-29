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

vim.api.nvim_create_autocmd("FocusLost", {
    pattern = "*",
    command = "silent! update",  -- Saves only if there are changes
})

-- Open Nvchad after the last buffer was closed
vim.api.nvim_create_autocmd("BufDelete", {
  group = vim.api.nvim_create_augroup("bufdelpost_autocmd", {}),
  desc = "BufDeletePost User autocmd",
  callback = function()
    vim.schedule(function()
      vim.api.nvim_exec_autocmds("User", {
        pattern = "BufDeletePost",
      })
    end)
  end,
})

vim.api.nvim_create_autocmd("User", {
  pattern = "BufDeletePost",
  group = vim.api.nvim_create_augroup("dashboard_delete_buffers", {}),
  desc = "Open Dashboard when no available buffers",
  callback = function(ev)
    local deleted_name = vim.api.nvim_buf_get_name(ev.buf)
    local deleted_ft = vim.api.nvim_get_option_value("filetype", { buf = ev.buf })
    local deleted_bt = vim.api.nvim_get_option_value("buftype", { buf = ev.buf })
    local dashboard_on_empty = deleted_name == "" and deleted_ft == "" and deleted_bt == ""

    if dashboard_on_empty then
      vim.cmd("cd ~")
      vim.cmd("silent! Nvdash")
    end
  end,
})

-- Use Ctrl+n to switch buffers (tabs)
 for i = 1, 9, 1 do
   vim.keymap.set("n", string.format("<C-%s>", i), function()
     vim.api.nvim_set_current_buf(vim.t.bufs[i])
   end)
 end

-- File: ~/.config/nvim/lua/custom/plugins/lspconfig.lua

return {
  config = function()
    local on_attach = function(client, bufnr)
      -- delete NvChad's mapping
      pcall(vim.keymap.del, "n", "<leader>wl", { buffer = bufnr })

      -- set your override
      vim.keymap.set("n", "<leader>wl", "<C-w>l", {
        buffer = bufnr,
        desc = "Switch right",
        silent = true,
      })
    end

    -- load NvChad default config and inject on_attach
    require("nvchad.configs.lspconfig").defaults.on_attach = on_attach
  end,
}
