-- Change a directory when opening a file
vim.api.nvim_create_autocmd("BufEnter", {
  callback = function()
    vim.cmd("silent! cd %:p:h")
  end,
})

-- Save file on focus lost
vim.api.nvim_create_autocmd("FocusLost", {
  pattern = "*",
  command = "silent! update", -- Saves only if there are changes
})

-- Use Ctrl+n to switch buffers (tabs)
for i = 1, 9 do
  vim.keymap.set("n", string.format("<A-%s>", i), function()
    vim.cmd(i .. "tabnext")
  end, { desc = "Go to tab " .. i })
end

-- Highlight text for some time after yanking
-- 1. Define a visible highlight group
vim.api.nvim_set_hl(0, "YankHighlight", { bg = "#7c6f64", fg = "#282828" })

-- 2. Use it in the on_yank autocmd
vim.api.nvim_create_autocmd("TextYankPost", {
  group = vim.api.nvim_create_augroup("YankHighlightGroup", { clear = true }),
  pattern = "*",
  callback = function()
    vim.highlight.on_yank({ higroup = "YankHighlight", timeout = 200 })
  end,
  desc = "Highlight yanked text",
})

-- Open dashboard when no buffers are left
vim.api.nvim_create_autocmd("BufDelete", {
  group = vim.api.nvim_create_augroup("dashboard_on_empty", { clear = true }),
  desc = "Open Snacks dashboard when all real buffers are closed",
  callback = function()
    -- Get list of real buffers
    local real_bufs = vim.tbl_filter(function(buf)
      return vim.api.nvim_buf_is_valid(buf)
        and vim.api.nvim_buf_is_loaded(buf)
        and vim.bo[buf].buflisted
        and vim.bo[buf].buftype == ""
    end, vim.api.nvim_list_bufs())

    -- If only one buffer left and it's [No Name]
    if #real_bufs == 1 and vim.api.nvim_buf_get_name(real_bufs[1]) == "" then
      vim.schedule(function()
        vim.cmd("cd ~")
        require("snacks.dashboard").open()
      end)
    end
  end,
})
