-- Save file on focus lost
vim.api.nvim_create_autocmd("FocusLost", {
  pattern = "*",
  command = "silent! update", -- Saves only if there are changes
})

-- Use Alt+n to switch workspaces
for i = 1, 9 do
  vim.keymap.set("n", string.format("<A-%s>", i), function()
    local tab_count = vim.fn.tabpagenr("$")
    if i <= tab_count then
      vim.cmd(i .. "tabnext")
    else
      vim.notify(
        string.format("Tab %d does not exist (only %d open)", i, tab_count),
        vim.log.levels.WARN,
        { title = "Tabs" }
      )
    end
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

-- LSP-integrated file renaming
-- Oil
vim.api.nvim_create_autocmd("User", {
  pattern = "OilActionsPost",
  callback = function(event)
    -- Check if Snacks is actually loaded
    if not _G.Snacks then
      return
    end

    for _, action in ipairs(event.data.actions) do
      if action.type == "move" then
        Snacks.rename.on_rename_file(action.src_url, action.dest_url)
      end
    end
  end,
})

-- Open QuickFix buffer after :grep
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
  pattern = "grep",
  callback = function()
    -- Only open Trouble if there are actually items in the quickfix list
    if #vim.fn.getqflist() > 0 then
      -- Close the standard quickfix if it accidentally opened
      vim.cmd("cclose")
      -- Open Trouble's quickfix view
      vim.cmd("Trouble quickfix open")
    end
  end,
})

-- Disable Copilot by default at startup
vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    vim.cmd("Copilot disable")
  end,
})

-- LSP Progress
vim.api.nvim_create_autocmd("LspProgress", {
  ---@param ev {data: {client_id: integer, params: lsp.ProgressParams}}
  callback = function(ev)
    local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
    vim.notify(vim.lsp.status(), "info", {
      id = "lsp_progress",
      title = "LSP Progress",
      opts = function(notif)
        notif.icon = ev.data.params.value.kind == "end" and " "
          or spinner[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner + 1]
      end,
    })
  end,
})
