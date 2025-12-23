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
-- TODO: remove? I have no plans to use nvimtree (use Snacks.explorer instead)
local prev = { new_name = "", old_name = "" } -- Prevents duplicate events
vim.api.nvim_create_autocmd("User", {
  pattern = "NvimTreeSetup",
  callback = function()
    local events = require("nvim-tree.api").events
    events.subscribe(events.Event.NodeRenamed, function(data)
      if prev.new_name ~= data.new_name or prev.old_name ~= data.old_name then
        data = data
        Snacks.rename.on_rename_file(data.old_name, data.new_name)
      end
    end)
  end,
})

-- Open QuickFix buffer after :grep
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
  pattern = "grep",
  callback = function()
    -- Open (or update) quickfix window
    vim.cmd("cwindow")

    -- Defer until UI is stable
    vim.schedule(function()
      local qf = vim.fn.getqflist({ winid = 0 })
      if qf.winid and qf.winid ~= 0 then
        vim.api.nvim_set_current_win(qf.winid)
      end
    end)
  end,
})
