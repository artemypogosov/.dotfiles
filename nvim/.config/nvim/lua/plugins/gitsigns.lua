return {
  -- Git integration for buffers
  "lewis6991/gitsigns.nvim",
  config = function()
    require("gitsigns").setup()
    vim.api.nvim_create_user_command("GitsignsBlameToggle", function()
      -- Get current state
      local cur_win = vim.api.nvim_get_current_win()

      -- Check if this specific buffer has a 'blame_window' associated with it
      -- OR if the current window IS the blame window itself
      local tracked_blame_win = vim.t.gitsigns_blame_win
      local source_win = vim.t.gitsigns_source_win

      -- TOGGLE OFF: If blame window is open and valid, close it
      if tracked_blame_win and vim.api.nvim_win_is_valid(tracked_blame_win) then
        vim.api.nvim_win_close(tracked_blame_win, true)

        -- Return focus to the source window if we know where it is
        if source_win and vim.api.nvim_win_is_valid(source_win) then
          vim.api.nvim_set_current_win(source_win)
        end

        -- Clear the tracking
        vim.t.gitsigns_blame_win = nil
        vim.t.gitsigns_source_win = nil
        return
      end

      -- GIT CHECK: Only when trying to OPEN
      if not vim.b.gitsigns_status_dict then
        return
      end

      -- TOGGLE ON: Open Blame
      local original_win = cur_win
      vim.cmd("Gitsigns blame")

      -- Gitsigns opens a new window; we wait a tiny bit to capture its ID
      vim.defer_fn(function()
        local new_win = vim.api.nvim_get_current_win()
        if new_win ~= original_win then
          -- Store the IDs in the Tab scope (vim.t) so they persist
          -- and are accessible from any window in this tab
          vim.t.gitsigns_blame_win = new_win
          vim.t.gitsigns_source_win = original_win
        end
      end, 50) -- 50ms delay to ensure Gitsigns has finished the split
    end, {})
  end,
}
