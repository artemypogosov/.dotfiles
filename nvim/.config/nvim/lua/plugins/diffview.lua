return {
  -- Single tabpage interface for easily cycling through diffs for all modified files for any git rev.
  "sindrets/diffview.nvim",
  config = function()
    local actions = require("diffview.actions")
    require("diffview").setup({
      enhanced_diff_hl = true,
      view = {
        merge_tool = {
          layout = "diff4_mixed",
        },
      },
      file_panel = {
        listing_style = "tree",
        win_config = {
          position = "left",
          width = 35,
        },
      },
      keymaps = {
        disable_defaults = true,
        view = {
          { "n", "<tab>", actions.select_next_entry, { desc = "Open the diff for the next file" } },
          { "n", "<s-tab>", actions.select_prev_entry, { desc = "Open the diff for the previous file" } },
          { "n", "<leader>dp", actions.toggle_files, { desc = "Toggle the file panel." } },
          { "n", "<leader>df", actions.focus_files, { desc = "Bring focus to the file panel" } },
          { "n", "[x", actions.prev_conflict, { desc = "In the merge-tool: jump to the previous conflict" } },
          { "n", "]x", actions.next_conflict, { desc = "In the merge-tool: jump to the next conflict" } },
          { "n", "<leader>do", actions.conflict_choose("ours"), { desc = "Choose OURS" } },
          { "n", "<leader>dO", actions.conflict_choose_all("ours"), { desc = "Choose OURS [the whole file]" } },
          { "n", "<leader>dt", actions.conflict_choose("theirs"), { desc = "Choose THEIRS" } },
          { "n", "<leader>dT", actions.conflict_choose_all("theirs"), { desc = "Choose THEIRS [the whole file]" } },
          { "n", "<leader>db", actions.conflict_choose("base"), { desc = "Choose BASE" } },
          { "n", "<leader>dB", actions.conflict_choose_all("base"), { desc = "Choose BASE [the whole file]" } },
          { "n", "<leader>da", actions.conflict_choose("all"), { desc = "Choose all" } },
          { "n", "<leader>dA", actions.conflict_choose_all("all"), { desc = "Choose all [the whole file]" } },
          { "n", "<leader>dx", actions.conflict_choose("none"), { desc = "Delete the conflict" } },
          { "n", "<leader>dX", actions.conflict_choose_all("none"), { desc = "Delete conflicts [the whole file]" } },
        },
        diff1 = {
          { "n", "g?", actions.help({ "view", "diff1" }), { desc = "Open the help panel" } },
        },
        diff2 = {
          { "n", "g?", actions.help({ "view", "diff2" }), { desc = "Open the help panel" } },
        },
        diff3 = {
          { "n", "g?", actions.help({ "view", "diff3" }), { desc = "Open the help panel" } },
        },
        diff4 = {
          { "n", "g?", actions.help({ "view", "diff4" }), { desc = "Open the help panel" } },
        },
        file_panel = {
          { "n", "<tab>", actions.select_next_entry, { desc = "Open the diff for the next file" } },
          { "n", "<s-tab>", actions.select_prev_entry, { desc = "Open the diff for the previous file" } },
          { "n", "<cr>", actions.select_entry, { desc = "Open the diff for the selected entry" } },
          { "n", "j", actions.next_entry, { desc = "Bring the cursor to the next file entry" } },
          { "n", "k", actions.prev_entry, { desc = "Bring the cursor to the previous file entry" } },
          { "n", "o", actions.select_entry, { desc = "Open the diff for the selected entry" } },
          { "n", "l", actions.select_entry, { desc = "Open the diff for the selected entry" } },
          { "n", "L", actions.open_commit_log, { desc = "Open the commit log panel" } },
          { "n", "<leader>dp", actions.toggle_files, { desc = "Toggle the file panel" } },
          { "n", "<leader>df", actions.focus_files, { desc = "Bring focus to the file panel" } },
          { "n", "<2-LeftMouse>", actions.select_entry, { desc = "Open the diff for the selected entry" } },
          { "n", "zo", actions.open_fold, { desc = "Expand fold" } },
          { "n", "h", actions.close_fold, { desc = "Collapse fold" } },
          { "n", "zc", actions.close_fold, { desc = "Collapse fold" } },
          { "n", "za", actions.toggle_fold, { desc = "Toggle fold" } },
          { "n", "zR", actions.open_all_folds, { desc = "Expand all folds" } },
          { "n", "zM", actions.close_all_folds, { desc = "Collapse all folds" } },
        },
        file_history_panel = {
          { "n", "g!", actions.options, { desc = "Open the option panel" } },
          { "n", "y", actions.copy_hash, { desc = "Copy the commit hash of the entry under the cursor" } },
          { "n", "L", actions.open_commit_log, { desc = "Show commit details" } },
          { "n", "X", actions.restore_entry, { desc = "Restore file to the state from the selected entry" } },
          { "n", "zo", actions.open_fold, { desc = "Expand fold" } },
          { "n", "zc", actions.close_fold, { desc = "Collapse fold" } },
          { "n", "h", actions.close_fold, { desc = "Collapse fold" } },
          { "n", "za", actions.toggle_fold, { desc = "Toggle fold" } },
          { "n", "zR", actions.open_all_folds, { desc = "Expand all folds" } },
          { "n", "zM", actions.close_all_folds, { desc = "Collapse all folds" } },
          { "n", "j", actions.next_entry, { desc = "Bring the cursor to the next file entry" } },
          { "n", "k", actions.prev_entry, { desc = "Bring the cursor to the previous file entry" } },
          { "n", "<cr>", actions.select_entry, { desc = "Open the diff for the selected entry" } },
          { "n", "o", actions.select_entry, { desc = "Open the diff for the selected entry" } },
          { "n", "l", actions.select_entry, { desc = "Open the diff for the selected entry" } },
          { "n", "<2-LeftMouse>", actions.select_entry, { desc = "Open the diff for the selected entry" } },
          { "n", "<c-b>", actions.scroll_view(-0.25), { desc = "Scroll the view up" } },
          { "n", "<c-f>", actions.scroll_view(0.25), { desc = "Scroll the view down" } },
          { "n", "<tab>", actions.select_next_entry, { desc = "Open the diff for the next file" } },
          { "n", "<s-tab>", actions.select_prev_entry, { desc = "Open the diff for the previous file" } },
          { "n", "gf", actions.goto_file_edit, { desc = "Open the file in the previous tabpage" } },
          { "n", "<leader>dp", actions.toggle_files, { desc = "Toggle the file panel" } },
          { "n", "<leader>df", actions.focus_files, { desc = "Bring focus to the file panel" } },
          { "n", "g?", actions.help("file_history_panel"), { desc = "Open the help panel" } },
        },
        option_panel = {
          { "n", "<tab>", actions.select_entry, { desc = "Change the current option" } },
          { "n", "q", actions.close, { desc = "Close the panel" } },
          { "n", "g?", actions.help("option_panel"), { desc = "Open the help panel" } },
        },
        help_panel = {
          { "n", "q", actions.close, { desc = "Close help menu" } },
          { "n", "<esc>", actions.close, { desc = "Close help menu" } },
        },
      },
    })
  end,
}
