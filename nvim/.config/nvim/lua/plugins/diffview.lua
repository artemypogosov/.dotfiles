return {
  "sindrets/diffview.nvim",
  config = function()
    local actions = require("diffview.actions")
    require("diffview").setup({
      view = {
        merge_tool = {
          layout = "diff4_mixed",
        },
      },
      keymaps = {
        disable_defaults = true,
        view = {
          {
            "n",
            "<leader>do",
            actions.conflict_choose("ours"),
            { desc = "Choose OURS" },
          },
          {
            "n",
            "<leader>dt",
            actions.conflict_choose("theirs"),
            { desc = "Choose THEIRS" },
          },
          {
            "n",
            "<leader>db",
            actions.conflict_choose("base"),
            { desc = "Choose BASE" },
          },
          {
            "n",
            "<leader>da",
            actions.conflict_choose("all"),
            { desc = "Choose all" },
          },
          { "n", "<leader>dx", actions.conflict_choose("none"), { desc = "Delete the conflict" } },
          {
            "n",
            "<leader>dO",
            actions.conflict_choose_all("ours"),
            { desc = "Choose OURS [the whole file]" },
          },
          {
            "n",
            "<leader>dT",
            actions.conflict_choose_all("theirs"),
            { desc = "Choose THEIRS [the whole file]" },
          },
          {
            "n",
            "<leader>dB",
            actions.conflict_choose_all("base"),
            { desc = "Choose BASE [the whole file]" },
          },
          {
            "n",
            "<leader>dA",
            actions.conflict_choose_all("all"),
            { desc = "Choose all the versions of a conflict for the whole file" },
          },
          {
            "n",
            "<leader>dX",
            actions.conflict_choose_all("none"),
            { desc = "Delete conflicts [the whole file]" },
          },
        },
      },
    })
  end,
}
