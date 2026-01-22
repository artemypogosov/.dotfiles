return {
  "stevearc/oil.nvim",
  ---@module 'oil'
  ---@type oil.SetupOpts
  opts = {
    watch_for_changes = true,
    columns = {
      "icon",
      "permissions",
      "size",
      "mtime",
    },
    view_options = {
      show_hidden = true,
    },
    keymaps = {
      ["<C-l>"] = "actions.select",
      ["<C-h>"] = { "actions.parent", mode = "n" },
      ["q"] = { "actions.close", mode = "n" },
      ["<C-p>"] = "actions.preview",
      ["H"] = { "actions.toggle_hidden", mode = "n" },
      ["C-o"] = "actions.open_external", -- Open file in external program (like Emacs)
      ["C-s"] = { "actions.change_sort", mode = "n" },
      ["g?"] = { "actions.show_help", mode = "n" },
    },
  },
  -- Optional dependencies
  dependencies = { { "nvim-mini/mini.icons", opts = {} } },
  -- Lazy loading is not recommended because it is very tricky to make it work correctly in all situations.
  lazy = false,
}
