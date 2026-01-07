local session = require("mini.sessions")

return {
  -- snacks.nvim --> A collection of QoL (Quality of Life) plugins for Neovim
  -- Beautiful declarative dashboards
  "folke/snacks.nvim",
  opts = {
    gitbrowse = { enabled = true },
    indent = { enabled = true },
    terminal = { enabled = true },
    explorer = { enabled = true },

    picker = {
      sources = {
        explorer = { hidden = true },
        diagnostics = { hidden = true },
        lines = { layout = { preset = "select", layout = { height = 0.5 } } },
        recent = { layout = { preset = "select", layout = { height = 0.25 } } },
        files = {
          layout = { preset = "select", layout = { height = 0.25 } },
          hidden = true,
          args = {
            "--type",
            "f",
            "--strip-cwd-prefix",
            "--exclude",
            ".git",
            "--exclude",
            "node_modules",
            "--exclude",
            ".cache",
            "--exclude",
            ".local",
          },
          matcher = {
            fuzzy = false,
          },
        },
        git_log_file = {
          current_file = true,
        },
        git_log = {
          -- current_file = true,
        },
        spelling = {},
        buffers = {
          current = false,
          sort_lastused = true,
          unloaded = false,
          layout = { preset = "select", layout = { height = 0.25 } },
        },
        man = {
          matcher = {
            fuzzy = false,
          },
        },
      },
    },

    dashboard = {
      width = 40,
      preset = {
        keys = {
          {
            key = "l",
            desc = "Load session",
            action = function()
              session.select("read")
            end,
          },
          { key = "r", desc = "Recent files", action = ":lua Snacks.picker.recent()" },
          {
            key = "p",
            desc = "Open project",
            action = ":lua require('custom.snacks.project_picker').open()",
          },
        },
        header = [[
███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗
████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║
██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║
██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║
██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║
╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝

To see with eyes unclouded by hate.⠀]],
      },
      sections = {
        { section = "header" },
        { section = "keys", gap = 1, padding = 2 },
        { section = "startup", icon = "" },
      },
    },
  },
}
