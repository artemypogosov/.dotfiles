local session = require("mini.sessions")

return {
  -- snacks.nvim --> A collection of QoL (Quality of Life) plugins for Neovim
  -- Beautiful declarative dashboards
  "folke/snacks.nvim",
  opts = {
    gitbrowse = { enabled = true },
    indent = { enabled = true },
    terminal = { enabled = true },
    dashboard = {
      -- your dashboard configuration comes here
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
          { key = "r", desc = "Recent files", action = ":Telescope oldfiles previewer=false" },
          {
            key = "p",
            desc = "Open project",
            action = ":lua require('telescope').extensions.workspaces.workspaces({ layout_config = { width = 80, height = 15, prompt_position = 'top' } })",
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
