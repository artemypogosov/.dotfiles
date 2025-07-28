return {
  "folke/snacks.nvim",
  opts = {
    dashboard = {
      -- your dashboard configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
      width = 40,
      -- pane_gap = 2
      preset = {
        keys = {
          { key = "r", desc = "Recent Files", action = ":Telescope oldfiles previewer=false" },
          { key = "p", desc = "Open Project", action = ":WorkspacesOpen" },
          { key = "l", desc = "Lazy", action = ":Lazy" },
          {
            key = "c",
            desc = "Config",
            action = ":lua Snacks.dashboard.pick('files', {cwd = vim.fn.stdpath('config')})",
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
