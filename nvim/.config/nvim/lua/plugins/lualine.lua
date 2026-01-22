local noice = require("noice")

return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-mini/mini.icons" },
  opts = {
    options = {
      -- theme = "gruvbox",
      component_separators = { left = "⫶", right = "⫶" },
      section_separators = { left = "", right = "" },
      -- component_separators = { left = '', right = '' },
      -- component_separators = { left = '│', right = '│' },
      -- section_separators = { left = '', right = '' },

      disabled_filetypes = {
        statusline = { "snacks_dashboard", "NvimTree" },
      },
    },
    sections = {
      lualine_a = {},
      lualine_b = {
        {
          "filename",
          path = 4,
          -- 0: Just the filename
          -- 1: Relative path
          -- 2: Absolute path
          -- 3: Absolute path, with tilde as the home directory
          -- 4: Filename and parent dir, with tilde as the home directory
        },
        {
          "tabs",
          mode = 1,
          tabs_color = {
            -- Same values as the general color option can be used here.
            active = "lualine_b_normal", -- Color for active tab.
            -- inactive = 'lualine_a_inactive', -- Color for inactive tab.
          },
          cond = function()
            return #vim.api.nvim_list_tabpages() > 1
          end,
        },
        {
          noice.api.statusline.mode.get,
          cond = noice.api.statusline.mode.has,
          color = { fg = "#fb4934" },
        },
      },
      lualine_c = {},
      lualine_x = { "diagnostics", "branch" },
      lualine_y = { "filetype", "lsp_status" },
      lualine_z = { "searchcount" },
    },
  },
  config = function(_, opts)
    -- mini.icons
    require("mini.icons").setup()

    -- replace nvim-web-devicons with mini.icons
    require("mini.icons").mock_nvim_web_devicons()

    -- setup lualine with provided opts
    require("lualine").setup(opts)
  end,
}
