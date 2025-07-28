return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {
    options = {
      theme = "gruvbox-material",
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
      },
      lualine_c = {},
      lualine_x = { "diagnostics", "branch" },
      lualine_y = { "filetype", "lsp_status" },
      lualine_z = { "searchcount" },
    },
  },
}
