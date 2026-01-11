return {
  "ellisonleao/gruvbox.nvim",
  priority = 1000,
  config = function()
    require("gruvbox").setup({
      bold = false,
      overrides = {
        -- It tells Neovim to use 'Normal' styles for 'SignColumn'
        SignColumn = { link = "Normal" },

        -- Messages & command line
        ErrorMsg = { fg = "#fb4934", bg = "NONE", bold = false },
        WarningMsg = { fg = "#fabd2f", bg = "NONE", bold = false },
        MoreMsg = { fg = "#b8bb26", bg = "NONE" },
        MsgArea = { bg = "NONE", link = "Normal" },

        -- Diagnostic icons
        DiagnosticSignError = { link = "SignColumn" },
        DiagnosticSignWarn = { link = "SignColumn" },
        DiagnosticSignInfo = { link = "SignColumn" },
        DiagnosticSignHint = { link = "SignColumn" },

        -- If you use a plugin like Noice or Snacks
        NormalFloat = { bg = "NONE", link = "Normal" },
        FloatBorder = { fg = "#928374", bg = "NONE" },
        SnacksPickerNormal = { link = "Normal" },
        NoiceFormatConfirm = { bg = "NONE", link = "Normal" },
        NoiceFormatConfirmDefault = { bg = "NONE", link = "Normal" },
      },
    })
    vim.cmd("colorscheme gruvbox")
  end,
}
