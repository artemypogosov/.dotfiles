local g = vim.g
local o = vim.o

return {
  "morhetz/gruvbox",
  lazy = false,
  priority = 1000,
  config = function()
    o.background = "dark" -- "light"
    g.gruvbox_bold = 0
    g.gruvbox_italic = 1
    g.gruvbox_contrast_dark = "medium" -- "soft", "medium" & "hard"
    g.gruvbox_contrast_light = "medium" -- "soft", "medium" & "hard"
    -- Changes cursor background while search is highlighted
    g.gruvbox_hls_cursor = "orange"
    g.gruvbox_sign_column = "bg0"
    g.gruvbox_invert_selection = 0

    vim.cmd.colorscheme("gruvbox")
    vim.api.nvim_set_hl(0, "Sneak", { fg = "#FBF1C7", bg = "#928374" })
    vim.api.nvim_set_hl(0, "SneakCurrent", { fg = "#D79921", bg = "NONE", bold = true })
  end,
}
