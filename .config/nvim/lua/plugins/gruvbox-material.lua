local g = vim.g

return {
  "sainnhe/gruvbox-material",
  lazy = false,
  priority = 1000,
  config = function()
    g.gruvbox_material_better_performance = 1
    g.gruvbox_material_foreground = "mix" -- "material" & "mix" & "original"
    g.gruvbox_material_background = "medium"
    g.gruvbox_material_ui_contrast = "low"
    g.gruvbox_material_float_style = "dim"
    g.gruvbox_material_enable_italic = 1
    g.gruvbox_material_enable_bold = 0
    g.gruvbox_material_spell_foreground = "none"
    g.gruvbox_material_diagnostic_line_highlight = 1
    g.gruvbox_material_statusline_style = "default" -- "mix" & "original"

    -- vim.cmd.colorscheme("gruvbox-material")
  end,
}
