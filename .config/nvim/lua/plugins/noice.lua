return {
  -- Highly experimental plugin that completely replaces the UI for messages, cmdline and the popupmenu.
  "folke/noice.nvim",
  event = "VeryLazy",
  opts = {
    -- you can enable a preset for easier configuration
    presets = {
      bottom_search = false, -- use a classic bottom cmdline for search
      long_message_to_split = true, -- long messages will be sent to a split
      inc_rename = false, -- enables an input dialog for inc-rename.nvim
      lsp_doc_border = false, -- add a border to hover docs and signature help
    },
    cmdline = {
      view = "cmdline",
    },
    lsp = {
      -- override markdown rendering so that *cmp* and other plugins use *Treesitter*
      override = {
        ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
        ["vim.lsp.util.stylize_markdown"] = true,
        ["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
      },
    },
  },
  dependencies = {
    "MunifTanjim/nui.nvim",
  },
}
