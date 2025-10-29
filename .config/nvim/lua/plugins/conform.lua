return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  opts = {
    formatters_by_ft = {
      lua = { "stylua" },
      -- Conform will run multiple formatters sequentially
      -- You can customize some of the format options for the filetype (:help conform.format)
      -- Conform will run the first available formatter
      javascript = { "prettierd", "prettier", stop_after_first = true },
      typescript = { "prettierd", "prettier", stop_after_first = true },

      sh = { "shfmt", "beautysh", stop_after_first = true },
      bash = { "shfmt", "beautysh", stop_after_first = true },
      zsh = { "shfmt", "beautysh", stop_after_first = true },
    },
    format_on_save = {
      -- These options will be passed to conform.format()
      timeout_ms = 500,
      lsp_format = "fallback",
    },
  },
}
