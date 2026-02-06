return {
  -- Lightweight yet powerful formatter plugin for Neovim
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  opts = {
    -- You can customize some of the format options for the filetype (:help conform.format)
    -- Conform will run the first available formatter
    formatters_by_ft = {
      -- Core
      lua = { "stylua" },

      -- Markup & Templates
      html = { "prettierd", "prettier", "biome", stop_after_first = true },
      markdown = { "prettierd", "prettier" },
      mdx = { "biome", "prettierd", "prettier", stop_after_first = true },

      -- Styling
      css = { "biome", "prettierd", "prettier", stop_after_first = true },
      scss = { "biome", "prettierd", "prettier", stop_after_first = true },
      less = { "biome", "prettierd", "prettier", stop_after_first = true },
      postcss = { "biome", "prettierd", "prettier", stop_after_first = true },

      -- JavaScript / TypeScript
      javascript = { "biome", "prettierd", "prettier", stop_after_first = true },
      javascriptreact = { "biome", "prettierd", "prettier", stop_after_first = true },
      typescript = { "biome", "prettierd", "prettier", stop_after_first = true },
      typescriptreact = { "biome", "prettierd", "prettier", stop_after_first = true },

      -- Web UI Frameworks
      vue = { "biome", "prettierd", "prettier", stop_after_first = true },
      svelte = { "biome", "prettierd", "prettier", stop_after_first = true },
      astro = { "biome", "prettierd", "prettier", stop_after_first = true },
      -- Data formats
      json = { "biome", "prettierd", "prettier", stop_after_first = true },
      jsonc = { "biome", "prettierd", "prettier", stop_after_first = true },
      yaml = { "prettierd", "prettier" },
      yml = { "prettierd", "prettier" },

      -- Node / tooling configs
      graphql = { "prettierd", "prettier" },
      prisma = { "prettierd", "prettier" },

      -- Shells
      sh = { "shfmt", "beautysh", stop_after_first = true },
      bash = { "shfmt", "beautysh", stop_after_first = true },
      zsh = { "shfmt", "beautysh", stop_after_first = true },
    },
    formatters = {
      biome = {
        args = { "check", "--write", "--stdin-file-path", "$FILENAME" },
        -- Only run biome if a biome.json exists and isn't broken
        condition = function(_, ctx)
          return vim.fs.root(ctx.filename, { "biome.json", "biome.jsonc" }) ~= nil
        end,
      },
    },
    format_on_save = {
      -- These options will be passed to conform.format()
      timeout_ms = 500,
      lsp_format = "fallback",
    },
  },
}
