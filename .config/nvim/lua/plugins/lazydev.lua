return {
  {
    "folke/lazydev.nvim",
    ft = "lua", -- Load only for Lua files
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
    dependencies = {
      {
        "hrsh7th/nvim-cmp",
        opts = function(_, opts)
          opts.sources = opts.sources or {}
          table.insert(opts.sources, {
            name = "lazydev",
            group_index = 0, -- disables LuaLS completions if using LazyDev
          })
        end,
      },
      {
        "saghen/blink.cmp",
        opts = {
          sources = {
            default = { "lazydev", "lsp", "path", "snippets", "buffer" },
            providers = {
              lazydev = {
                name = "LazyDev",
                module = "lazydev.integrations.blink",
                score_offset = 100,
              },
            },
          },
        },
      },
    },
  },
  {
    "folke/neodev.nvim",
    enabled = false, -- Disabled to avoid conflicts with lazydev.nvim
  },
}
