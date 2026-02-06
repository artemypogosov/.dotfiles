return {
  {
    -- Properly configures LuaLS for editing your Neovim config by lazily updating your workspace libraries.
    "folke/lazydev.nvim",
    ft = "lua", -- Load only for Lua files
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
    dependencies = {
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
}
