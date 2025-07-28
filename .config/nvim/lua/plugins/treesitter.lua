return {
  "nvim-treesitter/nvim-treesitter",
  event = "VeryLazy",
  build = ":TSUpdate",
  config = function()
    local configs = require("nvim-treesitter.configs")

    configs.setup({
      ensure_installed = {
        "html",
        "css",
        "scss",
        "javascript",
        "typescript",
        "vue",
        "sql",
        "dockerfile",
        "git_config",
        "git_rebase",
        "diff",
        "gitattributes",
        "gitcommit",
        "gitignore",
        "graphql",
        "json",
        "json5",
        "jsdoc",
        "markdown",
        "make",
        "lua",
        "luadoc",
        "clojure",
        "vim",
        "vimdoc",
        "nginx",
        "ssh_config",
        "yaml",
        "toml",
        "xml",
        "xresources",
        "editorconfig",
        "bash",
        "csv",
      },
      auto_install = true,
      sync_install = false,
      highlight = { enable = true },
      indent = { enable = true },

      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<Enter>", -- set to `false` to disable one of the mappings
          node_incremental = "<Enter>",
          scope_incremental = false,
          node_decremental = "<Backspace>",
        },
      },
    })
  end,
}

-- treesitter also can manage code-folding
