return {
  "nvim-treesitter/nvim-treesitter-textobjects",
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
  },
  init = function()
    require("nvim-treesitter.configs").setup({
      textobjects = {
        select = {
          enable = true,
          lookahead = true, -- Jump forward to textobject like targets.vim
          keymaps = {
            -- Functions
            ["af"] = { query = "@function.outer", desc = "Function (outer)" },
            ["if"] = { query = "@function.inner", desc = "Function (inner)" },

            -- Classes
            ["ac"] = { query = "@class.outer", desc = "Class (outer)" },
            ["ic"] = { query = "@class.inner", desc = "Class (inner)" },

            -- Comments (only if your grammar supports it)
            ["ao"] = { query = "@comment.outer", desc = "Comment (outer)" },

            -- Custom/local scopes
            ["as"] = {
              query = "@local.scope",
              query_group = "locals",
              desc = "Language scope",
            },
          },
          selection_modes = {
            ["@parameter.outer"] = "v", -- character-wise
            ["@function.outer"] = "V", -- line-wise
            ["@class.outer"] = "<c-v>", -- block-wise
          },
          include_surrounding_whitespace = true,
        },

        swap = {
          enable = true,
          swap_next = {
            ["<leader>n"] = { query = "@parameter.inner", desc = "Swap with next parameter" },
          },
          swap_previous = {
            ["<leader>N"] = { query = "@parameter.inner", desc = "Swap with previous parameter" },
          },
        },
      },
    })
  end,
}
