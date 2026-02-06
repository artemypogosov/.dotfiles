---@diagnostic disable [missing-fields]

return {
  -- Syntax aware text-objects, select, move, swap, and peek support
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
            ["io"] = { query = "@comment.inner", desc = "Comment (inner)" },

            -- Attribute
            ["aa"] = { query = "@attribute.outer", desc = "Attribute (outer)" },
            ["ia"] = { query = "@attribute.inner", desc = "Attribute (inner)" },

            -- Block
            ["ab"] = { query = "@block.outer", desc = "Block (outer)" },
            ["ib"] = { query = "@block.inner", desc = "Block (inner)" },

            -- Block
            ["ar"] = { query = "@return.outer", desc = "Return (outer)" },
            ["ir"] = { query = "@return.inner", desc = "Return (inner)" },

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
            ["<A-n>"] = { query = "@parameter.inner", desc = "Swap with next parameter" },
          },
          swap_previous = {
            ["<A-N>"] = { query = "@parameter.inner", desc = "Swap with previous parameter" },
          },
        },
      },
    })
  end,
}
