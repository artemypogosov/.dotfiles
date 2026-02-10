local session = require("mini.sessions")

return {
  -- A collection of QoL (Quality of Life) plugins for Neovim
  -- Ctrl + q --> sends Snacks result to 'Quick Fix List'
  "folke/snacks.nvim",
  init = function()
    vim.api.nvim_create_autocmd("ColorScheme", {
      desc = "Clean up LSP highlights and set Snacks underlines",
      callback = function()
        local hl_groups = {
          "LspReferenceText",
          "LspReferenceRead",
          "LspReferenceWrite",
          "SnacksWords",
          "SnacksWordsRead",
          "SnacksWordsWrite",
        }
        for _, group in ipairs(hl_groups) do
          vim.api.nvim_set_hl(0, group, { underline = true, bg = "none", force = true })
        end
      end,
    })
  end,
  opts = {
    gitbrowse = {},
    indent = {},
    terminal = {},
    explorer = {},
    rename = {},
    bufdelete = {},
    scratch = {
      win = {
        width = 170,
        height = 50,
      },
    },
    words = { debounce = 150 },
    notifier = { style = "minimal", top_down = false },
    zen = {
      toggles = {
        dim = false,
        mini_diff_signs = true,
      },
      win = {
        backdrop = { transparent = false, blend = 75 },
      },
    },
    picker = {
      actions = {
        -- Define a custom action
        open_in_trouble = function(picker)
          picker:action("qflist") -- This fills the list
          vim.schedule(function()
            vim.cmd("cclose")
            require("trouble").open("quickfix")
          end)
        end,
      },
      win = {
        input = {
          keys = {
            -- Map the key to our custom action
            ["<c-q>"] = { "open_in_trouble", mode = { "i", "n" } },
          },
        },
      },
      sources = {
        explorer = { hidden = true },
        diagnostics = { hidden = true },
        lines = { layout = { preset = "select", layout = { height = 0.5 } } },
        recent = { layout = { preset = "select", layout = { height = 0.25 } } },
        files = {
          layout = { preset = "select", layout = { height = 0.25 } },
          ignored = true,
          hidden = true,
          args = {
            "--type",
            "f",
            "--strip-cwd-prefix",
            "--exclude",
            ".git",
            "--exclude",
            "node_modules",
            "--exclude",
            ".cache",
          },
          matcher = {
            fuzzy = false,
          },
        },
        git_log_file = {
          current_file = true,
        },
        grep_word = {
          hidden = true,
        },
        grep = {
          hidden = true,
        },
        spelling = {},
        buffers = {
          current = false,
          sort_lastused = true,
          unloaded = false,
          layout = { preset = "select", layout = { height = 0.25 } },
        },
        man = {
          matcher = {
            fuzzy = false,
          },
        },
      },
    },
    dashboard = {
      width = 40,
      preset = {
        keys = {
          {
            key = "l",
            desc = "Load session",
            action = function()
              session.select("read")
            end,
          },
          { key = "r", desc = "Recent files", action = ":lua Snacks.picker.recent()" },
          {
            key = "p",
            desc = "Open project",
            action = ":lua require('custom.snacks.project_picker').open()",
          },
        },
        header = [[
███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗
████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║
██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║
██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║
██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║
╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝

To see with eyes unclouded by hate.⠀]],
      },
      sections = {
        { section = "header" },
        { section = "keys", gap = 1, padding = 2 },
        { section = "startup", icon = "" },
      },
    },
  },
}
