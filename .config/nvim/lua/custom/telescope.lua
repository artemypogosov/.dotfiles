local M = {}

M.setup = function()
  local present, telescope = pcall(require, "telescope")
  if not present then
    return
  end

  telescope.setup {
    extensions = {
      file_browser = {
        respect_gitignore = false,
        hidden = true,
        grouped = true,
        select_buffer = true,
        -- This makes it use the selected folder as cwd
        cwd_to_path = true,
      },
      fzf = {
        fuzzy = false,                   -- false will only do exact matching
        override_generic_sorter = true,  -- override the generic sorter
        override_file_sorter = true,     -- override the file sorter
        case_mode = "respect_case",      -- "ignore_case" / "respect_case" / "smart_case"
      }
    },
    defaults = {
      cwd = vim.fn.getcwd(),
      preview = true,
      file_ignore_patterns = {
        ".git/*", ".java/*", ".cache/*", ".local/share/*", "Downloads/Installed/*",
        ".steam/*", ".sane/*"
      },
    },
    pickers = {
      find_files = {
        find_command = {'rg', '--files', '--hidden', '-g', '!.git'},
        layout_config = {
          height = 0.70
        }
      },
    },
  }

  -- Load extensions safely
  pcall(telescope.load_extension, "file_browser")
end

return M
