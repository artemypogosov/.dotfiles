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
        -- Use the selected folder as cwd
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
      layout_config = {
        width = 0.6,
        height = 0.5,
    },
      preview = true,
      file_ignore_patterns = {
        ".git/*", ".java/*", ".cache/*", ".local/share/*", "Downloads/Installed/*",
        ".steam/*", ".sane/*"
      },
    },
    pickers = {
      find_files = {
        find_command = {'rg', '--files', '--hidden', '-g', '!.git'},
      },
    },
  }

  -- Load extensions safely
  pcall(telescope.load_extension, "file_browser")
  pcall(telescope.load_extension, "bookmarks")
  pcall(telescope.load_extension, "workspaces")
end

return M
