local M = {}

M.setup = function()
  local present, telescope = pcall(require, "telescope")
  if not present then
    return
  end

  telescope.setup {
    extensions = {
      file_browser = {
        hidden = true,
        follow_symlinks = true,
        path="%:p:h",
        hide_parent_dir = true,
        prompt_path = true,
        grouped = true,
        select_buffer = true,
        -- Use the selected folder as cwd
        cwd_to_path = true,
      },
      fzf = {
        fuzzy = false, -- 'false' will only do exact matching
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = "respect_case", -- "ignore_case" / "respect_case" / "smart_case"
      }
    },
    defaults = {
      cwd = vim.fn.getcwd(),
      layout_config = {
        width = 0.6,
        height = 0.5,
    },
      file_ignore_patterns = { "node_modules", ".java", ".cache", "Downloads/Installed", ".steam", ".sane" },
    },
     pickers = {
       find_files = {
         find_command = { 'fd', '--type', 'f', '--follow', '--exclude', '.git' },
       },
     },
  }

  -- Load extensions safely
  pcall(telescope.load_extension, "file_browser")
  pcall(telescope.load_extension, "fzf")
  pcall(telescope.load_extension, "bookmarks")
  pcall(telescope.load_extension, "workspaces")
end

return M
