local M = {}

M.setup = function()
  require('telescope').setup {
    extensions = {
      file_browser = {
        respect_gitignore = false,
        hidden = true,
        grouped = true,
        select_buffer = true,
        cwd_to_path = true, -- This makes it use the selected folder as cwd
      }
    },
    defaults = {
      -- Set the cwd dynamically based on the current working directory
      cwd = vim.fn.getcwd(),
      preview = false,
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
end

return M
