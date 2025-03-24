local M = {}

local git_custom = function()
  local handle = io.popen("git rev-parse --abbrev-ref HEAD 2>/dev/null")
  local branch = handle:read("*a"):gsub("\n", "")  -- Get the branch name
  handle:close()

  return branch ~= "" and " " .. branch .. " " or ""
end

M.modules = {
  statusline = {
    git_custom = git_custom,
  }
}
  return M
