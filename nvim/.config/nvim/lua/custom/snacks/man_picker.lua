-- Explicit require: no global Snacks
local Snacks = require("snacks")

local M = {}

-----------------------------------------------------------------------
-- Lazy-loaded whatis cache (IMPORTANT for performance)
-----------------------------------------------------------------------
local WHATIS = nil

local function load_whatis_map()
  local map = {}

  -- Single bulk call (fast, no freeze)
  local out = vim.fn.system("whatis -w '*'")
  if vim.v.shell_error ~= 0 then
    return map
  end

  for line in out:gmatch("[^\n]+") do
    -- Example:
    -- ls, dir, vdir (1) - list directory contents
    local names, section, desc = line:match("^(.-)%s+%(([%d%a]+)%)%s+%-%s+(.*)$")

    if names then
      for name in names:gmatch("[^,%s]+") do
        map[name] = { section, desc }
      end
    end
  end

  return map
end

local function get_whatis()
  if WHATIS then
    return WHATIS
  end
  WHATIS = load_whatis_map()
  return WHATIS
end

-----------------------------------------------------------------------
-- Collect real command man pages (sections 1 and 8 only)
-----------------------------------------------------------------------
local function get_man_commands()
  local dirs = {
    "/usr/share/man/man1",
    "/usr/share/man/man8",
  }

  local seen = {}
  local items = {}
  local whatis = get_whatis()

  for _, dir in ipairs(dirs) do
    local fs = vim.loop.fs_scandir(dir)
    if fs then
      while true do
        local name = vim.loop.fs_scandir_next(fs)
        if not name then
          break
        end

        -- ls.1.gz → ls
        local cmd = name:match("^(.-)%.%d")
        if cmd and not seen[cmd] then
          seen[cmd] = true
          local info = whatis[cmd]

          table.insert(items, {
            text = cmd,
            value = cmd,
            section = info and info[1],
            desc = info and info[2],
          })
        end
      end
    end
  end

  table.sort(items, function(a, b)
    return a.text < b.text
  end)

  return items
end

-----------------------------------------------------------------------
-- Public picker entry point
-----------------------------------------------------------------------
function M.open()
  Snacks.picker({
    title = "Man",

    layout = { preset = "select", layout = { height = 0.25 } },
    finder = get_man_commands,

    -- Snacks requires highlight chunks (NOT strings)
    format = function(item)
      return {
        { item.text, "SnacksPickerLabel" },
        item.section and { " (" .. item.section .. ") ", "Comment" } or { " ", "Normal" },
        item.desc and { item.desc, "SnacksPickerDesc" } or {},
      }
    end,

    actions = {
      confirm = function(picker, item)
        picker:close()
        vim.cmd("Man " .. item.value)
      end,
    },
  })
end

return M
