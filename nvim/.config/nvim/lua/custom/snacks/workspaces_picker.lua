local Snacks = require("snacks")

local M = {}

local function get_workspaces()
  local workspaces = require("workspaces").get()
  local items = {}

  for _, ws in ipairs(workspaces) do
    table.insert(items, {
      name = ws.name,
      path = ws.path,
    })
  end

  return items
end

function M.open()
  Snacks.picker({
    title = "Workspaces",

    finder = get_workspaces,

    format = function(item)
      return {
        { item.name, "SnacksPickerLabel" },
        { " ", "" },
        { vim.fn.fnamemodify(item.path, ":~"), "SnacksPickerDesc" },
      }
    end,

    actions = {
      confirm = function(picker, item)
        picker:close()
        vim.cmd("WorkspacesOpen " .. item.name)
      end,
    },
  })
end

return M
