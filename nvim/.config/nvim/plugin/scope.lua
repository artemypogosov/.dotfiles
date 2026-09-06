local helpers = require("helpers")

-- Every workspace (tabnew) has its own set of opened tabs
helpers.add({ "tiagovla/scope.nvim" })

require("scope").setup()
