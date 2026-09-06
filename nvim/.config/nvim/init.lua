-- Startup time
_G.StartupTime = vim.uv.hrtime()

vim.api.nvim_create_autocmd("VimEnter", {
	once = true,
	callback = function()
		_G.StartupTimeMs = (vim.uv.hrtime() - _G.StartupTime) / 1e6
	end,
})

require("options")
require("autocmd")
