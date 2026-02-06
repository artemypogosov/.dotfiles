---@diagnostic disable [missing-fields]

-- LSP - Language Server Protocol
-- :LspInfo - get info about current LSP status

return {
  -- Quickstart configs for Nvim LSP
  "neovim/nvim-lspconfig",
  dependencies = {
    -- Automatically install LSPs and related tools to stdpath for Neovim
    -- Mason must be loaded before its dependents so we need to set it up here.
    -- NOTE: `opts = {}` is the same as calling `require('mason').setup({})`
    { "mason-org/mason.nvim", opts = {} },
    "mason-org/mason-lspconfig.nvim",
    "WhoIsSethDaniel/mason-tool-installer.nvim",

    -- Allows extra capabilities provided by blink.cmp
    "saghen/blink.cmp",
  },
  config = function()
    local snacks = require("snacks")
    -- Brief aside: **What is LSP?**
    --
    -- LSP is an initialism you've probably heard, but might not understand what it is.
    --
    -- LSP stands for Language Server Protocol. It's a protocol that helps editors
    -- and language tooling communicate in a standardized fashion.
    --
    -- In general, you have a "server" which is some tool built to understand a particular
    -- language (such as `gopls`, `lua_ls`, `rust_analyzer`, etc.). These Language Servers
    -- (sometimes called LSP servers, but that's kind of like ATM Machine) are standalone
    -- processes that communicate with some "client" - in this case, Neovim!
    --
    -- LSP provides Neovim with features like:
    --  - Go to definition
    --  - Find references
    --  - Autocompletion
    --  - Symbol Search
    --  - and more!
    --
    -- Thus, Language Servers are external tools that must be installed separately from
    -- Neovim. This is where `mason` and related plugins come into play.
    --
    -- If you're wondering about lsp vs treesitter, you can check out the wonderfully
    -- and elegantly composed help section, `:help lsp-vs-treesitter`

    --  This function gets run when an LSP attaches to a particular buffer.
    --    That is to say, every time a new file is opened that is associated with
    --    an lsp (for example, opening `main.rs` is associated with `rust_analyzer`) this
    --    function will be executed to configure the current buffer
    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup("kickstart-lsp-attach", { clear = true }),
      callback = function(event)
        -- NOTE: Remember that Lua is a real programming language, and as such it is possible
        -- to define small helper and utility functions so you don't have to repeat yourself.
        --
        -- In this case, we create a function that lets us more easily define mappings specific
        -- for LSP related items. It sets the mode, buffer and description for us each time.
        local map = function(keys, func, desc, mode)
          mode = mode or "n"
          vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = desc })
        end

        -- []d --> jump between warnings
        -- Shift + K --> get info lookup (almost like in emacs)

        -- Rename the variable under your cursor.
        --  Most Language Servers support renaming across files, etc.
        map("<leader>cr", vim.lsp.buf.rename, "Rename")

        -- Jump to the definition of the word under your cursor.
        --  This is where a variable was first declared, or where a function is defined, etc.
        --  To jump back, press <C-t>.
        map("<leader>cd", snacks.picker.lsp_definitions, "Goto Definition")

        -- Find references for the word under your cursor.
        map("<leader>cD", snacks.picker.lsp_references, "Goto References")

        -- Jump to the implementation of the word under your cursor.
        --  Useful when your language has ways of declaring types without an actual implementation.
        map("<leader>ci", snacks.picker.lsp_implementations, "Goto Implementation")

        -- Interactively shows definition, references, implementations and type definitions
        map("<leader>cl", "<cmd>Trouble lsp toggle win.position=right win.size=55<cr>", "Defs/Refs/Impl...")

        -- Execute a code action, usually your cursor needs to be on top of an error
        -- or a suggestion from your LSP for this to activate.
        map("<leader>ca", vim.lsp.buf.code_action, "Goto Code Action", { "n", "x" })

        -- Fuzzy find all the symbols in your current document.
        -- Symbols are things like variables, functions, types, etc.
        map("<leader>cs", "<cmd>Trouble symbols toggle focus=true win.size=55<cr>", "Buffer Symbols")

        -- Fuzzy find all the symbols in your current workspace.
        --  Similar to document symbols, except searches over your entire project.
        map("<leader>co", snacks.picker.treesitter, "Treesitter objects")
        map("<leader>cS", snacks.picker.lsp_workspace_symbols, "Project Symbols")

        -- Jump to the type of the word under your cursor.
        --  Useful when you're not sure what type a variable is and you want to see
        --  the definition of its *type*, not where it was *defined*.
        map("<leader>cT", snacks.picker.lsp_type_definitions, "Goto Type Definition")

        -- [DIAGNOSTICS] --> use 'trouble' plugin instead
        --
        -- Show buffer diagnostics
        -- map("<leader>cx", function()
        --   snacks.picker.diagnostics_buffer()
        -- end, "Current Buffer Diagnostics")

        -- Show workspace diagnostics
        -- map("<leader>cX", snacks.picker.diagnostics, "Open Buffers Diagnostics")
        --

        -- WARN: This is not Goto Definition, this is Goto Declaration.
        --  For example, in C this would take you to the header.
        -- map("<leader>cD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

        -- This function resolves a difference between neovim nightly (version 0.11) and stable (version 0.10)
        ---@param client vim.lsp.Client
        ---@param method vim.lsp.protocol.Method
        ---@param bufnr? integer some lsp support methods only in specific files
        ---@return boolean
        local function client_supports_method(client, method, bufnr)
          if vim.fn.has("nvim-0.11") == 1 then
            return client:supports_method(method, bufnr)
          else
            return client.supports_method(method, { bufnr = bufnr })
          end
        end

        local client = vim.lsp.get_client_by_id(event.data.client_id)
        -- The following code creates a keymap to toggle inlay hints in your
        -- code, if the language server you are using supports them
        --
        -- This may be unwanted, since they displace some of your code
        if client and client_supports_method(client, vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
          map("<leader>ch", function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
          end, "Toggle Inlay Hints")
        end
      end,
    })

    -- Diagnostic Config
    -- See :help vim.diagnostic.Opts
    vim.diagnostic.config({
      severity_sort = true,
      float = { border = "rounded", source = "if_many" },
      underline = { severity = vim.diagnostic.severity.ERROR },
      signs = {
        text = {
          [vim.diagnostic.severity.ERROR] = " 󰅚",
          [vim.diagnostic.severity.WARN] = " 󰀪",
          [vim.diagnostic.severity.INFO] = " 󰋽",
          [vim.diagnostic.severity.HINT] = " 󰌶",
        },
      } or {},
      virtual_text = {
        source = "if_many",
        spacing = 2,
        format = function(diagnostic)
          local diagnostic_message = {
            [vim.diagnostic.severity.ERROR] = diagnostic.message,
            [vim.diagnostic.severity.WARN] = diagnostic.message,
            [vim.diagnostic.severity.INFO] = diagnostic.message,
            [vim.diagnostic.severity.HINT] = diagnostic.message,
          }
          return diagnostic_message[diagnostic.severity]
        end,
      },
    })

    -- LSP servers and clients are able to communicate to each other what features they support.
    --  So, we create new capabilities with blink.cmp, and then broadcast that to the servers.
    local original_capabilities = vim.lsp.protocol.make_client_capabilities()
    local capabilities = require("blink-cmp").get_lsp_capabilities(original_capabilities)

    -- Enable the following language servers
    local servers = {
      -- Core Web Languages
      html = {},
      cssls = {},
      jsonls = {},
      yamlls = {},

      -- Framework ecosystems
      svelte = {}, -- Svelte
      vue_ls = {}, -- Vue
      astro = {}, -- Astro

      -- Styling systems
      tailwindcss = {}, -- Tailwind CSS

      -- Node & tooling
      bashls = {}, -- Shell scripts
      dockerls = {}, -- Docker
      prismals = {}, -- Prisma schema
      lua_ls = {
        settings = {
          Lua = {
            hint = { enable = true },
            workspace = { checkThirdParty = false },
            telemetry = { enable = false },
            diagnostics = { disable = { "missing-fields" } },
          },
        },
      },
    }

    -- Ensure the servers and tools above are installed
    local ensure_installed = vim.tbl_keys(servers or {})
    -- Manual addition of ts_ls to ensure it's installed by Mason
    table.insert(ensure_installed, "ts_ls")
    require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

    require("mason-lspconfig").setup({
      handlers = {
        function(server_name)
          -- SKIP ts_ls here to avoid the "nil" settings error from automatic setup
          if server_name == "ts_ls" then
            return
          end

          local server = servers[server_name] or {}
          server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
          require("lspconfig")[server_name].setup(server)
        end,
      },
    })

    --- THE MANUAL WORKING SETUP FOR TS_LS (STOPS THE WARNING AND ENABLES HINTS) ---
    vim.lsp.config("ts_ls", {
      capabilities = capabilities,
      settings = {
        typescript = {
          inlayHints = {
            includeInlayParameterNameHints = "all",
            includeInlayParameterNameHintsWhenArgumentMatchesName = false,
            includeInlayFunctionParameterTypeHints = true,
            includeInlayVariableTypeHints = true,
            includeInlayPropertyDeclarationTypeHints = true,
            includeInlayFunctionLikeReturnTypeHints = true,
            includeInlayEnumMemberValueHints = true,
          },
        },
        javascript = {
          inlayHints = {
            includeInlayParameterNameHints = "all",
            includeInlayParameterNameHintsWhenArgumentMatchesName = false,
            includeInlayFunctionParameterTypeHints = true,
            includeInlayVariableTypeHints = true,
            includeInlayPropertyDeclarationTypeHints = true,
            includeInlayFunctionLikeReturnTypeHints = true,
            includeInlayEnumMemberValueHints = true,
          },
        },
      },
    })
    vim.lsp.enable("ts_ls")
  end,
}
