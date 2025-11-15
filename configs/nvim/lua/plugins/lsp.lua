return {
  {
    -- `lazydev` configures Lua LSP for your Neovim config, runtime and plugins
    -- used for completion, annotations and signatures of Neovim apis
    'folke/lazydev.nvim',
    ft = 'lua', -- only load on lua files
    opts = {
      library = {
        -- Load luvit types when the `vim.uv` word is found
        { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
      },
    },
  },
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      vim.lsp.enable({
        'lua_ls',
      })
    end,
  },
  {
    -- This handles setting lsp for rust
    'mrcjkb/rustaceanvim',
    version = '^6',
    lazy = false, -- This plugin is already lazy
    config = function()
      vim.g.rustaceanvim = {
        server = {
          -- TODO: need to look into large project perf more..
          -- - maybe try https://github.com/ethowitz/cargo-subspace
          -- - maybe try https://github.com/crisidev/bacon-ls
          default_settings = {
            ['rust-analyzer'] = {
              checkOnSave = true,
              check = {
                command = 'check',
                workspace = false,
                allTargets = false,
              },
            },
          },
          on_attach = function(_, bufnr)
            local function map(mode, lhs, rhs, opts)
              opts = opts or {}
              opts.buffer = bufnr
              opts.silent = true
              vim.keymap.set(mode, lhs, rhs, opts)
            end

            map('n', 'K', function()
              vim.cmd.RustLsp({ 'hover', 'actions' })
            end, { desc = 'Hover Actions (Rustaceanvim)' })

            map('n', '<leader>pa', vim.lsp.buf.code_action, { desc = 'Code Action' })

            -- Not exactly sure if rustaceanvim adds actions not found through
            -- nvim api above. Leaving this here to see.
            map('n', '<leader>pA', function()
              vim.cmd.RustLsp('codeAction')
            end, { desc = 'Code Action (Rustaceanvim)' })

            map('n', 'gd', vim.lsp.buf.definition, { desc = 'Go to Definition' })

            map('n', '<leader>pc', function()
              vim.cmd.RustLsp('flyCheck') -- defaults to run
            end, { desc = 'Run check' })

            map('n', '<leader>pC', function()
              vim.cmd.RustLsp({ 'flyCheck', 'clear' })
            end, { desc = 'Clear check' })

            map('n', '<leader>px', function()
              vim.cmd.RustLsp({ 'flyCheck', 'cancel' })
            end, { desc = 'Cancel check' })
          end,
        },
      }
    end,
  },
}
