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
          on_attach = function(client, bufnr)
            local function map(mode, lhs, rhs, opts)
              opts = opts or {}
              opts.buffer = bufnr
              opts.silent = true
              vim.keymap.set(mode, lhs, rhs, opts)
            end

            map('n', 'K', function()
              vim.cmd.RustLsp({ 'hover', 'actions' })
            end, { desc = 'Hover Actions (Rustaceanvim)' })
            map('n', '<leader>a', function()
              vim.cmd.RustLsp('codeAction')
            end, { desc = 'Code Action (Rustaceanvim)' })
            map('n', 'gd', vim.lsp.buf.definition, { desc = 'Go to Definition' })
          end,
        },
      }
    end,
  },
}
