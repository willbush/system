return {
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      vim.lsp.enable('rust-analyzer')
      vim.lsp.config('rust-analyzer', {
        cmd = { 'rust-analyzer' },
        filetypes = { 'rust' },
      })

      vim.lsp.enable('lua-language-server')
      vim.lsp.config('lua-language-server', {
        cmd = { 'lua-language-server' },
        filetypes = { 'lua' },
      })
    end,
  },
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
}
