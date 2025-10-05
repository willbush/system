return {
  { -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    main = 'nvim-treesitter.configs', -- Sets main module to use for opts
    -- [[ Configure Treesitter ]] See `:help nvim-treesitter`
    opts = {
      ensure_installed = {
        'bash',
        'c',
        'diff',
        'html',
        'lua',
        'luadoc',
        'markdown',
        'markdown_inline',
        'query',
        'ron',
        'rust',
        'vim',
        'vimdoc',
      },
      -- Autoinstall languages that are not installed
      -- NOTE: requires `tree-sitter` CLI installed locally
      auto_install = false, -- I grammars in my NixOS config.
      highlight = {
        enable = true,
      },
    },
  },
}
