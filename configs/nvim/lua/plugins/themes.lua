return {
  -- theme releated
  { 'echasnovski/mini.icons', lazy = true, opts = {}, version = false },
  { 'echasnovski/mini.statusline', opts = {}, version = false },
  {
    'rose-pine/neovim',
    lazy = false,
    name = 'rose-pine',
    config = function()
      vim.cmd('colorscheme rose-pine')
    end,
  },
  {
    'folke/tokyonight.nvim',
    lazy = true,
    opts = {},
  },
  {
    'catppuccin/nvim',
    name = 'catppuccin',
    lazy = true,
    opts = {
      auto_integrations = true,
    },
  },
}
