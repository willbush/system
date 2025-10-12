return {
  -- theme releated
  { 'echasnovski/mini.icons', lazy = true, opts = {}, version = false },
  { 'echasnovski/mini.statusline', opts = {}, version = false },
  -- themes
  {
    'folke/tokyonight.nvim',
    priority = 1000, -- Make sure to load this before all the other start plugins.
    init = function()
      vim.cmd.colorscheme('tokyonight-night')

      -- You can configure highlights by doing something like:
      vim.cmd.hi('Comment gui=none')
    end,
  },
}
