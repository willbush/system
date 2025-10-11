-- modes
local n = { 'n' }

local keymaps = {
  -- Note this maps over default `U` (see `:h U`) which "undo all latest
  -- changes on one line." I fail to imagine ever wanting that.
  { lhs = 'U', rhs = '<C-R>', desc = 'Redo', modes = n },
  { lhs = '<esc>', rhs = '<cmd>nohlsearch<cr>', desc = 'Clear search highlights', modes = n },

  -- Exit terminal mode in the builtin terminal with a shortcut that is a bit
  -- easier for people to discover. Otherwise, you normally need to press
  -- <C-\><C-n>, which is not what someone will guess without a bit more
  -- experience.
  { lhs = '<esc><esc>', rhs = '<C-\\><C-n>', desc = 'Exit terminal mode', modes = { 't' } },

  -- Rapid (emphasize easy access over mnemonics)
  { lhs = '<leader>rs', rhs = '<cmd>w<cr>', desc = 'Save buffer', modes = n },

  -- Buffers
  {
    lhs = '<leader>bb',
    rhs = function()
      require('fzf-lua').buffers()
    end,
    desc = 'Buffer picker',
    modes = n,
  },
  {
    lhs = '<leader>bd',
    rhs = function()
      require('snacks').bufdelete()
    end,
    desc = 'Delete Buffer',
    modes = n,
  },
  {
    lhs = '<leader>bk',
    rhs = '<cmd>bd<cr>',
    desc = 'Kill (delete) Buffer and Window',
    modes = n,
  },
  {
    lhs = '<leader>bD',
    rhs = function()
      require('snacks').bufdelete.all()
    end,
    desc = 'Delete All Buffers',
    modes = n,
  },
  {
    lhs = '<leader>bO',
    rhs = function()
      require('snacks').bufdelete.other()
    end,
    desc = 'Delete Other Buffers',
    modes = n,
  },
}

for _, ks in ipairs(keymaps) do
  local opts = { desc = ks.desc }
  -- Merge any extra options
  if ks.opts then
    opts = vim.tbl_extend('force', opts, ks.opts)
  end
  vim.keymap.set(ks.modes, ks.lhs, ks.rhs, opts)
end
