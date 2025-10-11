local map = vim.keymap.set

-- Note this maps over default `U` (see `:h U`) which "undo all latest changes
-- on one line." I fail to imagine ever wanting that.
map('n', 'U', '<C-R>')

-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
map('n', '<Esc>', '<cmd>nohlsearch<CR>')

map('n', '<leader>qq', '<cmd>wqa<CR>', { desc = 'Write and Quit all' })

-- Rapid (emphasize easy access over mnemonics)
map('n', '<leader>rs', '<cmd>w<CR>', { desc = 'Save buffer' })

-- Buffers
map('n', '<leader>bd', function()
  Snacks.bufdelete()
end, { desc = 'Delete Buffer' })
map('n', '<leader>bo', function()
  Snacks.bufdelete.other()
end, { desc = 'Delete Other Buffers' })

-- map("n", "<leader>bD", "<cmd>:bd<cr>", { desc = "Delete Buffer and Window" })

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
map('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })
