local map = vim.keymap.set

-- up/down/left/right movements
map({ 'n', 'x' }, 'm', 'h', { desc = 'Left' })
map({ 'n', 'x' }, 'n', 'j', { desc = 'Down' })
map({ 'n', 'x' }, 'e', 'k', { desc = 'Up' })
map({ 'n', 'x' }, 'i', 'l', { desc = 'Right' })

-- far word/WORD
map({ 'n', 'x', 'o' }, 'f', 'e', { desc = 'Far word forward' })
map({ 'n', 'x', 'o' }, 'F', 'E', { desc = 'Far WORD forward' })
map({ 'n', 'x', 'o' }, 'gf', 'ge', { desc = 'Far word forward' })
map({ 'n', 'x', 'o' }, 'gF', 'gE', { desc = 'Far WORD forward' })

-- insert mode
map('n', 'l', 'i')
map('n', 'L', 'I')
map('n', 'U', '<C-R>')

-- revise/range
map({ 'n', 'x', 'o' }, 'v', 'r')
map({ 'n', 'x', 'o' }, 'V', 'R')
map({ 'n', 'x' }, 'r', 'v')
map({ 'n', 'x' }, 'R', 'V')
map('n', 'gr', 'gv')
map('n', '<C-R>', '<C-V>')

-- search
map({ 'n', 'x', 'o' }, 's', 'f')
map({ 'n', 'x', 'o' }, 'S', 'F')

-- next/prev
map({ 'n', 'x', 'o' }, 'h', 'n')
map({ 'n', 'x', 'o' }, 'H', 'N')

-- line navigation
map({ 'n', 'x', 'o' }, 'M', '0')
map({ 'n', 'x', 'o' }, 'I', '$')

-- scrolling
map({ 'n', 'x', 'o' }, 'N', '10j')
map({ 'n', 'x', 'o' }, 'E', '10k')
map({ 'n', 'x', 'o' }, '<C-S-n>', '<C-D>')
map({ 'n', 'x', 'o' }, '<C-S-e>', '<C-U>')

-- join lines
map('n', '<C-J>', 'J')

-- marks
map('n', 'k', 'm')
map({ 'n', 'o' }, 'j', '`')
map({ 'n', 'o' }, 'J', "'")

-- find char repeats
map({ 'n', 'x', 'o' }, "'", ';')
map({ 'n', 'x', 'o' }, '"', ',')

-- register access
map({ 'n', 'x', 'o' }, '&', '"')

-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
map('n', '<Esc>', '<cmd>nohlsearch<CR>')

map('n', '<leader><space>', ':', { desc = 'Command mode' })

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
--
-- NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
map('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Keybinds to make split navigation easier.
--  Use CTRL+<hjkl> to switch between windows
--
--  See `:help wincmd` for a list of all window commands

-- Disable original hjkl bindings
map('n', '<C-w><C-h>', '<nop>')
map('n', '<C-w><C-j>', '<nop>')
map('n', '<C-w><C-k>', '<nop>')
map('n', '<C-w><C-l>', '<nop>')
map('n', '<C-w>H', '<nop>')
map('n', '<C-w>J', '<nop>')
map('n', '<C-w>K', '<nop>')
map('n', '<C-w>L', '<nop>')
map('n', '<C-w>h', '<nop>')
map('n', '<C-w>j', '<nop>')
map('n', '<C-w>k', '<nop>')
map('n', '<C-w>l', '<nop>')

-- New mnei bindings for window navigation
map('n', '<C-w><C-e>', '<C-w>k', { desc = 'Go to the Top Window' })
map('n', '<C-w><C-i>', '<C-w>l', { desc = 'Go to the Right Window' })
map('n', '<C-w><C-m>', '<C-w>h', { desc = 'Go to the Left Window' })
map('n', '<C-w><C-n>', '<C-w>j', { desc = 'Go to the Bottom Window' })
map('n', '<C-w>E', '<C-w>K', { desc = 'Move window to top' })
map('n', '<C-w>I', '<C-w>L', { desc = 'Move window far right' })
map('n', '<C-w>M', '<C-w>H', { desc = 'Move window far left' })
map('n', '<C-w>N', '<C-w>J', { desc = 'Move window to bottom' })
map('n', '<C-w>e', '<C-w>k', { desc = 'Go to the Top Window' })
map('n', '<C-w>i', '<C-w>l', { desc = 'Go to the Right Window' })
map('n', '<C-w>m', '<C-w>h', { desc = 'Go to the Left Window' })
map('n', '<C-w>n', '<C-w>j', { desc = 'Go to the Bottom Window' })

-- |CTRL-W_H|	CTRL-W H	   move current window to the far left
-- |CTRL-W_J|	CTRL-W J	   move current window to the very bottom
-- |CTRL-W_K|	CTRL-W K	   move current window to the very top
-- |CTRL-W_L|	CTRL-W L	   move current window to the far right
--
-- |CTRL-W_h|	CTRL-W h	   go to Nth left window (stop at first window)
-- |CTRL-W_j|	CTRL-W j	   go N windows down (stop at last window)
-- |CTRL-W_k|	CTRL-W k	   go N windows up (stop at first window)
-- |CTRL-W_l|	CTRL-W l	   go to Nth right window (stop at last window)
--
-- |CTRL-W_i|	CTRL-W i	   split window and jump to declaration of identifier under the cursor
--

-- map("n", "<C-m>", "<C-w>m", { desc = "Go to Left Window", remap = true })

-- map("n", "<C-m>", "<C-w>m", { desc = "Go to Left Window", remap = true })
-- map('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
-- map('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
-- map('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
-- map('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

-- map("n", "<C-h>", "<C-w>h", { desc = "Go to Left Window", remap = true })

-- vim.keymap.set('n', '<C-w>m', '<C-w>h')
-- vim.keymap.set('n', '<C-w><C-m>', '<C-w>h')

-- vim.keymap.set('n', '<C-w>h', '<nop>')
-- vim.keymap.set('n', '<C-w><C-h>', '<nop>')

-- map("n", "<C-j>", "<C-w>j", { desc = "Go to Lower Window", remap = true })
-- map("n", "<C-k>", "<C-w>k", { desc = "Go to Upper Window", remap = true })
-- map("n", "<C-l>", "<C-w>l", { desc = "Go to Right Window", remap = true })
