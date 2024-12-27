local map = vim.keymap.set

-- up/down/left/right movements
map({'n', 'x', 'o'}, 'm', 'h')
map({'n', 'x', 'o'}, 'n', 'j')
map({'n', 'x', 'o'}, 'e', 'k')
map({'n', 'x', 'o'}, 'i', 'l')

-- far word/WORD
map({'n', 'x', 'o'}, 'f', 'e')
map({'n', 'x', 'o'}, 'F', 'E')

-- insert mode
map('n', 'l', 'i')
map('n', 'L', 'I')
map('n', 'U', '<C-R>')

-- revise/range
map({'n', 'x', 'o'}, 'v', 'r')
map({'n', 'x', 'o'}, 'V', 'R')
map({'n', 'x'}, 'r', 'v')
map({'n', 'x'}, 'R', 'V')
map('n', 'gr', 'gv')
map('n', '<C-R>', '<C-V>')

-- search
map({'n', 'x', 'o'}, 's', 'f')
map({'n', 'x', 'o'}, 'S', 'F')

-- next/prev
map({'n', 'x', 'o'}, 'h', 'n')
map({'n', 'x', 'o'}, 'H', 'N')

-- line navigation
map({'n', 'x', 'o'}, 'M', '0')
map({'n', 'x', 'o'}, 'I', '$')

-- scrolling
map({'n', 'x', 'o'}, 'N', '10j')
map({'n', 'x', 'o'}, 'E', '10k')
map({'n', 'x', 'o'}, '<C-S-n>', '<C-D>')
map({'n', 'x', 'o'}, '<C-S-e>', '<C-U>')

-- join lines
map('n', '<C-J>', 'J')

-- marks
map('n', 'k', 'm')
map({'n', 'o'}, 'j', '`')
map({'n', 'o'}, 'J', "'")

-- find char repeats
map({'n', 'x', 'o'}, "'", ';')
map({'n', 'x', 'o'}, '"', ',')

-- register access
map({'n', 'x', 'o'}, '&', '"')

-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
map('n', '<Esc>', '<cmd>nohlsearch<CR>')

map('n', '<leader>qq', '<cmd>wqa<CR>', { desc = 'Write and [Q]uit all' })
map('n', '<leader>bd', '<cmd>q<CR>', { desc = '[D]elete Buffer' })

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

map("n", "<C-w>m", "<C-w>h", { desc = "Go to Left Window" })
map("n", "<C-w>h", "<nop>")

map("n", "<C-w>n", "<C-w>h", { desc = "Go to Left Window" })
map("n", "<C-w>j", "<nop>")
map("n", "<C-w>k", "<nop>")
map("n", "<C-w>l", "<nop>")
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
