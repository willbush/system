-- not rebinding these cause I would never use them anyway:
-- Insert mode:
-- `C-g C-j` or `C-g j` line down, to column where inserting started
-- `C-g C-k` or `C-g k` line up, to column where inserting started

-- `:h map-table` for reference:
--          Mode  | Norm | Ins | Cmd | Vis | Sel | Opr | Term | Lang | ~
-- Command        +------+-----+-----+-----+-----+-----+------+------+ ~
-- [nore]map      | yes  |  -  |  -  | yes | yes | yes |  -   |  -   |
-- n[nore]map     | yes  |  -  |  -  |  -  |  -  |  -  |  -   |  -   |
-- [nore]map!     |  -   | yes | yes |  -  |  -  |  -  |  -   |  -   |
-- i[nore]map     |  -   | yes |  -  |  -  |  -  |  -  |  -   |  -   |
-- c[nore]map     |  -   |  -  | yes |  -  |  -  |  -  |  -   |  -   |
-- v[nore]map     |  -   |  -  |  -  | yes | yes |  -  |  -   |  -   |
-- x[nore]map     |  -   |  -  |  -  | yes |  -  |  -  |  -   |  -   |
-- s[nore]map     |  -   |  -  |  -  |  -  | yes |  -  |  -   |  -   |
-- o[nore]map     |  -   |  -  |  -  |  -  |  -  | yes |  -   |  -   |
-- t[nore]map     |  -   |  -  |  -  |  -  |  -  |  -  | yes  |  -   |
-- l[nore]map     |  -   | yes | yes |  -  |  -  |  -  |  -   | yes  |

-- modes
local n = { 'n' }
local nx = { 'n', 'x' }
local nxo = { 'n', 'x', 'o' }
local xo = { 'x', 'o' }

local keymaps = {
  -- Core movements
  { lhs = 'm', rhs = 'h', desc = 'Left', modes = nxo },
  { lhs = 'n', rhs = 'j', desc = 'Down', modes = nxo },
  { lhs = 'e', rhs = 'k', desc = 'Up', modes = nxo },
  { lhs = 'i', rhs = 'l', desc = 'Right', modes = nxo, opts = { nowait = true } },
  --^ Special case for 'l' -> 'i' which needs `nowait` because `i` in visual-mode
  -- triggers a text object wait (e.g. `viw`).

  -- Bookkeeping Table because it's hard to keep this all in my
  -- head. I paste table after key bindings, then update it.
  --
  -- This table helps me answers the following questions:
  -- After the above key maps are applied:
  -- - What keys need to be remapped?
  -- - What keys are avaiable for mapping?
  -- - And for what mode?
  --
  -- Columns:
  -- - left of center is [A]vaiable
  -- - right of center is [R]emap or should `<nop>`

  -- |Norm n | Vis x | Opr o |
  -- | R   A | R   A | R   A |
  -- +-------+-------+-------+
  -- | m   h | m   h | m   h |
  -- | n   j | n   j | n   j |
  -- | e   k | e   k | e   k |
  -- | i   l | i   l | i   l |
  --   ^   ^- [A]vaiable
  --   ^----- need [R]emap or should `<nop>`
  -- Notes:
  -- `o` is needed for movement keys so `dn`, `dm` etc work.
  -- adds m, n, e, i to need remap
  -- adds h, j, k, l to avaiable

  -- remap "inner" text objects to new mnemonic "kernel"
  -- `viw`, `ciw` becomes `vkw`, `ckw` etc.
  { lhs = 'k', rhs = 'i', desc = 'Inner (Kernel)', modes = xo, alias = true },

  -- |Norm n | Vis x | Opr o |
  -- | R   A | R   A | R   A |
  -- +-------+-------+-------+
  -- | m   h | m   h | m   h |
  -- | n   j | n   j | n   j |
  -- | e   k | e     | e     |
  -- | i   l |     l |     l |
  -- Notes:
  -- k no longer avaiable in x o
  -- i remapped in x o

  -- -- line navigation
  { lhs = 'M', rhs = '0', desc = 'Insert Mode', modes = nxo, alias = true },
  { lhs = 'I', rhs = '$', desc = 'Insert Mode', modes = nxo, alias = true },

  -- -- scrolling
  { lhs = 'N', rhs = '10j', desc = 'Insert Mode', modes = nxo, alias = true },
  { lhs = 'E', rhs = '10k', desc = 'Insert Mode', modes = nxo, alias = true },
  { lhs = '<C-S-n>', rhs = '<C-D>', desc = 'Insert Mode', modes = nxo, alias = true },
  { lhs = '<C-S-e>', rhs = '<C-U>', desc = 'Insert Mode', modes = nxo, alias = true },

  -- |Norm n | Vis x | Opr o |
  -- | R   A | R   A | R   A |
  -- +-------+-------+-------+
  -- | m   h | m   h | m   h |
  -- | n   j | n   j | n   j |
  -- | e   k | e     | e     |
  -- | i   l |     l |     l |
  -- | M     | M     | M     |
  -- | N     | N     | N     |
  -- | E     | E     | E     |
  -- | I     | I     | I     |
  -- Notes:
  -- adds keys to remap:
  -- - M: cursor to middle line of screen
  -- - N: repeat the latest '/' or '?' N times in opposite direction
  -- - E: cursor forward to the end of WORD N
  -- - I: insert text before the first CHAR on the line N times

  -- Go into insert mode
  { lhs = 'l', rhs = 'i', desc = 'Insert Mode', modes = n },
  { lhs = 'L', rhs = 'I', desc = 'Insert at BOL', modes = n },

  -- |Norm n | Vis x | Opr o |
  -- | R   A | R   A | R   A |
  -- +-------+-------+-------+
  -- | m   h | m   h | m   h |
  -- | n   j | n   j | n   j |
  -- | e   k | e     | e     |
  -- |       |     l |     l |
  -- | M     | M     | M     |
  -- | N     | N     | N     |
  -- | E     | E     | E     |
  -- |       | I     | I     |
  -- | L     |       |       |
  -- Notes:
  -- take l from avaiable in n
  -- add L to remap for n
  -- L: cursor to line N from bottom of screen
  -- remove i / I from need remap in n

  -- End of / Far word movements
  { lhs = 'f', rhs = 'e', desc = 'Far word forward', modes = nxo },
  { lhs = 'F', rhs = 'E', desc = 'Far WORD forward', modes = nxo },

  -- Find / Seek
  { lhs = 's', rhs = 'f', desc = 'Seek', modes = nxo },
  { lhs = 'S', rhs = 'F', desc = 'Seek', modes = nxo },

  -- |Norm n | Vis x | Opr o |
  -- | R   A | R   A | R   A |
  -- +-------+-------+-------+
  -- | m   h | m   h | m   h |
  -- | n   j | n   j | n   j |
  -- |     k |       |       |
  -- |       |     l |     l |
  -- | M     | M     | M     |
  -- | N     | N     | N     |
  -- |       | I     | I     |
  -- | L     |       |       |
  -- Notes:
  -- f / F get remapped immediately to s / S for n x o
  -- remove e / E from need remap for n x o
  -- NOTE: not adding s / S to need to remap because I don't use this functionailty:
  -- S: delete N lines [into register x] and start insert; synonym for "cc".
  -- s: (substitute) delete N characters [into register x] and start insert

  -- Search `/` `?` navigation
  { lhs = 'h', rhs = 'n', desc = 'Next search result', modes = nxo },
  { lhs = 'H', rhs = 'N', desc = 'Prev search result', modes = nxo },

  -- |Norm n | Vis x | Opr o |
  -- | R   A | R   A | R   A |
  -- +-------+-------+-------+
  -- | m     | m     | m     |
  -- |     j |     j |     j |
  -- |     k |       |       |
  -- |       |     l |     l |
  -- | M     | M     | M     |
  -- |       | I     | I     |
  -- | L     |       |       |
  -- | H     | H     | H     |
  -- Notes:
  -- remove n / N from need remap
  -- remove h from avaiable
  -- add H to need remap
  -- H: cursor to line N from top of screen

  -- Visual / Range
  -- NOTE: need `o` mode so `:h operator-pending-index` force operators work.
  { lhs = 'r', rhs = 'v', desc = 'Range Mode', modes = nxo },
  { lhs = 'R', rhs = 'V', desc = 'Range line mode', modes = nxo },
  { lhs = '<C-R>', rhs = '<C-V>', desc = 'Range blockwise mode', modes = nxo },

  -- Replace / Revise
  -- NOTE: R (rhs) does actually have an effect in visual mode
  -- r: replace N chars with {char}
  -- R: enter replacing mode
  { lhs = 'v', rhs = 'r', desc = 'Revise char', modes = nx },
  { lhs = 'V', rhs = 'R', desc = 'Revise Mode', modes = nx },

  -- Marks
  -- NOTE: this differs from vim being able to set marks in visual-mode
  { lhs = 'k', rhs = 'm', desc = 'Keep mark', modes = n },

  -- |Norm n | Vis x | Opr o |
  -- | R   A | R   A | R   A |
  -- +-------+-------+-------+
  -- |       | m     | m     |
  -- |     j |     j |     j |
  -- |       |       |       |
  -- |       |     l |     l |
  -- | M     | M     | M     |
  -- |       | I     | I     |
  -- | L     |       |       |
  -- | H     | H     | H     |
  -- Notes:
  -- immediately swap r / R with v / V
  -- remove m from n
  -- take k from avaiable in n
  -- Final notes:
  -- m: was never a operator-pending keybind so it doesn't need rebinding.
  -- m: giving up ability to set marks in visual-mode (I never need this)
  -- H, M, L are all keys that move the cursor in the window. I don't use these
  -- so will not rebind them.

  -- New mnei bindings for window navigation
  { lhs = '<C-w>m', rhs = '<C-w>h', esc = 'Go to the Left Window', modes = n },
  { lhs = '<C-w><C-m>', rhs = '<C-w>h', desc = 'Go to the Left Window', modes = n },
  { lhs = '<leader>wm', rhs = '<C-w>h', desc = 'Go to the Left Window', modes = n, alias = true },

  { lhs = '<C-w>n', rhs = '<C-w>j', desc = 'Go to the Bottom Window', modes = n },
  { lhs = '<C-w><C-n>', rhs = '<C-w>j', desc = 'Go to the Bottom Window', modes = n },
  { lhs = '<leader>wn', rhs = '<C-w>j', desc = 'Go to the Bottom Window', modes = n, alias = true },

  { lhs = '<C-w>e', rhs = '<C-w>k', desc = 'Go to the Top Window', modes = n },
  { lhs = '<C-w><C-e>', rhs = '<C-w>k', desc = 'Go to the Top Window', modes = n },
  { lhs = '<leader>we', rhs = '<C-w>k', desc = 'Go to the Top Window', modes = n, alias = true },

  { lhs = '<C-w>i', rhs = '<C-w>l', desc = 'Go to the Right Window', modes = n },
  { lhs = '<C-w><C-i>', rhs = '<C-w>l', desc = 'Go to the Right Window', modes = n },
  { lhs = '<leader>wi', rhs = '<C-w>l', desc = 'Go to the Right Window', modes = n, alias = true },

  { lhs = '<C-w>M', rhs = '<C-w>H', desc = 'Move window far left', modes = n },
  { lhs = '<leader>wM', rhs = '<C-w>H', desc = 'Move window far left', modes = n, alias = true },

  { lhs = '<C-w>N', rhs = '<C-w>J', desc = 'Move window to bottom', modes = n },
  { lhs = '<leader>wN', rhs = '<C-w>J', desc = 'Move window to bottom', modes = n, alias = true },

  { lhs = '<C-w>E', rhs = '<C-w>K', desc = 'Move window to top', modes = n },
  { lhs = '<leader>wE', rhs = '<C-w>K', desc = 'Move window to top', modes = n, alias = true },

  { lhs = '<C-w>I', rhs = '<C-w>L', desc = 'Move window far right', modes = n },
  { lhs = '<leader>wI', rhs = '<C-w>L', desc = 'Move window far right', modes = n, alias = true },

  { lhs = '<C-w>k', rhs = '<C-w>c', desc = 'Kill window', modes = n },
  { lhs = '<C-w><C-k>', rhs = '<C-w>c', desc = 'Kill window', modes = n },
  { lhs = '<leader>wk', rhs = '<C-w>c', desc = 'Kill window', modes = n, alias = true },

  -- rebind i -> j and n -> c
  { lhs = '<C-w>j', rhs = '<C-w>i', desc = 'Split + jump to declaration', modes = n },
  { lhs = '<C-w>c', rhs = '<C-w>n', desc = 'Create window', modes = n },
  { lhs = '<C-w><C-c>', rhs = '<C-w>n', desc = 'Create window', modes = n },
  { lhs = '<leader>wc', rhs = '<C-w>n', desc = 'Create window', modes = n, alias = true },

  -- Add some nice-to-haves on leader, others I'll probably go through `C-w`
  { lhs = '<leader>wv', rhs = '<C-w>v', desc = 'Split vertically', modes = n, alias = true },
  { lhs = '<leader>ws', rhs = '<C-w>s', desc = 'Split horizontally', modes = n, alias = true },
}

-- Nuke the site from orbit. It's the only way to be sure.
for _, ks in ipairs(keymaps) do
  if ks.alias ~= true then
    vim.keymap.set(ks.modes, ks.rhs, '<nop>')
  end
end

for _, ks in ipairs(keymaps) do
  local opts = { desc = ks.desc }
  -- Merge any extra options
  if ks.opts then
    opts = vim.tbl_extend('force', opts, ks.opts)
  end
  vim.keymap.set(ks.modes, ks.lhs, ks.rhs, opts)
end

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
