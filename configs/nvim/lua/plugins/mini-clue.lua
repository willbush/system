return {
  {
    'nvim-mini/mini.clue',
    version = false,
    config = function()
      local gen_clues = require('mini.clue').gen_clues

      local function custom_window_clues()
        local window_clues = gen_clues.windows({
          submode_resize = true,
          submode_navigate = true,
          submode_move = true,
        })

        local remapped_keys = {
          -- rebind hjkl -> mnei
          ['<C-w>h'] = true,
          ['<C-w>j'] = true,
          ['<C-w>k'] = true,
          ['<C-w>l'] = true,
          -- rebind HJKL -> MNEI
          ['<C-w>H'] = true,
          ['<C-w>J'] = true,
          ['<C-w>K'] = true,
          ['<C-w>L'] = true,
          -- rebind i -> j and n -> c
          ['<C-w>c'] = true,
          ['<C-w>i'] = true,
          ['<C-w>n'] = true,
        }

        local custom_clues = {}

        for _, clue in ipairs(window_clues) do
          if not remapped_keys[clue.keys] then
            table.insert(custom_clues, clue)
          end
        end

        return custom_clues
      end

      require('mini.clue').setup({
        triggers = {
          -- Leader triggers
          { mode = 'n', keys = '<Leader>' },
          { mode = 'x', keys = '<Leader>' },

          -- `[` and `]` keys
          { mode = 'n', keys = '[' },
          { mode = 'n', keys = ']' },

          -- Built-in completion
          { mode = 'i', keys = '<C-x>' },

          -- `g` key
          { mode = 'n', keys = 'g' },
          { mode = 'x', keys = 'g' },

          -- Marks
          { mode = 'n', keys = "'" },
          { mode = 'n', keys = '`' },
          { mode = 'x', keys = "'" },
          { mode = 'x', keys = '`' },

          -- Registers
          { mode = 'n', keys = '"' },
          { mode = 'x', keys = '"' },
          { mode = 'i', keys = '<C-r>' },
          { mode = 'c', keys = '<C-r>' },

          -- Window commands
          { mode = 'n', keys = '<C-w>' },

          -- `z` key
          { mode = 'n', keys = 'z' },
          { mode = 'x', keys = 'z' },
        },

        clues = {
          gen_clues.square_brackets(),
          gen_clues.builtin_completion(),
          gen_clues.g(),
          gen_clues.marks(),
          gen_clues.registers(),
          gen_clues.z(),
          custom_window_clues(),

          { mode = 'n', keys = '<leader>b', desc = '+Buffers' },
          { mode = 'n', keys = '<leader>f', desc = '+Files' },
          { mode = 'n', keys = '<leader>p', desc = '+Project' },
          { mode = 'n', keys = '<leader>q', desc = '+Quit' },
          { mode = 'n', keys = '<leader>r', desc = '+Rapid' },
          { mode = 'n', keys = '<leader>s', desc = '+Search' },
          { mode = 'n', keys = '<leader>w', desc = '+Window' },

          { mode = 'n', keys = '<C-w>', desc = '+Window' },
        },
        window = {
          delay = 0,
          config = {
            width = 125,
            border = 'rounded',
          },
        },
      })
    end,
  },
}
