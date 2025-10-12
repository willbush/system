local maps = {
  -- Normal Mode
  n = {
    -- Global keys:

    -- Note this maps over default `U` (see `:h U`) which "undo all latest
    -- changes on one line." I fail to imagine ever wanting that.
    ['U'] = { '<C-R>', desc = 'Redo' },
    ['<Esc>'] = { '<cmd>nohlsearch<cr>', desc = 'Clear Search Highlights' },

    -- Leader keys:

    -- Rapid (emphasize easy access over mnemonics)
    ['<leader>rs'] = { '<cmd>w<cr>', desc = 'Save Buffer' },

    -- Buffers
    ['<leader>bb'] = {
      function()
        require('fzf-lua').buffers()
      end,
      desc = 'Buffer Picker',
    },
    ['<leader>bd'] = {
      function()
        require('snacks').bufdelete()
      end,
      desc = 'Delete Buffer',
    },
    ['<leader>bk'] = { '<cmd>bd<cr>', desc = 'Kill (delete) Buffer and Window' },
    ['<leader>bD'] = {
      function()
        require('snacks').bufdelete.all()
      end,
      desc = 'Delete All Buffers',
    },
    ['<leader>bO'] = {
      function()
        require('snacks').bufdelete.other()
      end,
      desc = 'Delete Other Buffers',
    },

    -- Files
    ['<leader>ff'] = {
      function()
        require('fzf-lua').files()
      end,
      desc = 'Find Files',
    },
    ['<leader>fl'] = {
      function()
        require('fzf-lua').lines()
      end,
      desc = 'Find Lines in All Buffers',
    },
    ['<leader>fL'] = {
      function()
        require('fzf-lua').blines()
      end,
      desc = 'Find Lines in Current Buffer',
    },

    ['<leader>/'] = {
      function()
        require('fzf-lua').live_grep()
      end,
      desc = 'Live grep',
    },

    -- Quit
    ['<leader>qq'] = { '<cmd>wqa<cr>', desc = 'Write and Quit All' },
  },

  -- Terminal Mode
  t = {
    -- Exit terminal mode in the builtin terminal with a shortcut that is a bit
    -- easier for people to discover. Otherwise, you normally need to press
    -- <C-\><C-n>, which is not what someone will guess without a bit more
    -- experience.
    ['<Esc><Esc>'] = { '<C-\\><C-n>', desc = 'Exit Terminal Mode' },
  },
}

local function set_keymaps(maps)
  for mode, mode_maps in pairs(maps) do
    for lhs, definition in pairs(mode_maps) do
      local rhs, opts

      if type(definition) == 'table' then
        rhs = definition[1]
        opts = {}
        for key, value in pairs(definition) do
          if type(key) == 'string' then -- Only copy named keys into opts
            opts[key] = value
          end
        end
      else
        -- mappings like: ['j'] = 'gj'
        rhs = definition
        opts = {}
      end

      vim.keymap.set(mode, lhs, rhs, opts)
    end
  end
end

set_keymaps(maps)
