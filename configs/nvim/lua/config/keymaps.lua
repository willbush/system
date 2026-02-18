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

local function yank_current_file_path(register)
  local path = vim.fn.expand('%:p')
  if path == '' then
    vim.notify('No file path for current buffer', vim.log.levels.WARN)
    return
  end

  local target_register = register or '"'
  vim.fn.setreg(target_register, path)
  vim.notify(('Yanked path (%s): %s'):format(target_register, path))
end

set_keymaps({
  -- Normal Mode
  n = {
    -- Global keys:

    -- Note this maps over default `U` (see `:h U`) which "undo all latest
    -- changes on one line." I fail to imagine ever wanting that.
    ['U'] = { '<C-R>', desc = 'Redo' },
    ['<Esc>'] = { '<cmd>nohlsearch<cr>', desc = 'Clear Search Highlights' },
    ['<Tab>'] = { '<cmd>bnext<cr>', desc = 'Next buffer' },
    ['<S-Tab>'] = { '<cmd>bprev<cr>', desc = 'Prev buffer' },

    -- Leader keys:

    -- Rapid (emphasize easy access over mnemonics)
    ['<leader>rs'] = { '<cmd>update<cr>', desc = 'Save Buffer' },

    -- Buffers
    ['<leader>bb'] = {
      function()
        require('fzf-lua').buffers()
      end,
      desc = 'Buffer Picker',
    },
    ['<leader>ba'] = {
      function()
        require('snacks').bufdelete.all()
      end,
      desc = 'Delete all buffers',
    },
    ['<leader>bA'] = {
      function()
        require('snacks').bufdelete.all({ force = true })
      end,
      desc = 'Force delete all buffers',
    },
    ['<leader>bd'] = {
      function()
        require('snacks').bufdelete()
      end,
      desc = 'Delete Buffer',
    },
    ['<leader>bD'] = {
      function()
        require('snacks').bufdelete({ force = true })
      end,
      desc = 'Force delete buffer',
    },
    ['<leader>bO'] = {
      function()
        require('snacks').bufdelete.other()
      end,
      desc = 'Delete Other Buffers',
    },
    ['<leader>bk'] = { '<cmd>bd<cr>', desc = 'Kill (delete) Buffer and Window' },
    ['<leader>bn'] = { '<cmd>enew<cr>', desc = 'New Buffer' },
    ['<leader>bs'] = { '<cmd>edit /tmp/scratch<cr>', desc = 'Edit the scratch buffer' },

    -- Explore
    ['<leader>e'] = {
      function()
        require('oil').open()
      end,
      desc = 'Explore Directory with Oil',
    },
    ['<leader>E'] = {
      function()
        require('oil').open(vim.fn.getcwd())
      end,
      desc = 'Open CWD in Oil',
    },
    -- Files
    ['<leader>ff'] = {
      function()
        require('fzf-lua').files()
      end,
      desc = 'Find Files',
    },
    ['<leader>fo'] = {
      function()
        require('fzf-lua').oldfiles()
      end,
      desc = 'Find Old Files (History)',
    },
    ['<leader>fy'] = {
      function()
        yank_current_file_path('"')
      end,
      desc = 'Yank Absolute File Path',
    },
    ['<leader>fY'] = {
      function()
        yank_current_file_path('+')
      end,
      desc = 'Yank Absolute File Path to Clipboard',
    },
    -- Search
    ['<leader>sl'] = {
      function()
        require('fzf-lua').lines()
      end,
      desc = 'Search Lines in All Buffers',
    },
    ['<leader>sL'] = {
      function()
        require('fzf-lua').blines()
      end,
      desc = 'Search Lines in Current Buffer',
    },
    ['<leader>sw'] = {
      function()
        require('fzf-lua').grep_cword()
      end,
      desc = 'Search Word under cursor',
    },
    ['<leader>sW'] = {
      function()
        require('fzf-lua').grep_cWORD()
      end,
      desc = 'Search WORD under cursor',
    },
    ['<leader>sr'] = {
      function()
        require('fzf-lua').resume()
      end,
      desc = 'Resume last FZF search',
    },
    ['<leader>sm'] = {
      function()
        require('fzf-lua').marks()
      end,
      desc = 'Search Marks',
    },
    ['<leader>sj'] = {
      function()
        require('fzf-lua').jumps()
      end,
      desc = 'Search Jumps',
    },
    ['<leader>sh'] = {
      function()
        require('fzf-lua').helptags()
      end,
      desc = 'Search Help Tags',
    },

    -- Project
    ['<leader>pp'] = {
      function()
        require('fzf-lua').zoxide({
          actions = {
            enter = function(selected)
              if not selected[1] then
                return
              end
              local path = selected[1]:match('[^\t]+$') or selected[1]
              require('oil').open(path)
            end,
          },
        })
      end,
      desc = 'Zoxide peek dir',
    },
    ['<leader>pt'] = {
      function()
        require('fzf-lua').zoxide()
      end,
      desc = 'Zoxide travel',
    },

    -- TODO: not sure if I want to have all or most LSP key binds here or under goto.
    ['<leader>pd'] = {
      function()
        require('fzf-lua').diagnostics_workspace()
      end,
      desc = 'Diagnostics (Workspace)',
    },

    ['<leader>/'] = {
      function()
        require('fzf-lua').live_grep()
      end,
      desc = 'Live grep',
    },

    -- Quit
    ['<leader>qq'] = { '<cmd>wqa<cr>', desc = 'Write and Quit All' },

    -- Yank / Paste
    ['<leader>xy'] = { '"+y', desc = 'Yank to system clipboard' },
    ['<leader>xp'] = { '"+p', desc = 'Paste from system clipboard' },
    ['<leader>xP'] = { '"+P', desc = 'Paste before from system clipboard' },
  },

  -- Insert Mode
  i = {
    -- Emacs-like keybindings for line navigation
    ['<C-a>'] = { '<Home>', desc = 'Beginning of line' },
    ['<C-e>'] = { '<End>', desc = 'End of line' },
  },

  -- Command-line Mode
  c = {
    ['<C-a>'] = { '<Home>', desc = 'Beginning of line' },
    ['<C-e>'] = { '<End>', desc = 'End of line' },
  },

  -- Visual Mode
  x = {
    -- Yank / Paste
    ['<leader>xy'] = { '"+y', desc = 'Yank selection to system clipboard' },
    ['<leader>xp'] = { '"+p', desc = 'Paste from system clipboard' },
  },

  -- Terminal Mode
  t = {
    -- Exit terminal mode in the builtin terminal with a shortcut that is a bit
    -- easier for people to discover. Otherwise, you normally need to press
    -- <C-\><C-n>, which is not what someone will guess without a bit more
    -- experience.
    ['<Esc><Esc>'] = { '<C-\\><C-n>', desc = 'Exit Terminal Mode' },
  },
})
