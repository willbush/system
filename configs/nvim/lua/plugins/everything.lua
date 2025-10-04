return {
  {
    -- `lazydev` configures Lua LSP for your Neovim config, runtime and plugins
    -- used for completion, annotations and signatures of Neovim apis
    'folke/lazydev.nvim',
    ft = 'lua', -- only load on lua files
    opts = {
      library = {
        -- Load luvit types when the `vim.uv` word is found
        { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
      },
    },
  },
  {
    'neovim/nvim-lspconfig',
    config = function()
      vim.lsp.enable('rust-analyzer')
      vim.lsp.enable('lua-language-server')
    end,
  },
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.8',
    dependencies = { 'nvim-lua/plenary.nvim' },
  },
  {
    'ibhagwan/fzf-lua',
    cmd = 'FzfLua',

    config = function()
      -- Use skim profile
      require('fzf-lua').setup({ 'skim' })

      -- Add zoxide integration
      local function zoxide_list()
        require('fzf-lua').fzf_exec('zoxide query -l', {
          actions = {
            ['default'] = function(selected)
              vim.cmd('cd ' .. selected[1])
            end,
          },
          prompt = 'Zoxide> ',
        })
      end

      -- Create command and keymap
      vim.api.nvim_create_user_command('Z', zoxide_list, {})
      vim.keymap.set('n', '<leader>z', zoxide_list, { desc = 'Zoxide Jump' })
    end,

    keys = {
      -- { "<c-j>", "<c-j>", ft = "fzf", mode = "t", nowait = true },
      -- { "<c-k>", "<c-k>", ft = "fzf", mode = "t", nowait = true },
      -- {
      --   "<leader>,",
      --   "<cmd>FzfLua buffers sort_mru=true sort_lastused=true<cr>",
      --   desc = "Switch Buffer",
      -- },
      -- { "<leader>/", LazyVim.pick("live_grep"), desc = "Grep (Root Dir)" },
      -- { "<leader>:", "<cmd>FzfLua command_history<cr>", desc = "Command History" },
      -- { "<leader><space>", LazyVim.pick("files"), desc = "Find Files (Root Dir)" },

      -- -- find
      -- { "<leader>fb", "<cmd>FzfLua buffers sort_mru=true sort_lastused=true<cr>", desc = "Buffers" },
      -- { "<leader>fc", LazyVim.pick.config_files(), desc = "Find Config File" },
      -- { "<leader>ff", LazyVim.pick("files"), desc = "Find Files (Root Dir)" },
      -- { "<leader>fF", LazyVim.pick("files", { root = false }), desc = "Find Files (cwd)" },
      -- { "<leader>fg", "<cmd>FzfLua git_files<cr>", desc = "Find Files (git-files)" },
      -- { "<leader>fr", "<cmd>FzfLua oldfiles<cr>", desc = "Recent" },
      -- { "<leader>fR", LazyVim.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
      {
        '<leader>fb',
        function()
          require('fzf-lua').buffers()
        end,
        desc = 'Buffers',
      },
      {
        '<leader>fc',
        function()
          require('fzf-lua').files({ cwd = vim.fn.stdpath('config') })
        end,
        desc = 'Find Config File',
      },
      {
        '<leader>ff',
        function()
          require('fzf-lua').files()
        end,
        desc = 'Find Files (Root Dir)',
      },
      {
        '<leader>fF',
        function()
          require('fzf-lua').files({ cwd = vim.uv.cwd() })
        end,
        desc = 'Find Files (cwd)',
      },
      {
        '<leader>fg',
        function()
          require('fzf-lua').git_files()
        end,
        desc = 'Find Files (git-files)',
      },
      {
        '<leader>fr',
        function()
          require('fzf-lua').oldfiles()
        end,
        desc = 'Recent',
      },
      {
        '<leader>fR',
        function()
          require('fzf-lua').oldfiles({ cwd = vim.uv.cwd() })
        end,
        desc = 'Recent (cwd)',
      },

      -- -- git
      -- { "<leader>gc", "<cmd>FzfLua git_commits<CR>", desc = "Commits" },
      -- { "<leader>gs", "<cmd>FzfLua git_status<CR>", desc = "Status" },

      -- rapid
      {
        '<leader>rc',
        function()
          require('fzf-lua').colorschemes()
        end,
        desc = 'Colorscheme with Preview',
      },

      -- search
      -- { '<leader>s"', "<cmd>FzfLua registers<cr>", desc = "Registers" },
      { '<leader>sa', '<cmd>FzfLua autocmds<cr>', desc = 'Auto Commands' },
      { '<leader>sb', '<cmd>FzfLua grep_curbuf<cr>', desc = 'Buffer' },
      { '<leader>sc', '<cmd>FzfLua command_history<cr>', desc = 'Command History' },
      { '<leader>sC', '<cmd>FzfLua commands<cr>', desc = 'Commands' },
      { '<leader>sd', '<cmd>FzfLua diagnostics_document<cr>', desc = 'Document Diagnostics' },
      { '<leader>sD', '<cmd>FzfLua diagnostics_workspace<cr>', desc = 'Workspace Diagnostics' },
      -- { "<leader>sg", LazyVim.pick("live_grep"), desc = "Grep (Root Dir)" },
      -- { "<leader>sG", LazyVim.pick("live_grep", { root = false }), desc = "Grep (cwd)" },
      { '<leader>sh', '<cmd>FzfLua help_tags<cr>', desc = 'Help Pages' },
      { '<leader>sH', '<cmd>FzfLua highlights<cr>', desc = 'Search Highlight Groups' },
      { '<leader>sj', '<cmd>FzfLua jumps<cr>', desc = 'Jumplist' },
      { '<leader>sk', '<cmd>FzfLua keymaps<cr>', desc = 'Key Maps' },
      { '<leader>sl', '<cmd>FzfLua loclist<cr>', desc = 'Location List' },
      { '<leader>sM', '<cmd>FzfLua man_pages<cr>', desc = 'Man Pages' },
      { '<leader>sm', '<cmd>FzfLua marks<cr>', desc = 'Jump to Mark' },
      { '<leader>sR', '<cmd>FzfLua resume<cr>', desc = 'Resume' },
      { '<leader>sq', '<cmd>FzfLua quickfix<cr>', desc = 'Quickfix List' },
      {
        '<leader>sw',
        function()
          require('fzf-lua').grep_cword()
        end,
        desc = 'Word (Root Dir)',
      },
      {
        '<leader>sW',
        function()
          require('fzf-lua').grep_cword({ cwd = vim.uv.cwd() })
        end,
        desc = 'Word (cwd)',
      },
      {
        '<leader>sw',
        function()
          require('fzf-lua').grep_visual()
        end,
        mode = 'v',
        desc = 'Selection (Root Dir)',
      },
      {
        '<leader>sW',
        function()
          require('fzf-lua').grep_visual({ cwd = vim.uv.cwd() })
        end,
        mode = 'v',
        desc = 'Selection (cwd)',
      },
      {
        '<leader>ss',
        function()
          require('fzf-lua').lsp_document_symbols({
            regex_filter = symbols_filter,
          })
        end,
        desc = 'Goto Symbol',
      },
      {
        '<leader>sS',
        function()
          require('fzf-lua').lsp_live_workspace_symbols({
            regex_filter = symbols_filter,
          })
        end,
        desc = 'Goto Symbol (Workspace)',
      },
    },
  },
  {
    'folke/snacks.nvim',
    priority = 1000,
    lazy = false,
    opts = {
      bigfile = { enabled = false },
      dashboard = { enabled = false },
      indent = { enabled = false },
      input = { enabled = false },
      notifier = { enabled = false },
      quickfile = { enabled = false },
      scroll = { enabled = false },
      statuscolumn = { enabled = false },
      words = { enabled = false },
    },
  },
  {
    'folke/tokyonight.nvim',
    priority = 1000, -- Make sure to load this before all the other start plugins.
    init = function()
      vim.cmd.colorscheme('tokyonight-night')

      -- You can configure highlights by doing something like:
      vim.cmd.hi('Comment gui=none')
    end,
  },
  -- git signs highlights text that has changed since the list
  -- git commit, and also lets you interactively stage & unstage
  -- hunks in a commit.
  {
    'lewis6991/gitsigns.nvim',
    event = 'VimEnter',
    opts = {},
  },

  { 'echasnovski/mini.icons', lazy = true, version = false },
  { 'echasnovski/mini.statusline', opts = {}, version = false },

  -- { -- Collection of various small independent plugins/modules
  --   'echasnovski/mini.nvim',
  --   config = function()
  --     local statusline = require 'mini.statusline'
  --     statusline.setup { use_icons = vim.g.have_nerd_font }

  --     -- You can configure sections in the statusline by overriding their
  --     -- default behavior. For example, here we set the section for
  --     -- cursor location to LINE:COLUMN
  --     ---@diagnostic disable-next-line: duplicate-set-field
  --     statusline.section_location = function()
  --       return '%2l:%-2v'
  --     end

  --     -- ... and there is more!
  --     --  Check out: https://github.com/echasnovski/mini.nvim
  --   end,
  -- },

  {
    'nvim-mini/mini.nvim',
    version = '*',
    config = function()
      -- You can configure other mini modules here as well

      -- Set up mini.clue
      require('mini.clue').setup({
        -- Must be set to true or a table with options
        triggers = {
          -- Leader triggers
          { mode = 'n', keys = '<Leader>' },
          { mode = 'x', keys = '<Leader>' },

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
          -- Enhance this by adding descriptions for <Leader> mapping groups
          require('mini.clue').gen_clues.builtin_completion(),
          require('mini.clue').gen_clues.g(),
          require('mini.clue').gen_clues.marks(),
          require('mini.clue').gen_clues.registers(),
          require('mini.clue').gen_clues.z(),
        },
      })
    end,
  },
  -- {
  --   'folke/which-key.nvim',
  --   event = 'VeryLazy',
  --   opts = {
  --
  --     plugins = {
  --       presets = {
  --         -- disable the built-in window commands since I remap many of them. It
  --         -- will show the hjkl mappings even if I filter them out.
  --         windows = false,
  --       },
  --     },
  --
  --     -- Exclude mappings with no description
  --     filter = function(mapping)
  --       return mapping.desc and mapping.desc ~= ''
  --     end,
  --
  --     icons = {
  --       -- set icon mappings to true if you have a Nerd Font
  --       mappings = vim.g.have_nerd_font,
  --     },
  --     -- Document existing key chains
  --     spec = {
  --       {
  --         mode = { 'n', 'v' },
  --         -- { "<leader><tab>", group = "tabs" },
  --         -- { "<leader>c", group = "code" },
  --         -- { "<leader>d", group = "debug" },
  --         -- { "<leader>dp", group = "profiler" },
  --         -- { "<leader>g", group = "git" },
  --         -- { "<leader>gh", group = "hunks" },
  --         -- { "<leader>q", group = "quit/session" },
  --         -- { "<leader>u", group = "ui", icon = { icon = "󰙵 ", color = "cyan" } },
  --         -- { "<leader>x", group = "diagnostics/quickfix", icon = { icon = "󱖫 ", color = "green" } },
  --         -- { "gs", group = "surround" },
  --         { '<leader>f', group = 'file' },
  --         { '<leader>q', group = 'quit' },
  --         { '<leader>r', group = 'rapid' },
  --         { '<leader>s', group = 'search' },
  --         { '<leader>w', proxy = '<c-w>', group = 'windows' }, -- proxy to window mappings
  --         { '[', group = 'prev' },
  --         { ']', group = 'next' },
  --         { 'g', group = 'goto' },
  --         { 'z', group = 'fold' },
  --         { '<leader>b', group = 'buffer' },
  --       },
  --     },
  --   },
  --   keys = {
  --     {
  --       '<leader>?',
  --       function()
  --         require('which-key').show({ global = false })
  --       end,
  --       desc = 'Buffer Local Keymaps (which-key)',
  --     },
  --   },
  -- },
  { -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    main = 'nvim-treesitter.configs', -- Sets main module to use for opts
    -- [[ Configure Treesitter ]] See `:help nvim-treesitter`
    opts = {
      ensure_installed = {
        'bash',
        'c',
        'diff',
        'html',
        'lua',
        'luadoc',
        'markdown',
        'markdown_inline',
        'query',
        'ron',
        'rust',
        'vim',
        'vimdoc',
      },
      -- Autoinstall languages that are not installed
      auto_install = true,
      highlight = {
        enable = true,
      },
    },
  },
}
