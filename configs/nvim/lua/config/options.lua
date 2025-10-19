-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.g.have_nerd_font = true

-- [[ Setting options ]]
--  For more options `:help option-list`

vim.o.number = true -- Make line numbers default
vim.o.relativenumber = true

vim.o.jumpoptions = 'stack'

vim.o.mouse = 'a' -- Enable mouse mode

vim.o.showmode = false -- disable showing modes in command line
vim.o.termguicolors = true -- enable 24-bit RGB color in the TUI

-- Sync clipboard between OS and Neovim.
--  Schedule the setting after `UiEnter` because it can increase startup-time.
--  See `:help 'clipboard'`
vim.schedule(function()
  vim.o.clipboard = 'unnamedplus'
end)

vim.o.undofile = true -- enable persistent undo

vim.o.ignorecase = true -- case insensitive searching
vim.o.smartcase = true -- case sensitive searching

vim.o.signcolumn = 'yes' -- Keep signcolumn on by default

vim.o.updatetime = 250 -- Decrease update time
vim.o.timeoutlen = 300 -- Decrease mapped sequence wait time

-- Sets how neovim will display certain whitespace characters in the editor.
--  See `:help 'list'`
--  and `:help 'listchars'`
vim.o.list = true
vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }

vim.o.inccommand = 'split' -- Preview substitutions live when typing!
vim.o.cursorline = true -- Show which line your cursor is on
vim.o.scrolloff = 10 -- Minimal number of screen lines to keep above and below the cursor.
vim.o.confirm = true -- raise a dialog asking if you wish to save the current file(s)

vim.o.shiftwidth = 0 -- number of space inserted for indentation; when zero the 'tabstop' value will be used
vim.opt.tabstop = 2 -- number of space in a tab
vim.o.expandtab = true -- enable the use of space in tab

vim.o.wrap = false -- disable wrapping of lines longer than the width of window
vim.o.breakindent = true -- wrap indent to match line start

vim.diagnostic.config({
  virtual_text = true,
  severity_sort = true,

  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = '',
      [vim.diagnostic.severity.WARN] = '',
      [vim.diagnostic.severity.INFO] = '',
      [vim.diagnostic.severity.HINT] = '',
    },
  },

  float = {
    source = true,
    border = 'rounded',
    header = '',
    prefix = '',
  },

  jump = {
    float = true,
  },
})
