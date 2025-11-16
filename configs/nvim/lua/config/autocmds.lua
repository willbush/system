local function augroup(name)
  return vim.api.nvim_create_augroup('my_' .. name, { clear = true })
end

-- Highlight on yank
vim.api.nvim_set_hl(0, 'YankHighlight', { bg = '#9e38a4', fg = '#000000' })
vim.api.nvim_create_autocmd('TextYankPost', {
  group = augroup('highlight_yank'),
  callback = function()
    vim.hl.on_yank({
      higroup = 'YankHighlight',
      timeout = 100,
    })
  end,
})

-- resize splits if window got resized
vim.api.nvim_create_autocmd({ 'VimResized' }, {
  group = augroup('resize_splits'),
  callback = function()
    local current_tab = vim.fn.tabpagenr()
    vim.cmd('tabdo wincmd =')
    vim.cmd('tabnext ' .. current_tab)
  end,
})

-- Move :help commands into vsplit to far right
vim.api.nvim_create_autocmd('FileType', {
  group = augroup('vertical_help'),
  pattern = 'help',
  callback = function()
    vim.cmd('wincmd L')
  end,
  desc = 'Open :help in vertical split',
})

vim.api.nvim_create_autocmd('LspAttach', {
  group = augroup('lsp_keymaps'),
  callback = function(ev)
    local bufnr = ev.buf
    local function map(mode, lhs, rhs, desc)
      vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, silent = true, desc = desc })
    end

    map('n', 'gd', vim.lsp.buf.definition, 'Go to definition')
    map('n', 'gy', vim.lsp.buf.type_definition, 'Go to type definition')

    -- This (`gra`) is not really needed as fzf-lua UI works for normal code
    -- actions, but not other builtin bindings for some reason so I rather do
    -- this explicitly.
    map('n', 'gra', function()
      require('fzf-lua').lsp_code_actions()
    end, 'LSP Code Actions')

    map('n', 'grd', function()
      require('fzf-lua').diagnostics_workspace()
    end, 'LSP Definitions')

    map('n', 'grD', function()
      require('fzf-lua').lsp_declarations()
    end, 'LSP Declarations')

    map('n', 'grr', function()
      require('fzf-lua').lsp_references()
    end, 'LSP References')

    map('n', 'gri', function()
      require('fzf-lua').lsp_implementations()
    end, 'LSP Implementations')

    map('n', 'grs', function()
      require('fzf-lua').lsp_document_symbols()
    end, 'LSP Symbols (Document)')

    map('n', 'grS', function()
      require('fzf-lua').lsp_live_workspace_symbols()
    end, 'LSP Symbols (Workspace)')

    -- NOTE: not binding the following on purpose:
    -- - `lsp_incoming_calls` like lsp_references but less generic (doesn't
    --    work for types). Only upside is for doesn't include function definition
    --    in results?
    --
    -- - `lsp_outgoing_calls`: don't see the point of this and it seems buggy
    -- - `lsp_finder`: combines all the different things into one bringing in issue above.
    -- - `lsp_typedefs`: can't figure out how this is different than builtin
  end,
})
