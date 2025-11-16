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
