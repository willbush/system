return {
  {
    'ibhagwan/fzf-lua',
    dependencies = { 'nvim-mini/mini.icons' },
    cmd = 'FzfLua',
    config = function()
      local fzf_lua = require('fzf-lua')
      fzf_lua.setup({
        'skim', -- requires `sk` binary
      })
      fzf_lua.register_ui_select()
    end,
  },
}
