return {
  {
    'ibhagwan/fzf-lua',
    dependencies = { 'nvim-mini/mini.icons' },
    cmd = 'FzfLua',
    opts = {
      'skim', -- requires `sk` binary
    },
    config = function()
      require('fzf-lua').register_ui_select()
    end,
  },
}
