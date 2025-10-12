return {
  {
    'ibhagwan/fzf-lua',
    dependencies = { 'nvim-mini/mini.icons' },
    cmd = 'FzfLua',
    opts = {
      'skim', -- requires `sk` binary
    },
  },
}
