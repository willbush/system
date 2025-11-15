return {
  {
    'MeanderingProgrammer/render-markdown.nvim',
    ft = 'markdown',
    dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-mini/mini.icons' },
    ---@module 'render-markdown'
    ---@type render.md.UserConfig
    opts = {},
  },
  {
    'yousefhadder/markdown-plus.nvim',
    ft = 'markdown',
    opts = {
      enabled = true,
      features = {
        list_management = true,
        text_formatting = true,
        headers_toc = true,
        links = true,
        quotes = true,
        code_block = true,
        table = true,
      },
      keymaps = {
        enabled = true, -- Enable default keymaps (<Plug> available for custom)
      },
      toc = { -- TOC window configuration
        initial_depth = 2,
      },
      table = {
        auto_format = true,
        default_alignment = 'left',
        confirm_destructive = false,
        keymaps = {
          enabled = true,
          prefix = '<leader>t',
          insert_mode_navigation = false,
        },
      },
      filetypes = { 'markdown' }, -- Filetypes to enable the plugin for
    },
  },
}
