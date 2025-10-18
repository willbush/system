return {
  'dmtrKovalenko/fff.nvim',
  build = 'nix run .#release',
  opts = {
    prompt = '❯ ',
    max_threads = 8,
    layout = {
      prompt_position = 'top',
    },
  },
  -- This plugin initializes itself lazily.
  lazy = false,
}
