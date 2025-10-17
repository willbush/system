return {
  'dmtrKovalenko/fff.nvim',
  build = 'nix run .#release',
  opts = {},
  -- This plugin initializes itself lazily.
  lazy = false,
}
