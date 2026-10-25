{ ... }:
{
  programs.yazi.enable = true;

  home.file = {
    ".config/yazi/keymap.toml" = {
      source = ../../configs/yazi/keymap.toml;
    };
  };
}
