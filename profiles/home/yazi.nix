{ ... }:
{
  programs.yazi = {
    enable = true;
    # Adds https://yazi-rs.github.io/docs/quick-start#shell-wrapper as `yy` command
    enableFishIntegration = true;
  };

  home.file = {
    ".config/yazi/keymap.toml" = {
      source = ../../configs/yazi/keymap.toml;
    };
  };
}
