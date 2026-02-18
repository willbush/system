{ ... }:
{
  programs.yazi = {
    enable = true;
    # Adds https://yazi-rs.github.io/docs/quick-start#shell-wrapper as `y` command
    enableFishIntegration = true;
  };

  home.file = {
    ".config/yazi/keymap.toml" = {
      source = ../../configs/yazi/keymap.toml;
    };
  };
}
