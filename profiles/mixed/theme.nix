{ config, pkgs, ... }:
{
  stylix = {
    enable = true;
    # NB: when switching themes make sure new theme doesn't break difftastic colors
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    image = pkgs.fetchurl {
      # https://www.reddit.com/r/WidescreenWallpaper/comments/1dzli4w/untitled_7680x2160/
      url = "https://i.redd.it/1k3jwtm7zlbd1.jpeg";
      hash = "sha256-3GJ6pwrExTGJ5y+X//7iBY2ABDUkcFlxQzot24evceo=";
    };

    fonts = {
      sizes = {
        applications = 14; # default: 12
        desktop = 12; # default: 10
        popups = 12; # default: 10
        terminal = 14; # default: 12
      };
      # Change the default momonospace font
      monospace = {
        package = pkgs.nerd-fonts.zed-mono;
        name = "ZedMono NFM Extd";
      };
    };
  };

  home-manager.users.${config.user.name} = {
    home.pointerCursor = {
      enable = true;
      gtk.enable = true;
      name = "BreezeX-RosePine-Linux";
      package = pkgs.rose-pine-cursor;
      size = 32;
    };
  };
}
