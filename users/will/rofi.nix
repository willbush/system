{ pkgs, ... }: {

  programs.rofi = {
    enable = true;
    theme = "Arc-Dark";
    pass.enable = true;
    font = "Hack Nerd Font Mono 12";
  };

  home.packages = with pkgs; [ rofi-power-menu ];
}
