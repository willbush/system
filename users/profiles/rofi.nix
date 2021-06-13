{ pkgs, ... }: {

  programs.rofi = {
    enable = true;
    theme = "Indego";
    pass.enable = true;
    font = "Hack Nerd Font Mono 12";
    width = 25;
  };

  home.packages = with pkgs; [ rofi-power-menu ];
}
