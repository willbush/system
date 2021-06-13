{ pkgs, ... }: {

  programs.rofi = {
    enable = true;
    theme = "Indego";
    pass.enable = true;
    font = "Hack 12";
    width = 25;
  };

  home.packages = with pkgs; [ rofi-power-menu ];
}
