{ pkgs, ... }: {

  programs.rofi = {
    enable = true;
    theme = "Arc-Dark";
    pass.enable = true;
    font = "Hack Nerd Font Mono 12";
    plugins = with pkgs; [
      rofi-calc
    ];
  };

  home.packages = with pkgs; [ rofi-power-menu ];
}
