{ pkgs, ... }:
let
  wallpapers =
    if builtins.pathExists "/home/will/sync/wallpapers/"
    then "%h/sync/wallpapers/" # %h is a systemd thing for ~/.
    else "${pkgs.gnome.gnome-backgrounds}/share/backgrounds/gnome";
in
{
  services.betterlockscreen = {
    enable = true;
    arguments = [ "dim" ];
  };

  services.random-background = {
    enable = true;
    imageDirectory = wallpapers;
    interval = "1m";
  };
}
