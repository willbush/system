{ pkgs, config, ... }:
let
  wallpapers =
    if config.modules.services.syncthing.enable
    then "/home/will/sync/wallpapers"
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
    interval = "1h";
  };
}
