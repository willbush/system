{ pkgs, config, ... }:
let
  lock = "${pkgs.swaylock-effects}/bin/swaylock --daemonize --screenshots --clock --indicator";
  hyprlandPkg = config.wayland.windowManager.hyprland.package;
in
{
  services.swayidle = {
    enable = true;
    events = [
      # executes command before systemd puts the computer to sleep.
      {
        event = "before-sleep";
        command = lock;
      }
      # executes command when logind signals that the session should be locked
      {
        event = "lock";
        command = lock;
      }
    ];
    timeouts = [
      {
        timeout = 300;
        command = lock;
      }
      {
        timeout = 600;
        command = "${hyprlandPkg}/bin/hyprctl dispatch dpms off";
        resumeCommand = "${hyprlandPkg}/bin/hyprctl dispatch dpms on";
      }
    ];
  };
}
