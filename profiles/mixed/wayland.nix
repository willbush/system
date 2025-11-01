{ config, pkgs, ... }:
let
  lock = "${pkgs.swaylock-effects}/bin/swaylock --daemonize --screenshots --clock --indicator";
  hmConfig = config.home-manager.users.${config.user.name};
  hyprlandPkg = hmConfig.wayland.windowManager.hyprland.package;
in
{
  # enable Ozone Wayland support in Chromium and Electron based applications
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # important for system-wide configuration despite being installed via home-manager
  programs.hyprland.enable = true;

  security.pam.services.swaylock = { };

  home-manager.users.${config.user.name} = {
    wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = builtins.readFile ../../configs/hypr/hyprland.conf;
    };

    # lightweight notification daemon for Wayland
    services.mako = {
      enable = true;
      settings.default-timeout = "5000";
    };

    # requires `security.pam.services.swaylock = { };` at the system level or else
    # unlock will not work.
    programs.swaylock = {
      enable = true;
      package = pkgs.swaylock-effects;

      settings = {
        indicator-radius = 100;
        indicator-thickness = 7;
        effect-blur = "7x5";
        effect-vignette = "0.5:0.5";
        fade-in = 0.2;
      };
    };

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

    services.hyprpaper = {
      enable = true;
      settings = {
        ipc = "on";
        # if files are not present, it will just fallback to hyprland's default
        # wallpaper which is fine with me. I rather have a non-declaritve
        # approach to wallpapers so it doesn't need to be in the nix-store.
        preload = [
          "~/images/wallpapers/1.png"
          "~/images/wallpapers/2.jpg"
          "~/images/wallpapers/3.jpg"
          "~/images/wallpapers/wildflowers.png"
          "~/images/wallpapers/blood-moon.png"
        ];

        # Format: `monitor, /home/me/amongus.png`.
        # monitors info using `hyprctl monitors`.
        # or `, /home/me/amongus.png` to apply to all monitors.
        # - https://wiki.hypr.land/Hypr-Ecosystem/hyprpaper/#the-preload-and-wallpaper-keywords
        wallpaper = [
          "DP-1, ~/images/wallpapers/2.jpg"
        ];
      };
    };
  };
}
