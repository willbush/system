{ config, pkgs, ... }:
let
  hmConfig = config.home-manager.users.${config.user.name};
  hyprlandPkg = hmConfig.wayland.windowManager.hyprland.package;
  lock = "${hyprlandPkg}/bin/hyprctl dispatch exec hyprlock";
in
{
  # enable Ozone Wayland support in Chromium and Electron based applications
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # important for system-wide configuration despite being installed via home-manager
  programs.hyprland.enable = true;

  security.pam.services.hyprlock = { };

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

    programs.hyprlock = {
      enable = true;
      settings = {
        general = {
          ignore_empty_input = true;
        };

        background = {
          path = "screenshot";
          blur_passes = 3;
          blur_size = 5;
        };

        input-field = {
          size = "300, 50";
          outline_thickness = 3;
          position = "0, -20";
        };

        label = [
          {
            text = "cmd[update:1000] date +%H:%M";
            font_size = 64;
            position = "0, 80";
          }
        ];
      };
    };

    services.swayidle = {
      enable = true;
      events = {
        "before-sleep" = lock;
        "lock" = lock;
      };
      timeouts = [
        {
          timeout = 600;
          command = lock;
        }
        {
          timeout = 1200;
          command = "${hyprlandPkg}/bin/hyprctl dispatch dpms off";
          resumeCommand = "${hyprlandPkg}/bin/hyprctl dispatch dpms on";
        }
      ];
    };

    services.hyprpaper = {
      enable = true;
      settings = {
        ipc = "on";
        splash = false;
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

        # https://wiki.hypr.land/Hypr-Ecosystem/hyprpaper/#the-preload-and-wallpaper-keywords
        wallpaper = [
          {
            monitor = "DP-1";
            path = "~/images/wallpapers/1.png";
          }
        ];
      };
    };
  };
}
