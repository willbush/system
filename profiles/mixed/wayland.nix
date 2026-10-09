{ config, pkgs, ... }:
let
  hmConfig = config.home-manager.users.${config.user.name};
  hyprlandPkg = hmConfig.wayland.windowManager.hyprland.package;
  lock = "${hyprlandPkg}/bin/hyprctl dispatch 'hl.dsp.exec_cmd(\"hyprlock\")'";
  colors = config.lib.stylix.colors.withHashtag;
in
{
  # enable Ozone Wayland support in Chromium and Electron based applications
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # important for system-wide configuration despite being installed via home-manager
  programs.hyprland = {
    enable = true;
    withUWSM = true;
  };

  security.pam.services.hyprlock = { };

  home-manager.users.${config.user.name} = {
    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = false; # managed by UWSM instead
      configType = "lua";
      extraConfig = builtins.readFile ../../configs/hypr/hyprland.lua;
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

    home.packages = [ pkgs.ironbar ];

    # no stylix target for ironbar, so recolor its built-in minimal theme
    xdg.configFile."ironbar/style.css".text = ''
      @import url("file://${pkgs.ironbar.src}/examples/minimal/style.css");

      :root {
        --color-dark-primary: ${colors.base00};
        --color-dark-secondary: ${colors.base02};
        --color-white: ${colors.base05};
        --color-active: ${colors.base0D};
        --color-urgent: ${colors.base08};
      }
    '';

    # mirrors upstream's ironbar.service; UWSM drives graphical-session.target
    systemd.user.services.ironbar = {
      Unit = {
        Description = "Ironbar status bar";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
        Requisite = [ "graphical-session.target" ];
        X-Restart-Triggers = [ hmConfig.xdg.configFile."ironbar/style.css".source ];
      };
      Service = {
        # explicit built-in layout, else a missing config.json is logged as an error
        ExecStart = "${pkgs.ironbar}/bin/ironbar --config minimal --theme ${hmConfig.xdg.configHome}/ironbar/style.css";
        Restart = "on-failure";
      };
      Install.WantedBy = [ "graphical-session.target" ];
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
          command = "${hyprlandPkg}/bin/hyprctl dispatch 'hl.dsp.dpms({ action = \"off\" })'";
          resumeCommand = "${hyprlandPkg}/bin/hyprctl dispatch 'hl.dsp.dpms({ action = \"on\" })'";
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
            path = "~/images/wallpapers/wildflowers.png";
          }
        ];
      };
    };
  };
}
