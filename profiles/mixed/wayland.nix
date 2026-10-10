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

    # layout and css adapted from sanjar-xolmatov/hyprland-dotfiles, recolored with stylix
    stylix.targets.waybar.addCss = false;

    # UWSM drives the graphical-session.target the unit hangs off
    programs.waybar = {
      enable = true;
      systemd.enable = true;
      style = ''
        * {
          all: unset;
          font-family: "${config.stylix.fonts.monospace.name}";
          font-size: ${toString config.stylix.fonts.sizes.desktop}pt;
        }
      ''
      + builtins.readFile ../../configs/waybar/style.css;
      settings.main = {
        position = "top";
        # hyprland/workspaces clicks send legacy hyprctl syntax, which the lua config rejects
        modules-left = [ "ext/workspaces" ];
        modules-center = [ "hyprland/window" ];
        modules-right = [
          "tray"
          "pulseaudio"
          "cpu"
          "memory"
          "disk"
          "clock"
        ];
        "ext/workspaces".on-click = "activate";
        "hyprland/window" = {
          format = "{class}  {title}";
          max-length = 50;
          separate-outputs = true;
          tooltip = false;
        };
        tray.icon-size = 18;
        pulseaudio = {
          # NFM squeezes glyphs to one cell, the non-mono NF variant draws them full size
          # (an x-large span instead pushed the text off the baseline)
          format = "<span font_family='ZedMono NF Extd'>{icon}</span>  {volume}%";
          format-muted = "<span font_family='ZedMono NF Extd'>󰝟</span>";
          format-icons.default = [
            ""
            ""
            ""
          ];
          scroll-step = 5;
          # launched as its own unit so restarting waybar does not kill it
          on-click = "uwsm app -- pavucontrol";
        };
        cpu = {
          format = "cpu: {usage}%";
          interval = 4;
        };
        memory = {
          format = "mem: {percentage}%";
          tooltip-format = "RAM: {used:0.1f}G / {total:0.1f}G";
          interval = 4;
        };
        disk = {
          format = "disk: {percentage_used}%";
          tooltip-format = "{path}: {specific_used:0.2f}GB used / {specific_free:0.2f}GB free";
          unit = "GB";
          path = "/nix";
          interval = 30;
        };
        clock = {
          format = "{:%H:%M}";
          format-alt = "{:%A, %B %d, %Y (%R)} <span font_family='ZedMono NF Extd'>󰃰</span> ";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          actions = {
            on-click-right = "mode";
            # reversed so the year view scrolls like a page
            on-scroll-up = "shift_down";
            on-scroll-down = "shift_up";
          };
          calendar = {
            mode = "year";
            mode-mon-col = 3;
            weeks-pos = "right";
            iso8601 = true; # monday start, ISO week numbers
            on-scroll = 1;
            format = {
              months = "<span color='${colors.base06}'><b>{}</b></span>";
              days = "<span color='${colors.base0F}'><b>{}</b></span>";
              weeks = "<span color='${colors.base0C}'><b>W{}</b></span>";
              weekdays = "<span color='${colors.base0A}'><b>{}</b></span>";
              today = "<span color='${colors.base08}'><b><u>{}</u></b></span>";
            };
          };
        };
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
