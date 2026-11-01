{ pkgs, ... }:
{
  # stream to Moonlight
  services.sunshine = {
    enable = true;
    openFirewall = true;
    settings = {
      # left unset, sunshine also tries the portal, which pops a screen-share picker
      capture = "wlr";
      # DuckStation's headless output. When absent (Desktop) wlr falls back to DP-1
      output_name = "SUNSHINE";
    };
    applications.env = {
      # the 8BitDo connects after launch and appimage-run's user namespace drops
      # udev hotplug events, so let SDL watch /dev/input itself
      SDL_JOYSTICK_DISABLE_UDEV = "1";
    };
    # replaces sunshine's default list, so Desktop is re-added
    applications.apps = [
      { name = "Desktop"; }
      {
        name = "DuckStation";
        # nogui skips the main window, which could map after Big Picture and unfullscreen it
        cmd = "/home/will/downloads/DuckStation-x64.AppImage -nogui -bigpicture -fullscreen";
        # a headless output at the client's mode so 16:9 fills the TV.
        # two entries since a failing do skips its own undo
        prep-cmd = [
          {
            # rule first, the output reads it on connect
            do = toString (
              pkgs.writeShellScript "sunshine-output-create" ''
                r=$(hyprctl eval "hl.monitor({ output = \"SUNSHINE\", mode = \"''${SUNSHINE_CLIENT_WIDTH}x''${SUNSHINE_CLIENT_HEIGHT}@''${SUNSHINE_CLIENT_FPS}\", position = \"auto\", scale = 1 })")
                [ "$r" = ok ] || { echo "$r" >&2; exit 1; }
                r=$(hyprctl output create headless SUNSHINE)
                [ "$r" = ok ] || [ "$r" = "Name already taken" ] || { echo "$r" >&2; exit 1; }
              ''
            );
            # also moves focus back to DP-1
            undo = toString (pkgs.writeShellScript "sunshine-output-remove" "hyprctl output remove SUNSHINE");
          }
          {
            # DuckStation opens on the focused monitor
            do = toString (
              pkgs.writeShellScript "sunshine-output-focus" ''
                r=$(hyprctl dispatch 'hl.dsp.focus({ monitor = "SUNSHINE" })')
                [ "$r" = ok ] || { echo "$r" >&2; exit 1; }
              ''
            );
          }
        ];
      }
    ];
  };
}
