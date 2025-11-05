{ ... }:
{
  programs.ghostty = {
    enable = true;
    settings = {
      auto-update = "off";
      window-decoration = false;
      # don't prompt to close window if process is still running.
      confirm-close-surface = false;

      custom-shader = [
        "${../../configs/ghostty/shaders/cursor_warp.glsl}"
        "${../../configs/ghostty/shaders/cursor_ripple.glsl}"
      ];

      # Keybindings
      keybind = [
        # don't need or use 99% of this shit.
        "clear" # clear all defaults

        "ctrl+shift+comma=reload_config"

        # C-+ is broken for qmk. just rebind
        "ctrl+arrow_up=increase_font_size:1"
        "ctrl+arrow_down=decrease_font_size:1"
        "ctrl+zero=reset_font_size"

        "ctrl+shift+a=select_all"
        "ctrl+shift+c=copy_to_clipboard"
        "ctrl+shift+v=paste_from_clipboard"

        "alt+a>h=write_scrollback_file:copy"
        "alt+a>s=write_screen_file:copy"
      ];
    };
  };
}
