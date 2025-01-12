{ ... }:
{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    ghostty = {
      enable = true;
      settings = {
        auto-update = "off";
        window-decoration = false;

        # Keybindings
        keybind = [
          "clear" # clear all defaults

          "ctrl+shift+comma=reload_config"

          "ctrl+shift+d=toggle_window_decorations"

          "ctrl+shift+physical:equal=increase_font_size:1" # C-+ handles QMK quirk
          "ctrl+minus=decrease_font_size:1"
          "ctrl+zero=reset_font_size"

          "ctrl+shift+a=select_all"
          "ctrl+shift+c=copy_to_clipboard"
          "ctrl+shift+v=paste_from_clipboard"

          "ctrl+alt+shift+j=write_scrollback_file:open"
          "ctrl+shift+j=write_scrollback_file:paste"

          "shift+end=scroll_to_bottom"
          "shift+home=scroll_to_top"
          "shift+page_down=scroll_page_down"
          "shift+page_up=scroll_page_up"

          # Tabs
          "ctrl+shift+t=new_tab"
          "ctrl+shift+tab=previous_tab"
          "ctrl+shift+w=close_surface"
          "ctrl+tab=next_tab"

          # Splits (ctrl+a leader key)
          "ctrl+a>s=new_split:down" # bottom split (vim like)
          "ctrl+a>v=new_split:right" # vertical

          "ctrl+shift+enter=toggle_split_zoom"
          "ctrl+shift+down=goto_split:next"
          "ctrl+shift+up=goto_split:previous"
        ];
      };
    };

    bat.enable = true;
    bottom.enable = true; # command: btm
    btop.enable = true;
    htop.enable = true;
    imv.enable = true; # command line image viewer intended for use with tiling window managers.
    rbw.enable = true;
    skim.enable = true;
    wofi.enable = true;
    yazi.enable = true;
    zoxide.enable = true;
  };
}
