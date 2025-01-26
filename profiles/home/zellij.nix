{ inputs, pkgs, ... }:
let
  zjstatus = inputs.zjstatus.packages.${pkgs.system}.default;
in
{
  programs.zellij = {
    enable = true;
  };

  xdg.configFile."zellij/layouts/default.kdl".text = ''
    layout {
        default_tab_template {
            children
            pane size=1 borderless=true {
                plugin location="file:${zjstatus}/bin/zjstatus.wasm" {
                    hide_frame_for_single_pane "true"

                    format_left  "{mode}#[fg=#89B4FA,bg=#181825,bold] {session}#[bg=#181825] {tabs}"
                    format_space "#[bg=#181825]"

                    mode_normal          "#[bg=#89B4FA] "
                    mode_tmux            "#[bg=#ffc387] "
                    mode_default_to_mode "tmux"

                    tab_normal               "#[fg=#6C7086,bg=#181825] {index} {name} {fullscreen_indicator}{sync_indicator}{floating_indicator}"
                    tab_active               "#[fg=#9399B2,bg=#181825,bold,italic] {index} {name} {fullscreen_indicator}{sync_indicator}{floating_indicator}"
                    tab_fullscreen_indicator "□ "
                    tab_sync_indicator       "  "
                    tab_floating_indicator   "󰉈 "

                    datetime          "#[fg=#9399B2,bg=#181825] {format} "
                    datetime_format   "%A, %d %b %Y %H:%M"
                    datetime_timezone "America/Chicago"
                }
            }
        }
    }'';
}
