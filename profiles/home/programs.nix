{
  config,
  inputs,
  pkgs,
  ...
}:
let
  system = pkgs.stdenv.hostPlatform.system;
in
{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    bat.enable = true;
    bottom.enable = true; # command: btm
    btop.enable = true;
    htop.enable = true;
    imv.enable = true; # command line image viewer intended for use with tiling window managers.
    mpv.enable = true;

    rbw.enable = true;
    fzf.enable = true; # used by yazi
    skim.enable = true;

    tofi = {
      enable = true;
      # style settings / font handled by stylix
      settings = {
        border-width = 1;
        outline-width = 1;
      };
    };
    zoxide.enable = true;
  };

  # its theming shrinks the font to 14 at 1.33 scale, too small to read in game
  stylix.targets.mangohud.enable = false;
  # game overlay, caps frames and reports render resolution
  programs.mangohud = {
    enable = true;
    settings = {
      fps_limit = 144;
      no_display = true;
      position = "top-left";
      font_size = 28;
      # defaults sit on Shift_L+F1..F4, too easy to hit
      toggle_hud = "Shift_R+F12";
      toggle_hud_position = "Shift_R+F11";
      toggle_preset = "Shift_R+F10";
      toggle_fps_limit = "Shift_R+F1";
      toggle_logging = "Shift_R+F2";
      upload_log = "Shift_R+F3";
      reload_cfg = "Shift_R+F4";
      fps = true; # on by default, listed to keep the readouts in one place
      frame_timing = true;
      resolution = true;
    };
  };

  # This also has the undocumented effect of `.claude.json` ending up in this folder.
  home.sessionVariables.CLAUDE_CONFIG_DIR = "${config.xdg.configHome}/claude";
  programs.claude-code = {
    enable = true;
    package = inputs.claude-code-nix.packages.${system}.claude-code;
  };
  programs.codex = {
    enable = true;
    package = inputs.codex-cli-nix.packages.${system}.default;
  };
}
