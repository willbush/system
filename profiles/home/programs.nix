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

    # game overlay, fps cap + hardware monitoring
    mangohud = {
      enable = true;
      settings = {
        fps_limit = 144;
        position = "top-right";
        # defaults sit on Shift_L+F1..F4, too easy to hit
        toggle_hud = "Shift_R+F12";
        toggle_hud_position = "Shift_R+F11";
        toggle_preset = "Shift_R+F10";
        toggle_fps_limit = "Shift_R+F1";
        toggle_logging = "Shift_R+F2";
        upload_log = "Shift_R+F3";
        reload_cfg = "Shift_R+F4";
        fps = true;
        frame_timing = true;
        gpu_stats = true;
        gpu_temp = true;
        # rdna3 throttles on junction temp long before edge temp looks bad
        gpu_junction_temp = true;
        # only renders while actively throttling
        throttling_status_graph = true;
        gpu_power = true;
        vram = true;
        cpu_stats = true;
        cpu_temp = true;
        ram = true;
      };
    };

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
