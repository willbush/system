{ ... }:
{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    bat.enable = true;
    bottom.enable = true; # command: btm
    btop.enable = true;
    codex.enable = true;
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
}
