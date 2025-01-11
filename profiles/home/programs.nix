{ ... }:
{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    alacritty.enable = true;
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
