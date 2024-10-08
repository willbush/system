{ ... }:
{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    neovim = {
      enable = true;
      extraConfig = builtins.readFile ../../configs/nvim/init.vim;
    };

    bat.enable = true;
    bottom.enable = true; # command: btm
    btop.enable = true;
    fzf.enable = true;
    htop.enable = true;
    imv.enable = true; # command line image viewer intended for use with tiling window managers.
    k9s.enable = true;
    wofi.enable = true;
    yazi.enable = true;
    zoxide.enable = true;
  };
}
