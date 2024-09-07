{ ... }:
{
  programs = {
    fzf.enable = true;
    htop.enable = true;
    btop.enable = true;
    bottom.enable = true; # command: btm

    # command line image viewer intended for use with tiling window managers.
    imv.enable = true;

    zoxide = {
      enable = true;
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    k9s.enable = true;

    wofi.enable = true;

    yazi.enable = true;

    neovim = {
      enable = true;
      extraConfig = builtins.readFile ../../configs/nvim/init.vim;
    };
  };
}
