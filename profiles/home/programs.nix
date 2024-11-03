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

    alacritty.enable = true;
    bat.enable = true;
    bottom.enable = true; # command: btm
    btop.enable = true;
    fzf.enable = true;
    htop.enable = true;
    imv.enable = true; # command line image viewer intended for use with tiling window managers.
    wofi.enable = true;
    yazi.enable = true;
    zoxide.enable = true;
  };
}
