{ ... }:
{
  programs = {
    fzf = {
      enable = true;
      # This defaults to true, but I want to make it explicit because installing
      # fzf this way is different than putting it in the home.packages list.
      enableZshIntegration = true;
    };

    htop.enable = true;
    btop.enable = true;
    bottom.enable = true; # command: btm

    # command line image viewer intended for use with tiling window managers.
    imv.enable = true;

    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
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
