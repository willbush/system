{ pkgs, ... }:
{
  stylix.targets.neovim.enable = false;

  home.file.".config/nvim" = {
    source = ../../configs/nvim;
    recursive = true;
  };

  programs.neovim = {
    enable = true;
    defaultEditor = false;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    # Make these packages available to neovim, but not globally.
    extraPackages = with pkgs; [
      gcc
      gnumake
      tree-sitter
    ];
  };
}
