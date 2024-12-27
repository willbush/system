{ pkgs, ... }:
{
  stylix.targets.neovim.enable = false;

  home.file.".config/nvim" = {
    source = ../../configs/nvim;
    recursive = true;
  };

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    # Make these packages available to neovim, but not globally.
    extraPackages = with pkgs; [
      gcc
      gnumake
      tree-sitter
      luajitPackages.luarocks
    ];
  };
}
