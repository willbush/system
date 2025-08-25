{ pkgs, config, ... }:
{
  stylix.targets.neovim.enable = false;

  programs.neovim = {
    enable = true;
    defaultEditor = false;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    # Make these packages available to neovim, but not globally.
    extraPackages = with pkgs; [
      gcc
    ];

    extraWrapperArgs = [
      "--set"
      "XDG_CONFIG_HOME"
      "${config.home.homeDirectory}/code/system/configs"
    ];
  };

}
