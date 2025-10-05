{
  inputs,
  pkgs,
  config,
  ...
}:
{
  stylix.targets.neovim.enable = false;

  programs.neovim = {
    enable = true;
    package = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;

    defaultEditor = false;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    # Changing the config home to here nvim lua config should be in this repo.
    # After deploying emacs config for year via nix. Think I'm going to try it
    # the ideomatic non-nix way and just use a plugin manager.
    # NOTE: This completely breaks `programs.neovim.plugins` for some reason.
    extraWrapperArgs = [
      "--set"
      "XDG_CONFIG_HOME"
      "${config.home.homeDirectory}/code/system/configs"
    ];
  };
}
