{ config, pkgs, ... }:
{
  programs = {
    zsh = {
      enable = true;
      dotDir = ".config/zsh";
      enableCompletion = true;
      autosuggestion.enable = true;

      shellAliases = {
        k = "kubectl";
        l = "eza";
        la = "eza -lah";
        ll = "eza -l";
        tp = "trash-put";
        vi = "nvim";
        vim = "nvim";
      };

      history = {
        path = "${config.xdg.dataHome}/zsh/zsh_history";
        extended = false; # Whether to insert timestamps
        ignoreDups = true;
        size = 100000;
        save = 100000;
      };
      initExtra = pkgs.lib.fileContents ../../configs/zsh/zshrc-init-extra.sh;
    };
    starship.enable = true;
  };

  home.packages = [ pkgs.zsh-completions ];
}
