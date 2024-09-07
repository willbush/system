{ ... }:
{
  programs = {
    fish = {
      enable = true;

      shellAliases = {
        k = "kubectl";
        la = "eza -lah";
        tp = "trash-put";
      };

      # history = {
      #   # path = "${config.xdg.dataHome}/zsh/zsh_history";
      #   extended = false; # Whether to insert timestamps
      #   ignoreDups = true;
      #   size = 100000;
      #   save = 100000;
      # };
    };
    starship.enable = true;
  };
}
