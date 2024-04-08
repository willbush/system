{ config, pkgs, ... }:
{
  programs = {
    alacritty = {
      enable = true;
      catppuccin.enable = true;
      settings.font.size = 12;
    };

    fzf = {
      enable = true;
      # This defaults to true, but I want to make it explicit because installing
      # fzf this way is different than putting it in the home.packages list.
      enableZshIntegration = true;
    };

    zsh = {
      enable = true;
      dotDir = ".config/zsh";
      enableCompletion = true;
      autosuggestion.enable = true;

      shellAliases = {
        k = "kubectl";
        kcn = "kubectl config set-context $(kubectl config current-context) --namespace";
        mk = "minikube kubectl --"; # https://minikube.sigs.k8s.io/docs/handbook/kubectl/
        em = "emacsclient --create-frame";
        kgp = "kubectl get pods";
        l = "eza";
        la = "eza -lah";
        ll = "eza -l";
        tp = "trash-put";
        vi = "nvim";
        vim = "nvim";
      };

      oh-my-zsh = {
        enable = true;
        plugins = [
          # Completion providers:
          "dotnet"
          "fd"
          "helm"
          "kubectl"
          "minikube"
          "poetry"
          "ripgrep"
          "rust"
        ];
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

    starship = {
      enable = true;
      catppuccin.enable = true;
    };

    htop.enable = true;

    btop = {
      enable = true;
      catppuccin.enable = true;
    };

    bottom = {
      enable = true;
      catppuccin.enable = true;
    };

    imv = {
      enable = true;
      catppuccin.enable = true;
    };

    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
