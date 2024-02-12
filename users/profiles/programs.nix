{ config, pkgs, ... }: {
  programs = {
    alacritty = {
      enable = true;
      # Colors (One Dark)
      # setting examples: https://github.com/jwilm/alacritty/blob/master/alacritty.yml
      # themes: https://github.com/eendroroy/alacritty-theme
      settings = {
        font.size = 12;
        colors = {
          # I changed the primary background color to black because I like that
          # with transparency.
          primary = {
            background = "0x000000";
            foreground = "0xabb2bf";
          };
          normal = {
            black = "0x1e2127";
            red = "0xe06c75";
            green = "0x98c379";
            yellow = "0xd19a66";
            blue = "0x61afef";
            magenta = "0xc678dd";
            cyan = "0x56b6c2";
            white = "0xabb2bf";
          };
          bright = {
            black = "0x5c6370";
            red = "0xe06c75";
            green = "0x98c379";
            yellow = "0xd19a66";
            blue = "0x61afef";
            magenta = "0xc678dd";
            cyan = "0x56b6c2";
            white = "0xffffff";
          };
        };
      };
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
      enableAutosuggestions = true;
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
          "docker"
          "docker-compose"
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

    starship.enable = true;

    htop.enable = true;

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
