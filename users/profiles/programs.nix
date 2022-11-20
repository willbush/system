{ config, pkgs, ... }: {
  programs = {
    alacritty = {
      enable = true;
      # Colors (One Dark)
      # setting examples: https://github.com/jwilm/alacritty/blob/master/alacritty.yml
      # themes: https://github.com/eendroroy/alacritty-theme
      settings = {
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
        l = "exa";
        la = "exa -lah";
        ll = "exa -l";
        vim = "nvim";
        # TODO: test if I can remove this.
        # This is a work around to only the uppercase binary is on the path when I install this via cli.nix.
        # Not sure why that happens. When I install it via `nix-shell -p omnisharp` it works fine.
        omnisharp = "OmniSharp";
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
      initExtra = pkgs.lib.fileContents ../../config/zsh/zshrc-init-extra.sh;
    };

    starship.enable = true;

    htop.enable = true;

    broot = {
      enable = true;
      enableZshIntegration = true;
    };

    navi = {
      enable = true;
      enableZshIntegration = true;
    };

    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };
  };
}
