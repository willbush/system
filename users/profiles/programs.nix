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

    git = {
      enable = true;
      userName = "willbush";
      userEmail = "will.g.bush@gmail.com";
      signing = {
        # public key fingerprint
        key = "4441422E61E4C8F3EBFE5E333823864B54B13BDA";
        signByDefault = true;
      };

      extraConfig = {
        merge.conflictstyle = "diff3";
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
        l = "exa";
        ll = "exa -l";
        la = "exa -lah";
        vim = "nvim";
      };

      oh-my-zsh = {
        enable = true;
        plugins = [
          # Completion providers:
          "ripgrep"
          "cargo"
          "rust"
          "fd"
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

    htop = {
      enable = true;
      meters = {
        left = [ "AllCPUs" "Memory" "Swap" ];
        right = [ "Tasks" "LoadAverage" "Uptime" ];
      };
    };

    ssh = {
      enable = true;
      serverAliveInterval = 30;

      matchBlocks."github" = {
        hostname = "github.com";
        identityFile = "~/.secrets/id_rsa_github";
      };
    };
  };
}
