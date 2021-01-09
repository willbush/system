{ config, pkgs, ... }: {
  imports = [
    (import ../profiles/emacs.nix {
      inherit pkgs;
      emacsPackage = pkgs.emacsGit;
    })
    ../profiles/home/bat.nix
  ];

  home = rec {
    stateVersion = "20.09";
    username = "sonia";
    homeDirectory = "/home/sonia";

    sessionVariables = {
      EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    };

    file = {
      ".config".source = ../../config;
      ".config".recursive = true;
      ".xmonad/xmonad.hs".source = ../../xmonad/xmonad.hs;
      # Outside of NixOS the dictionary directory needs to be set.
      # https://github.com/NixOS/nixpkgs/issues/4521
      ".aspell.conf".text = ''
        dict-dir ${homeDirectory}/.nix-profile/lib/aspell
        master en_US
        extra-dicts en-computers.rws
      '';
    };

    packages = with pkgs; [
      # hicolor-icon-theme # fall back icon theme
      albert
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      bc
      cachix
      calibre
      chromium
      clang-tools
      du-dust
      exa
      feh
      firefox
      gcc
      gimp
      git-crypt
      glances
      gnome3.gnome-screenshot
      gnupg
      gopass
      keepassxc
      libreoffice
      mkpasswd
      nixfmt
      okular
      pavucontrol
      pdfgrep
      peek
      python3
      shfmt
      shutter
      simple-scan
      syncthing-cli # provides stcli
      unar
      unzip
      vlc
      xorg.xkill
      zip
    ];
  };

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

    # TODO
    git = {
      enable = true;
      userName = "willbush";
      userEmail = "will.g.bush@gmail.com";
      signing = {
        # public key fingerprint
        key = "4441422E61E4C8F3EBFE5E333823864B54B13BDA";
        signByDefault = true;
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

    broot = {
      enable = true;
      enableZshIntegration = true;
    };

    htop = {
      enable = true;
      meters = {
        left = [ "AllCPUs" "Memory" "Swap" ];
        right = [ "Tasks" "LoadAverage" "Uptime" ];
      };
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
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

  services = {
    # picom = {
    #   enable = true;
    #   fade = true;
    #   vSync = true;
    #   experimentalBackends = true;
    #   # the default 'glx' backend lags like crazy for me for some reason.
    #   backend = "xrender";
    #   fadeDelta = 1;
    #   # I only want transparency for a couple of applications.
    #   opacityRule = [
    #     "95:class_g *?= 'emacs' && focused"
    #     "75:class_g *?= 'emacs' && !focused"
    #     "90:class_g ?= 'alacritty' && focused"
    #     "75:class_g ?= 'alacritty' && !focused"
    #   ];
    # };

    redshift = {
      enable = true;
      latitude = "33";
      longitude = "-97";
      temperature.day = 6500;
      temperature.night = 3000;
    };
  };

  xdg.enable = true;

  # gtk = {
  #   enable = true;
  #   iconTheme = {
  #     name = "Adwaita";
  #     package = pkgs.gnome3.adwaita-icon-theme;
  #   };
  #   theme = {
  #     name = "Adwaita-dark";
  #     package = pkgs.gnome3.gnome_themes_standard;
  #   };
  # };
}
