{ config, pkgs, ... }:

let planck = pkgs.callPackage ../../keyboard-firmware/planck { };
in {
  imports = [
    ../profiles/alacritty.nix
    ../profiles/bat.nix
    ../profiles/emacs.nix
    ../profiles/packages.nix
  ];

  home = rec {
    stateVersion = "20.09";
    username = "will";
    homeDirectory = "/home/will";

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
      (firefox.override { extraNativeMessagingHosts = [ browserpass ]; })
      binutils # needed when using cabal-install outside of a nix-shell
      cabal-install
      cabal2nix
      dnsutils
      dotnet-sdk
      haskellPackages.cabal-plan
      haskellPackages.ghcid
      haskellPackages.hasktags
      haskellPackages.hoogle
      hicolor-icon-theme # fall back icon theme
      neofetch
      niv
      nix-prefetch-git
      pandoc
      planck
      ranger
      remmina
      rust-analyzer
      rustup
      slack
      teams
      texlive.combined.scheme-small # things needed for pandoc
      tokei
      transmission-gtk
      virt-manager
      xdotool
      yacreader
    ];
  };

  programs = {
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
    picom = {
      enable = true;
      fade = true;
      vSync = true;
      experimentalBackends = true;
      # the default 'glx' backend lags like crazy for me for some reason.
      backend = "xrender";
      fadeDelta = 1;
      # I only want transparency for a couple of applications.
      opacityRule = [
        "95:class_g *?= 'emacs' && focused"
        "75:class_g *?= 'emacs' && !focused"
        "90:class_g ?= 'alacritty' && focused"
        "75:class_g ?= 'alacritty' && !focused"
      ];
    };

    redshift = {
      enable = true;
      latitude = "33";
      longitude = "-97";
      temperature.day = 6500;
      temperature.night = 3000;
    };

    lorri.enable = true;
  };

  xdg.enable = true;

  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.gnome_themes_standard;
    };
  };
}
