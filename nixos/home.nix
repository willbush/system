{ config, pkgs, ... }:

let
  homeDir = builtins.getEnv "HOME";
  sources = import ./nix/sources.nix;
  planck = pkgs.callPackage ./keyboard-firmware/planck { };
  nixos20_03 = import sources."nixpkgs-20.03" { };
in {
  imports = [ ./emacs.nix ];

  home.stateVersion = "19.09";

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: { stable = nixos20_03; };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.sessionVariables = {
    EDITOR = "emacsclient --create-frame --alternate-editor emacs";
  };

  home.file = {
    ".config".source = ./config;
    ".config".recursive = true;
    ".xmonad/xmonad.hs".source = ../xmonad/xmonad.hs;
    # Outside of NixOS the dictionary directory needs to be set.
    # https://github.com/NixOS/nixpkgs/issues/4521
    ".aspell.conf".text = ''
      dict-dir ${homeDir}/.nix-profile/lib/aspell
      master en_US
      extra-dicts en-computers.rws
      add-extra-dicts en_US-science.rws
    '';
  };

  xdg.enable = true;

  home.packages = with pkgs; [
    albert
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    # https://github.com/NixOS/nixpkgs/issues/55995
    # needed when using cabal-install outside of a nix-shell
    binutils
    cabal-install
    cabal2nix
    cachix
    calibre
    chromium
    dnsutils
    docker-compose
    dotnet-sdk
    du-dust
    exa
    firefox-beta-bin
    fzf
    gcc
    gimp
    glances
    gnome3.gnome-screenshot
    gnupg
    stable.haskellPackages.apply-refact # used by hlint-refactor
    stable.haskellPackages.brittany
    haskellPackages.ghcid
    haskellPackages.hakyll
    haskellPackages.hasktags
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.hpack
    keepassxc
    libreoffice
    neofetch
    nix-prefetch-git
    nixfmt
    okular
    openconnect
    pavucontrol
    planck
    ranger
    remmina
    rustup
    simple-scan
    syncthing-cli # provides stcli
    tokei
    transmission-gtk
    unar
    unzip
    vlc
    yacreader
    zip
  ];

  programs.alacritty = {
    enable = true;
    # Colors (One Dark)
    # setting examples: https://github.com/jwilm/alacritty/blob/master/alacritty.yml
    # themes: https://github.com/eendroroy/alacritty-theme
    settings = {
      colors = {
        # Using the default primary background and foreground colors because I
        # like the dark black with transparency.
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

  programs.git = {
    enable = true;
    userName = "willbush";
    userEmail = "will.g.bush@gmail.com";
  };

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    enableAutosuggestions = true;
    shellAliases = {
      l = "exa";
      ll = "exa -l";
      la = "exa -lah";
      vim = "nvim";
      du = "dust";
    };
    oh-my-zsh = {
      enable = true;
      theme = "lambda";
    };
  };

  programs.htop = {
    enable = true;
    meters = {
      left = [ "AllCPUs" "Memory" "Swap" ];
      right = [ "Tasks" "LoadAverage" "Uptime" ];
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.ssh = {
    enable = true;
    serverAliveInterval = 30;

    matchBlocks."github" = {
      hostname = "github.com";
      identityFile = "~/.ssh/id_rsa_github";
    };
  };

  services.picom = {
    enable = true;
    fade = true;
    backend = "xrender";
    fadeDelta = 1;
    # I only want transparency for a couple of applications.
    opacityRule = [
      "90:class_g ?= 'emacs' && focused"
      "75:class_g ?= 'emacs' && !focused"
      "90:class_g ?= 'alacritty' && focused"
      "75:class_g ?= 'alacritty' && !focused"
    ];
  };

  services.redshift = {
    enable = true;
    latitude = "33";
    longitude = "-97";
    temperature.day = 6500;
    temperature.night = 3000;
  };

  services.syncthing.enable = true;
  services.lorri.enable = true;
}
