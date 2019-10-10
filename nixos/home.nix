{ config, pkgs, ... }:

let
  homeDir = builtins.getEnv "HOME";
  planck = pkgs.callPackage ./keyboard-firmware/planck { };
  nixos19_09 = import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.09.tar.gz") { };
in {
  imports = [ ./emacs.nix ];

  home.stateVersion = "19.09";

  nixpkgs.config = { packageOverrides = pkgs: { stable = nixos19_09; }; };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.sessionVariables = {
    EDITOR = "emacsclient --create-frame --alternate-editor emacs";
  };

  home.file = {
    ".config".source = ../config;
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

  # home packages that need no extra configuration
  home.packages = with pkgs; [
    albert
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    cabal-install
    cachix
    chromium
    dnsutils
    docker-compose
    dotnet-sdk
    exa
    feh
    firefox
    fzf
    gcc
    ghc
    gimp
    gnupg
    haskellPackages.apply-refact
    haskellPackages.brittany
    haskellPackages.ghcid
    haskellPackages.hakyll
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stylish-haskell
    keepassxc
    libreoffice
    nix-prefetch-git
    nixfmt
    openconnect
    planck
    ranger
    remmina
    rustup
    simple-scan
    stable.cabal2nix
    stack
    tokei
    transmission-gtk
    unar
    vlc
  ];

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
      dropbox = "docker exec -it dropbox dropbox";
      dropbox-start = ''
        docker run -d --restart=always --name=dropbox \
          -v ${homeDir}/Dropbox:/dbox/Dropbox \
          -v ${homeDir}/.dropbox:/dbox/.dropbox \
          -e DBOX_UID=1000 -e DBOX_GID=100 janeczku/dropbox
      '';
    };
    oh-my-zsh = {
      enable = true;
      theme = "lambda";
    };
  };

  programs.htop = {
    enable = true;
    meters = {
      left = [ "AllCPUs" "Memory" "Swap" "Battery" ];
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

  services.compton = {
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
    temperature.night = 1800;
  };

  services.syncthing.enable = true;
}
