{ config, pkgs, ... }:

let
  homeDir = builtins.getEnv "HOME";
  planck = pkgs.callPackage ./keyboard-firmware/planck {};
  unstableTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
in
{
  imports = [
    ./emacs.nix
  ];

  home.stateVersion = "19.03";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  home.sessionVariables =  {
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
  home.packages = with pkgs.unstable; [
    # haskellPackages.brittany # marked broken but fixed with an overlay
    # haskellPackages.hakyll
    albert
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    cabal-install
    cabal2nix
    chromium
    dotnet-sdk
    exa
    feh
    firefox
    fzf
    gcc
    gimp
    gnupg
    haskellPackages.apply-refact
    haskellPackages.ghcid
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stylish-haskell
    keepassxc
    libreoffice
    nix-prefetch-git
    openconnect
    planck
    ranger
    remmina
    rustup
    simple-scan
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
      l  = "exa";
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
    temperature.night = 2200;
  };

  services.syncthing.enable = true;
}
