{ config, pkgs, ... }:

{
  home.stateVersion = "19.03";

  # home packages that need no extra configuration
  home.packages = with pkgs; [
    exa
    htop
    firefox
    chromium
    keepassxc
    vlc
    albert
    aspell
    aspellDicts.en
    libreoffice
    gcc
    rustup
    tokei
    feh
    fzf
    ranger
    gnupg
    stow
    #haskell
    cabal-install
    cabal2nix
    nix-prefetch-git
    stack
    haskellPackages.apply-refact
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stylish-haskell
    haskellPackages.ghcid
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "willbush";
    userEmail = "will.g.bush@gmail.com";
    extraConfig = ''
      [credential]
      helper = cache --timeout=3600
    '';
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
        -v /home/will/Dropbox:/dbox/Dropbox \
        -v /home/will/.dropbox:/dbox/.dropbox \
        -e DBOX_UID=1000 -e DBOX_GID=100 janeczku/dropbox'';
    };
    oh-my-zsh = {
      enable = true;
      plugins = ["vi-mode" "web-search"];
      theme = "agnoster";
    };
  };

  services.compton = {
    enable = true;
    fade = true;
    backend = "xrender";
    fadeDelta = 1;
    inactiveOpacity = "0.75";
    activeOpacity = "0.90";
    opacityRule = [
      "99:name *= 'Firefox'"
      "99:name *= 'Chromium'"
      "99:name *= 'VLC'"
    ];
  };

  services.redshift = {
    enable = true;
    latitude = "33";
    longitude = "-97";
    temperature.day = 6500;
    temperature.night = 2700;
  };
}
