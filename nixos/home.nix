{ config, pkgs, ... }:

{
  # home packages that need no extra configuration
  home.packages = with pkgs; [
    firefox
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
    stack
    haskellPackages.apply-refact
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stylish-haskell
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

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
