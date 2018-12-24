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

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
