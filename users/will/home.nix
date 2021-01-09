{ config, pkgs, ... }:

let planck = pkgs.callPackage ../../keyboard-firmware/planck { };
in {
  imports = [
    (import ../profiles/emacs.nix { inherit pkgs; })
    ../profiles/bat.nix
    ../profiles/packages.nix
    ../profiles/programs.nix
    ../profiles/services.nix
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
    ];
  };

  programs = {
    broot = {
      enable = true;
      enableZshIntegration = true;
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };
  };

  services.lorri.enable = true;

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
