{ config, pkgs, ... }:

let planck = pkgs.callPackage ../../keyboard-firmware/planck { };
in {
  imports = [
    (import ../profiles/emacs.nix {
      inherit pkgs;
      emacsPackage = pkgs.emacsGit;
    })
    ../profiles/bat.nix
    ../profiles/packages.nix
    ../profiles/programs.nix
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
      feh
      haskellPackages.cabal-plan
      haskellPackages.ghcid
      haskellPackages.hasktags
      haskellPackages.hoogle
      hicolor-icon-theme # fall back icon theme
      neofetch
      niv
      nix-prefetch-git
      openconnect
      pandoc
      planck
      ranger
      remmina
      rust-analyzer
      rustup
      signal-desktop
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
