{ config, pkgs, ... }:

let planck = pkgs.callPackage ../../keyboard-firmware/planck { };
in
{
  imports = [
    (import ../profiles/emacs.nix {
      inherit pkgs;
      emacsPackage = pkgs.emacsGcc;
    })
    ../profiles/bat.nix
    ../profiles/gpg.nix
    ../profiles/packages-gui.nix
    ../profiles/packages.nix
    ../profiles/programs.nix
    ../profiles/rofi.nix
    ../profiles/services.nix
    ../profiles/xdg.nix
  ];

  home = rec {
    stateVersion = "21.11";
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
      # GUI

      ## browsers
      (firefox.override { extraNativeMessagingHosts = [ browserpass ]; })
      ungoogled-chromium

      ## chat
      discord
      signal-desktop
      slack
      tdesktop # telegram desktop
      teams

      ## other
      hicolor-icon-theme # fall back icon theme
      mpv-unwrapped
      remmina
      sxiv
      transmission-gtk
      virt-manager

      # non-gui
      android-tools
      dnsutils
      dotnet-sdk_5
      exiftool
      feh
      lsof
      neofetch
      nethogs
      niv
      nix-prefetch-git
      omnisharp-roslyn
      openconnect
      pandoc
      planck
      rust-analyzer
      rustup
      texlive.combined.scheme-small # things needed for pandoc
      tokei
      woeusb # Windows ISO to USB drive utility
      xclip
      xdotool
      zola
    ];
  };

  programs = {
    neovim = {
      enable = true;
      extraConfig = builtins.readFile ../../nvim/init.vim;
    };

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
      fadeDelta = 3;
      # I only want transparency for a couple of applications.
      opacityRule = [
        "98:class_g *?= 'emacs' && focused"
        "88:class_g *?= 'emacs' && !focused"
        "98:class_g ?= 'alacritty' && focused"
        "88:class_g ?= 'alacritty' && !focused"
      ];
    };
    lorri.enable = true;
  };

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
