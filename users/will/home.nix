{ config, pkgs, ... }:

let planck = pkgs.callPackage ../../keyboard-firmware/planck { };
in {
  imports = [
    (import ../profiles/emacs.nix {
      inherit pkgs;
      emacsPackage = pkgs.emacsGcc;
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
      discord
      dnsutils
      dotnet-sdk_3
      feh
      hicolor-icon-theme # fall back icon theme
      lsof
      neofetch
      nethogs
      niv
      nix-prefetch-git
      openconnect
      pandoc
      planck
      remmina
      rust-analyzer
      rustup
      signal-desktop
      slack
      sxiv
      tdesktop # telegram desktop
      texlive.combined.scheme-small # things needed for pandoc
      tokei
      transmission-gtk
      ungoogled-chromium
      virt-manager
      xclip
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

  xdg = {
    enable = true;
    mime.enable = true;
    mimeApps = {
      enable = true;
      # query mime type from a file like this:
      # xdg-mime query filetype your-file.extension
      # also check out:
      # https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
      defaultApplications = {
        "application/msword" = "writer.desktop";
        "application/pdf" = "okularApplication_pdf.desktop";
        "application/vnd.oasis.opendocument.text" = "writer.desktop";
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document" =
          "writer.desktop";

        "application/vnd.ms-excel" = "calc.desktop";
        "application/vnd.oasis.opendocument.spreadsheet" = "calc.desktop";
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" =
          "calc.desktop";

        "audio/aac" = "vlc.desktop";
        "audio/mpeg" = "vlc.desktop";
        "audio/ogg" = "vlc.desktop";
        "audio/wav" = "vlc.desktop";
        "audio/webm" = "vlc.desktop";
        "audio/x-midi" = "vlc.desktop";

        "image/gif" = "sxiv.desktop";
        "image/jpeg" = "sxiv.desktop";
        "image/jpg" = "sxiv.desktop";
        "image/png" = "sxiv.desktop";
        "image/svg+xml" = "firefox.desktop";
        "image/tiff" = "sxiv.desktop";
        "image/vnd.microsoft.icon" = "sxiv.desktop";
        "image/webp" = "sxiv.desktop";

        # Great for going from an albert directory search straight to Emacs # dired.
        "inode/directory" = "emacsclient.desktop";

        "text/html" = "firefox.desktop";
        "text/plain" = "emacsclient.desktop";

        "video/mp4" = "vlc.desktop";
        "video/mpeg" = "vlc.desktop";
        "video/ogg" = "vlc.desktop";
        "video/webm" = "vlc.desktop";
        "video/x-msvideo" = "vlc.desktop";

        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
      };

      associations.added = {
        # These apps overwrite mimeapps.list on startup unless this is explicitly added
        "x-scheme-handler/magnet" = "transmission-gtk.desktop";
        "x-scheme-handler/tg" = "telegramdesktop.desktop";
      };
    };

    userDirs = {
      enable = true;
      desktop = "$HOME/desktop";
      download = "$HOME/downloads";
      videos = "$HOME/videos";
    };
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
