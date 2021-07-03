{ config, pkgs, ... }: {
  imports = [
    (import ../profiles/emacs.nix {
      inherit pkgs;
      emacsPackage = pkgs.emacsGit;
    })
    ../profiles/bat.nix
    ../profiles/packages.nix
    ../profiles/programs.nix
    ../profiles/services.nix
  ];

  home = rec {
    stateVersion = "21.11";
    username = "sonia";
    homeDirectory = "/home/sonia";

    sessionVariables = {
      EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    };

    file = {
      ".config".source = ../../config;
      ".config".recursive = true;
    };

    packages = with pkgs; [ chromium firefox vscodium ];
  };

  xdg.enable = true;

  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita";
      package = pkgs.gnome3.gnome_themes_standard;
    };
  };
}
