{ config, pkgs, ... }: {
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
    username = "sonia";
    homeDirectory = "/home/sonia";

    sessionVariables = {
      EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    };

    file = {
      ".config".source = ../../config;
      ".config".recursive = true;
    };

    packages = with pkgs;
      [
        # hicolor-icon-theme # fall back icon theme
        firefox
      ];
  };

  xdg.enable = true;

  # gtk = {
  #   enable = true;
  #   iconTheme = {
  #     name = "Adwaita";
  #     package = pkgs.gnome3.adwaita-icon-theme;
  #   };
  #   theme = {
  #     name = "Adwaita-dark";
  #     package = pkgs.gnome3.gnome_themes_standard;
  #   };
  # };
}
