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
    stateVersion = "20.09";
    username = "sonia";
    homeDirectory = "/home/sonia";

    sessionVariables = {
      EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    };

    file = {
      ".config".source = ../../config;
      ".config".recursive = true;
      ".xmonad/xmonad.hs".source = ../../xmonad/xmonad.hs;
      # Outside of NixOS the dictionary directory needs to be set.
      # https://github.com/NixOS/nixpkgs/issues/4521
      ".aspell.conf".text = ''
        dict-dir ${homeDirectory}/.nix-profile/lib/aspell
        master en_US
        extra-dicts en-computers.rws
      '';
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
