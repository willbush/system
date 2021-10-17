{ config, pkgs, ... }:
{
  imports = [
    (import ../profiles/emacs.nix {
      inherit pkgs;
      emacsPackage = pkgs.emacsGcc;
    })
    ../profiles/bat.nix
    ../profiles/gpg.nix
    ../profiles/pkgs/cli.nix
    ../profiles/pkgs/gui.nix
    ../profiles/programs.nix
    ../profiles/redshift.nix
    ./gtk.nix
    ./picom.nix
    ./pkgs/cli.nix
    ./pkgs/gui.nix
    ./rofi.nix
    ./xdg.nix
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
  };

  services.lorri.enable = true;
}
