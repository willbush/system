{ config, lib, pkgs, ... }:
let inherit (lib) fileContents;
in
{
  imports = [
    ../../modules/services/syncthing.nix
    ../../profiles/common/fonts.nix
    ../../profiles/common/nix-settings.nix
  ];

  users = {
    mutableUsers = false;
    users = {
      root.hashedPassword = fileContents ../../secrets/hashed-password-root.txt;

      sonia = {
        isNormalUser = true;
        home = "/home/sonia";
        hashedPassword = fileContents ../../secrets/hashed-password-sonia.txt;
        shell = pkgs.zsh;
        extraGroups = [ "wheel" "networkmanager" ];
      };
    };
  };

  home-manager.users.sonia = {
    imports = [
      (import ../profiles/emacs.nix {
        inherit pkgs;
        emacsPackage = pkgs.emacsGit;
      })
      ../profiles/bat.nix
      ../profiles/gpg.nix
      ../profiles/pkgs/cli.nix
      ../profiles/pkgs/gui.nix
      ../profiles/programs.nix
      ../profiles/redshift.nix
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
  };

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
  };

  modules.services.syncthing = {
    enable = true;
    user = "sonia";
  };

  services = {
    xserver = {
      enable = true;

      desktopManager.gnome.enable = true;
      displayManager = {
        gdm.enable = true;
        defaultSession = "gnome";
      };
    };
  };

  programs.less.enable = true;

  system.stateVersion = "21.11";
}
