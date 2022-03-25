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
        extraGroups = [
          "wheel"
          "networkmanager"
          "docker"
          # For Arduino ESP32
          "tty"
          "dialout"
        ];
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
      ../profiles/gtk.nix
      ../profiles/pkgs/cli.nix
      ../profiles/pkgs/gui.nix
      ../profiles/programs.nix
      ../profiles/redshift.nix
      ./git.nix
      ./pkgs/gui.nix
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

      packages = with pkgs; [ chromium firefox ];
    };

    xdg.enable = true;

    programs = {
      neovim.enable = true;
    };
  };

  modules.unfree.allowList = [ "discord" "teams" "android-studio-stable" ];

  networking = {
    firewall.enable = false;
    networkmanager.enable = true;
  };

  modules.services.syncthing = {
    enable = true;
    user = "sonia";
  };

  services = {
    xserver = {
      enable = true;

      # swaps caps-lock and escape keys for better escape access in vim.
      xkbOptions = "caps:swapescape";

      desktopManager.gnome.enable = true;
      displayManager = {
        gdm = {
          enable = true;
          wayland = false;
        };
        defaultSession = "gnome";
      };
    };
  };

  programs.less.enable = true;

  system.stateVersion = "21.11";
}
