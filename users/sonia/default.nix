{ config, lib, pkgs, ... }:
let inherit (lib) fileContents;
in {
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

  home-manager.users.sonia = import ./home.nix;

  modules.unfree.allowList = [ "teams" ];

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

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [ curl fd ripgrep tree wget ];

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    # needed for gnome / gtk themes
    dconf.enable = true;
    qt5ct.enable = true;
    less.enable = true;
  };

  system.stateVersion = "20.09";
}
