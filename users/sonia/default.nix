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

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
  };

  modules.services.syncthing = {
    enable = true;
    user = "sonia";
  };

  # TODO
  # services.xserver = {
  #   enable = true;
  #   displayManager.sddm.enable = true;
  #   desktopManager.plasma5.enable = true;
  # };

  services = {
    xserver = {
      enable = true;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackges: [
          haskellPackges.xmonad-contrib
          haskellPackges.xmonad-extras
          haskellPackges.xmonad
        ];
      };

      displayManager = {
        defaultSession = "none+xmonad";
        lightdm.enable = true;
      };
    };
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [ curl fd neovim ripgrep tree wget ];

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
