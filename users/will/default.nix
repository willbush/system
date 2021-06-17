{ config, lib, pkgs, ... }:
let inherit (lib) fileContents;
in {
  imports = [
    ../../modules/services/syncthing.nix
    ../../profiles/common/fonts.nix
    ../../profiles/common/nix-settings.nix
    ../../profiles/common/system-programs.nix
  ];

  users = {
    mutableUsers = false;
    users = {
      root.hashedPassword = fileContents ../../secrets/hashed-password-root.txt;

      will = {
        isNormalUser = true;
        home = "/home/will";
        hashedPassword = fileContents ../../secrets/hashed-password-will.txt;
        shell = pkgs.zsh;
        extraGroups = [ "wheel" "networkmanager" "docker" "libvirtd" ];
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEE2hQsuOQZ3PvM2DdI0vxpaBFoRQpFhGXZmeRq8Srs6 tau-ceti-2020-11-16"
        ];
      };
    };
  };

  home-manager.users.will = import ./home.nix;

  modules.unfree.allowList = [ "discord" "slack" "teams" ];

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
    iproute2.enable = true; # Needed for mullvad daemon
    wireguard.enable = true;
  };

  modules.services.syncthing = {
    enable = true;
    user = "will";
  };

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

    mullvad-vpn.enable = true;
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    curl
    fd
    ripgrep
    tree
    wget
    # broken ATM unless firewall is disabled see: https://github.com/NixOS/nixpkgs/issues/113589
    mullvad-vpn
  ];

  programs.less = {
    enable = true;
    # Rebind to my custom vim bindings for Colemak-DHm
    commands = {
      n = "forw-line";
      e = "back-line";
      N = "forw-scroll";
      E = "back-scroll";
      h = "repeat-search";
      H = "reverse-search";
      "^H" = "help"; # This syntax means C-h
      k = "set-mark";
      K = "set-mark-bottom";
      j = "goto-mark";
    };
  };

  system.stateVersion = "20.09";
}
