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

      will = {
        uid = 1000;
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

  # TODO can remove for some hosts?
  nixpkgs.config = {
    # Allow unfree, which is required for some drivers.
    allowUnfree = true;
  };

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
    iproute2.enable = true; # Needed for mullvad daemon
    wireguard.enable = true;
  };

  modules.services.syncthing.enable = true;

  services = {
    # Enable CUPS to print documents.
    printing.enable = true;
    printing.drivers = [ pkgs.hll2390dw-cups ];

    xserver = {
      enable = true;
      layout = "us";

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
      desktopManager.xterm.enable = false;
    };

    mullvad-vpn.enable = true;
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    curl
    fd
    neovim
    ripgrep
    tree
    wget
    mullvad-vpn
  ];

  programs = {
    ssh.startAgent = true;
    # needed for gnome / gtk themes
    dconf.enable = true;
    qt5ct.enable = true;
    gnupg.agent.enable = true;

    less = {
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
  };

  system.stateVersion = "20.09";
}
