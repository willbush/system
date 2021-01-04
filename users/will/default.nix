{ config, lib, pkgs, ... }:
let inherit (lib) fileContents;
in {
  imports = [ ../../fonts.nix ../../modules/services/syncthing.nix ];

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

  home-manager = {
    users.will = import ./home.nix;
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  # TODO can remove for some hosts?
  nixpkgs.config = {
    # Allow unfree, which is required for some drivers.
    allowUnfree = true;
  };

  nix = {
    # Required until nix version 2.4 for nix flakes
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    useSandbox = true;
    autoOptimiseStore = true;
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hercules-ci.cachix.org"
      "https://iohk.cachix.org"
      "https://nix-community.cachix.org"
      "https://nix-tools.cachix.org"
      "https://nixcache.reflex-frp.org"
      "https://willbush.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-tools.cachix.org-1:ebBEBZLogLxcCvipq2MTvuHlP7ZRdkazFSQsbs0Px1A="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "willbush.cachix.org-1:PuQjKarzPYTnxgEzKUoTDQ+aN0SImhO8NMZ0CamKBL4="
    ];
    gc = {
      automatic = true;
      dates = "23:00";
      options = "--delete-older-than 30d";
    };
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  boot = {
    # required for libvirtd
    kernelModules = [ "kvm-amd" "kvm-intel" ];

    cleanTmpDir = true; # cleans all files in /tmp during boot
    initrd.checkJournalingFS = true; # run fsck for journal file system

    loader = {
      # Timeout (in seconds) until loader boots the default menu item.
      timeout = 2;
      # Use the systemd-boot EFI boot loader.
      systemd-boot = {
        enable = true;
        # Limit number of configurations to keep /boot partition from filling
        # up.
        configurationLimit = 10;
        memtest86.enable = true;
        # Fixes a security hole in place for the sake of backwards
        # compatibility. See description in:
        # nixpkgs/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
        editor = false;
      };
      efi.canTouchEfiVariables = true;
    };
  };

  sound.enable = true;
  hardware = {
    pulseaudio.enable = true;
    sane.enable = true;
    sane.brscan4 = {
      enable = true;
      netDevices = {
        office-printer = {
          model = "HL-2380DW";
          ip = "192.168.1.166";
        };
      };
    };
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

  # Change the default timeout for a service from 90 seconds.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=30s
  '';

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
