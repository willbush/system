{ config, pkgs, ... }:
let sources = import ../nix/sources.nix;
in {
  imports = [
    ../fonts.nix
    ../users.nix
    ../pia/pia-nm.nix
  ];

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
    # I was fooling around with elasticsearch in docker and needed to
    # set this to get it to work. figured I would leave it after
    # testing. see the following for what this does:
    # https://www.kernel.org/doc/Documentation/sysctl/vm.txt
    kernel.sysctl."vm.max_map_count" = 262144;
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
    networkmanager = {
      enable = true;
      # pia-vpn.enable = true;
      # pia-vpn.usernameFile = /home/will/.secrets/pia-vpn.username;
      # pia-vpn.passwordFile = /home/will/.secrets/pia-vpn.password;
      # pia-vpn.serverList = [
      #   "us-atlanta"
      #   "us-california"
      #   "us-chicago"
      #   "us-dal"
      #   "us-denver"
      #   "us-east"
      #   "us-florida"
      #   "us-houston"
      #   "us-lasvegas"
      #   "us-nyc"
      #   "us-sea"
      #   "us-siliconvalley"
      #   "us-washingtondc"
      #   "us-west"
      # ];
    };
  };

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
  };

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    curl
    docker
    docker-compose
    fd
    neovim
    ripgrep
    tree
    wget
  ];

  programs.ssh.startAgent = true;
  # needed for gnome / gtk themes
  programs.dconf.enable = true;
  programs.qt5ct.enable = true;
  programs.gnupg.agent.enable = true;

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
