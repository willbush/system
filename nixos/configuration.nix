{ config, pkgs, ... }:
let
  sources = import ./nix/sources.nix;
  all-hies = import sources."all-hies" { };
  ghcide-nix = import sources."ghcide-nix" { };
in {
  imports =
    [ ./hardware-configuration.nix ./fonts.nix ./users.nix ./pia/pia-nm.nix ];

  nixpkgs.config = {
    # Allow unfree, which is required for some drivers.
    allowUnfree = true;
  };

  nix = {
    useSandbox = true;
    autoOptimiseStore = true;
    maxJobs = 16; # should be 1 per CPU logical core
    binaryCaches = [
      "https://all-hies.cachix.org"
      "https://cache.nixos.org/"
      "https://ghcide-nix.cachix.org"
      "https://hercules-ci.cachix.org"
      "https://iohk.cachix.org"
      "https://nix-tools.cachix.org"
      "https://nixcache.reflex-frp.org"
      "https://willbush.cachix.org"
    ];
    binaryCachePublicKeys = [
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
      "ghcide-nix.cachix.org-1:ibAY5FD+XWLzbLr8fxK6n8fL9zZe7jS+gYeyxyWYK5c="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
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
    cleanTmpDir = true; # cleans all files in /tmp during boot
    initrd.checkJournalingFS = true; # run fsck for journal file system

    loader = {
      # Timeout (in seconds) until loader boots the default menu item.
      timeout = 1;
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
    hostName = "nixos";
    networkmanager = {
      enable = true;
      pia-vpn.enable = true;
      pia-vpn.usernameFile = "/etc/pia-vpn.username";
      pia-vpn.passwordFile = "/etc/pia-vpn.password";
      pia-vpn.serverList = [
        "us-california"
        "us-east"
        "us-chicago"
        "us-texas"
        "us-seattle"
        "us-west"
        "us-siliconvalley"
        "us-newyorkcity"
        "us-lasvegas"
        "us-houston"
        "us-denver"
      ];
    };
  };

  services = {
    # Enable CUPS to print documents.
    printing.enable = true;
    printing.drivers = [ pkgs.hll2390dw-cups ];

    xserver = {
      enable = true;
      layout = "us";

      videoDrivers = [ "nvidiaBeta" ];

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
        sessionCommands = ''
          feh --bg-scale /home/will/images/retro.jpg
          albert &
        '';
      };
      desktopManager.xterm.enable = false;
    };
  };

  virtualisation.docker.enable = true;

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
    ghcide-nix.ghcide-ghc865
    (all-hies.unstable.selection { selector = p: { inherit (p) ghc865 ghc882; }; })
  ];

  programs.ssh.startAgent = true;

  system.stateVersion = "19.09";
}
