{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./fonts.nix
    ./users.nix
  ];

  # Allow unfree, which is required for some drivers.
  nixpkgs.config.allowUnfree = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  boot = {
    cleanTmpDir = true; # cleans all files in /tmp during boot

    loader = {
      # Use the systemd-boot EFI boot loader.
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    # I was fooling around with elasticsearch in docker and needed to
    # set this to get it to work. figured I would leave it after
    # testing. see the following for what this does:
    # https://www.kernel.org/doc/Documentation/sysctl/vm.txt
    kernel.sysctl."vm.max_map_count" = 262144;
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  networking.hostName = "nixos";

  services = {
    # Enable CUPS to print documents.
    printing.enable = true;
    printing.drivers = [ pkgs.hll2390dw-cups ];

    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "caps:swapescape";

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
      windowManager.default = "xmonad";

      displayManager = {
        lightdm.enable = true;
        sessionCommands = ''
          feh --bg-scale /home/will/Images/retro.jpg &
          albert &
        '';
      };
      desktopManager.xterm.enable = false;
    };

    emacs = {
      enable = true;
      defaultEditor = true;
    };
  };

  virtualisation.docker.enable = true;

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    alacritty
    curl
    docker
    fd
    libreoffice
    neovim
    ripgrep
    tree
    unzip
    wget
  ];

  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
