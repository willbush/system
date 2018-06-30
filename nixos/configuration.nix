# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # Allow unfree, which is required for some drivers.
  nixpkgs.config.allowUnfree = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Use the GRUB 2 boot loader.
  boot = {
    cleanTmpDir = true; # cleans all files in /tmp during boot
    loader.grub.enable = true;
    loader.grub.version = 2;
    # Define on which hard drive you want to install Grub.
    loader.grub.device = "/dev/sda"; # or "nodev" for efi only
    loader.grub.useOSProber = true; # autodetect installed OSes
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  networking.hostName = "nixos"; # Define your hostname.

  # List services that you want to enable:
  services = {
    # Enable CUPS to print documents.
    # printing.enable = true;

    # Enable GeoClue, which redshift uses.
    geoclue2.enable = true;

    xserver = {
      # Enable the X11 windowing system.
      enable = true;
      layout = "us";
      xkbOptions = "terminate:ctrl_alt_bksp, caps:swapescape";

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
        auto = {
            enable = true;
            user = "will";
        };
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.will = {
    isNormalUser = true;
    home = "/home/will";
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "docker"];
    shell = pkgs.zsh;
  };

  virtualisation.docker.enable = true;

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      interactiveShellInit = ''
alias dropbox="docker exec -it dropbox dropbox"
alias dropbox-start="docker run -d --restart=always --name=dropbox \
  -v /home/will/Dropbox:/dbox/Dropbox \
  -v /home/will/.dropbox:/dbox/.dropbox \
  -e DBOX_UID=1000 -e DBOX_GID=100 janeczku/dropbox"
'';

      ohMyZsh = {
        enable = true;
        plugins = ["git" "vi-mode"];
        theme = "agnoster";
      };
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    emacs
    firefox
    git
    ripgrep
    redshift
    keepassxc
    docker
    alacritty
    fzf
    #synapse
    #hexchat
  ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      anonymousPro
      hack-font
      corefonts
      dejavu_fonts
      font-droid
      freefont_ttf
      google-fonts
      inconsolata
      liberation_ttf
      powerline-fonts
      source-code-pro
      terminus_font
      ttf_bitstream_vera
      ubuntu_font_family
    ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
