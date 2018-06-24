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
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

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
    extraGroups = [ "wheel" "networkmanager" ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    emacs
    firefox
    git
    synapse
    redshift
    dropbox
    keepassxc
    hexchat
    termite
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
