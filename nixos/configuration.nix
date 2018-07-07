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
              haskellPackges.xmobar
          ];
      };
      windowManager.default = "xmonad";
    };

    compton = {
      enable = true;
      fade = true;
      fadeDelta = 1;
      inactiveOpacity = "0.75";
      # activeOpacity = "0.95";
    };

    redshift = {
      enable = true;
      latitude = "33";
      longitude = "-97";
      temperature.day = 6500;
      temperature.night = 2700;
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
      syntaxHighlighting.enable = true;
      interactiveShellInit = ''
alias ls=exa
alias vim=nvim
alias dropbox="docker exec -it dropbox dropbox"
alias dropbox-start="docker run -d --restart=always --name=dropbox \
  -v /home/will/Dropbox:/dbox/Dropbox \
  -v /home/will/.dropbox:/dbox/.dropbox \
  -e DBOX_UID=1000 -e DBOX_GID=100 janeczku/dropbox"
'';

      ohMyZsh = {
        enable = true;
        plugins = ["git" "vi-mode" "docker" "web-search"];
        theme = "agnoster";
      };
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    emacs
    neovim
    firefox
    keepassxc
    vlc
    synapse
    wget
    curl
    git
    ripgrep
    docker
    alacritty
    fzf
    exa
    tree
    fd
    feh
    htop
    unzip
  ];

  systemd.user.services."synapse" = {
    enable = true;
    description = "launcher to start applications and find relevant files";
    wantedBy = [ "default.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.synapse}/bin/synapse --startup";
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      hack-font
      anonymousPro
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
