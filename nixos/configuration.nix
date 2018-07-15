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

    # TODO learn how to control transparency per application
    # compton = {
    #   enable = true;
    #   fade = true;
    #   fadeDelta = 1;
    #   inactiveOpacity = "0.75";
    #   # activeOpacity = "0.95";
    # };

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
      shellAliases = {
        l  = "exa";
        ll = "exa -l";
        la = "exa -lah";
        vim = "nvim";
        dropbox = "docker exec -it dropbox dropbox";
        docker-start = ''
        docker run -d --restart=always --name=dropbox \
          -v /home/will/Dropbox:/dbox/Dropbox \
          -v /home/will/.dropbox:/dbox/.dropbox \
          -e DBOX_UID=1000 -e DBOX_GID=100 janeczku/dropbox'';
      };
      ohMyZsh = {
        enable = true;
        plugins = ["vi-mode" "web-search"];
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
    albert
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
    gnupg
    aspell
    aspellDicts.en
    nix-repl
    stack
    gnumake
    gcc
    binutils
    ghc
    cabal-install
    haskellPackages.cabal-install
    haskellPackages.hindent
    haskellPackages.stylish-haskell
    haskellPackages.hlint
    haskellPackages.hoogle
  ];

  # TODO figure out how to set albert up as a service
  # systemd.user.services."albert" = {
  #   enable = true;
  #   description = "A Desktop Agnostic Launcher";
  #   wantedBy = [ "graphical-session.target" ];
  #   partOf = [ "graphical-session.target" ];
  #   environment = {
  #     QT_PLUGIN_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtPluginPrefix;
  #   };
  #   serviceConfig = {
  #     Restart = "always";
  #     RestartSec = 2;
  #     # Environment = "PATH=%h/.nix-profile/bin";
  #     ExecStart = "${pkgs.albert}/bin/albert";
  #   };
  # };

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
  system.nixos.stateVersion = "18.03"; # Did you read the comment?
}
