{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) fileContents;
  secretsEnabled = config.modules.secrets.enable;
  username = "will";
in
{
  imports = [
    ../modules/services/docker.nix
    ../modules/services/syncthing.nix
    ../modules/services/virt.nix
    ../profiles/greetd.nix
    ../profiles/less.nix
    ../profiles/nix-settings.nix
  ];

  users = {
    mutableUsers = false;
    users = {
      root.hashedPassword = config.modules.secrets.rootHashedPassword;

      will = {
        isNormalUser = true;
        home = "/home/will";
        hashedPassword = config.modules.secrets.willHashedPassword;
        shell = pkgs.zsh;
        extraGroups = [
          "networkmanager"
          "wheel"
          "wireshark"
        ];
      };
    };
  };

  home-manager.users.will = {
    imports = [
      inputs.catppuccin.homeManagerModules.catppuccin
      ../home-manager/bat.nix
      ../home-manager/dotnet.nix
      ../home-manager/emacs.nix
      ../home-manager/git.nix
      ../home-manager/gpg.nix
      ../home-manager/gui-theme.nix
      ../home-manager/krew.nix
      ../home-manager/pkgs/cli.nix
      ../home-manager/pkgs/gui.nix
      ../home-manager/programs.nix
      ../home-manager/swayidle.nix
      ../home-manager/swaylock.nix
      ../home-manager/xdg.nix
      ../home-manager/zathura.nix
    ];

    home = {
      stateVersion = "24.05";
      inherit username;
      homeDirectory = "/home/will";

      file.".ideavimrc".text = fileContents ../configs/nvim/init.vim;

      sessionVariables = {
        # Set docker to use BuildKit by default.
        # TODO: remove when this is the default in docker
        DOCKER_BUILDKIT = 1;
      };
    };

    # set catppuccin theme flavor for home-manager module
    catppuccin.flavor = "mocha";

    wayland.windowManager.hyprland = {
      enable = true;
      catppuccin.enable = true;
      extraConfig = builtins.readFile ../configs/hypr/hyprland.conf;
    };

    # lightweight notification daemon for Wayland
    services.mako = {
      enable = true;
      catppuccin.enable = true;
      # timeout in milliseconds.
      defaultTimeout = 5000;
    };

    # Limiter, compressor, convolver, equalizer and auto volume and many other
    # plugins for PipeWire applications
    services.easyeffects.enable = true;

    programs = {
      wofi.enable = true;
      neovim = {
        enable = true;
        catppuccin.enable = true;
        extraConfig = builtins.readFile ../configs/nvim/init.vim;
      };
    };

    # Nicely reload system units when changing configs
    systemd.user.startServices = "sd-switch";
  };

  modules.unfree.allowList = [
    "discord"
    "rider"
    "slack"
  ];

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
    iproute2.enable = true; # Needed for mullvad daemon
    wireguard.enable = true;
  };

  modules = {
    services.syncthing = {
      enable = config.modules.secrets.enable && config.networking.hostName != "ton-618";
      user = username;
    };
    services.virt = {
      enable = true;
      user = username;
    };
    services.docker = {
      enable = true;
      user = username;
    };
  };

  services = {
    # Enable the gnome-keyrig secrets vault. Will be exposed through DBus to
    # programs willing to store secrets.
    gnome.gnome-keyring.enable = true;

    mullvad-vpn.enable = true;

    # needed for globalprotect-openconnect to work
    globalprotect = {
      enable = secretsEnabled;
      settings = config.modules.secrets.globalprotectSettings;
    };
  };

  programs.zsh.enable = true;

  programs.wireshark.enable = true;

  security = {
    sudo.enable = false;
    sudo-rs.enable = true;
    sudo-rs.execWheelOnly = true;

    pki.certificates = config.modules.secrets.pkiCertificates;
    pam.services.swaylock = { };

    krb5 = {
      enable = secretsEnabled;
      settings = config.modules.secrets.krb5Settings;
    };
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [ mullvad-vpn ];

  fonts.packages = with pkgs; [
    dejavu_fonts
    emacs-all-the-icons-fonts
    fira-mono
    hack-font
    inconsolata
    iosevka
    liberation_ttf
    libre-baskerville
    nerdfonts
    powerline-fonts
    source-code-pro
    ubuntu_font_family
  ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
