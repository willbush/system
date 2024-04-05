{
  config,
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
    ../../modules/services/syncthing.nix
    ../../modules/services/virt.nix
    ../../modules/services/docker.nix
    ../../profiles/fonts.nix
    ../../profiles/nix-settings.nix
    ../../profiles/sudo-rs.nix
    ./greetd.nix
    ./less.nix
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
      ../profiles/bat.nix
      ../profiles/dotnet.nix
      ../profiles/emacs.nix
      ../profiles/gpg.nix
      ../profiles/gui-theme.nix
      ../profiles/pkgs/cli.nix
      ../profiles/pkgs/gui.nix
      ../profiles/programs.nix
      ../profiles/swaylock.nix
      ../profiles/swayidle.nix
      ./git.nix
      ./pkgs/cli.nix
      ./pkgs/gui.nix
      ./xdg.nix
    ];

    home = {
      stateVersion = "24.05";
      inherit username;
      homeDirectory = "/home/will";

      file.".ideavimrc".text = fileContents ../../configs/nvim/init.vim;

      sessionVariables = {
        # Set docker to use BuildKit by default.
        # TODO: remove when this is the default in docker
        DOCKER_BUILDKIT = 1;
      };
    };

    wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = builtins.readFile ../../configs/hypr/hyprland.conf;
    };

    # lightweight notification daemon for Wayland
    services.mako.enable = true;
    # timeout in milliseconds.
    services.mako.defaultTimeout = 5000;

    # Limiter, compressor, convolver, equalizer and auto volume and many other
    # plugins for PipeWire applications
    services.easyeffects.enable = true;

    programs = {
      wofi.enable = true;
      neovim = {
        enable = true;
        extraConfig = builtins.readFile ../../configs/nvim/init.vim;
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

  security.pki.certificates = config.modules.secrets.pkiCertificates;
  security.pam.services.swaylock = { };

  security.krb5 = {
    enable = secretsEnabled;
    settings = config.modules.secrets.krb5Settings;
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [ mullvad-vpn ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
