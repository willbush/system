{ inputs, config, lib, pkgs, ... }:
let
  inherit (lib) fileContents;
  secretsEnabled = config.modules.secrets.enable;
in
{
  imports = [
    ../../modules/services/clamav.nix
    ../../modules/services/syncthing.nix
    ../../profiles/common/fonts.nix
    ../../profiles/common/nix-settings.nix
    ../../profiles/common/sudo-rs.nix
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
          "docker"
          "libvirtd"
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
      ./git.nix
      ./krew.nix
      ./picom.nix
      ./pipx.nix
      ./pkgs/cli.nix
      ./pkgs/gui.nix
      ./xdg.nix
    ];

    home = rec {
      stateVersion = "24.05";
      username = "will";
      homeDirectory = "/home/will";

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

    services.lorri.enable = true;

    # lightweight notification daemon for Wayland
    services.mako.enable = true;

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
      user = "will";
    };
    services.clamav.enable = true;
  };

  services = {
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

  security.krb5 = {
    enable = secretsEnabled;
    settings = config.modules.secrets.krb5Settings;
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [ mullvad-vpn ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
