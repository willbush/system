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
    ../modules/services/docker.nix
    ../modules/services/syncthing.nix
    ../modules/services/virt.nix
    ../profiles/nixos/greetd.nix
    ../profiles/nixos/less.nix
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
        ];
      };
    };
  };

  home-manager.users.will = {
    imports = [
      ../profiles/home/bat.nix
      ../profiles/home/dotnet.nix
      ../profiles/home/emacs.nix
      ../profiles/home/git.nix
      ../profiles/home/gpg.nix
      ../profiles/home/krew.nix
      ../profiles/home/pkgs/cli.nix
      ../profiles/home/pkgs/gui.nix
      ../profiles/home/programs.nix
      ../profiles/home/swayidle.nix
      ../profiles/home/swaylock.nix
      ../profiles/home/xdg.nix
      ../profiles/home/zathura.nix
    ];

    home = {
      stateVersion = "24.11";
      inherit username;
      homeDirectory = "/home/will";

      file.".ideavimrc".text = fileContents ../configs/nvim/init.vim;
    };

    gtk.enable = true;

    wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = builtins.readFile ../configs/hypr/hyprland.conf;
    };

    # lightweight notification daemon for Wayland
    services.mako = {
      enable = true;
      # timeout in milliseconds.
      defaultTimeout = 5000;
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
  };

  programs.zsh.enable = true;

  programs.nh = {
    enable = true;
    flake = "/home/will/code/system";
    clean.enable = true;
    clean.extraArgs = "--keep-since 10d --keep 5";
  };

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

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    image = pkgs.fetchurl {
      # https://www.reddit.com/r/WidescreenWallpaper/comments/1dzli4w/untitled_7680x2160/
      url = "https://i.redd.it/1k3jwtm7zlbd1.jpeg";
      hash = "sha256-3GJ6pwrExTGJ5y+X//7iBY2ABDUkcFlxQzot24evceo=";
    };
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.11";
}
