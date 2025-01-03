{
  config,
  pkgs,
  ...
}:
let
  username = "will";
  ageKeyFile = "/nix/persist/home/will/.secrets/sops-nix/admin_will.txt";
in
{
  imports = [
    ../modules/services/docker.nix
    ../modules/services/lianli-pwm-rgb-sync.nix
    ../modules/services/virt.nix
    ../profiles/nixos/greetd.nix
    ../profiles/nixos/less.nix
  ];

  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    age.keyFile = ageKeyFile;

    secrets.root-password-hashed.neededForUsers = true;
    secrets.will-password-hashed.neededForUsers = true;
  };

  users = {
    mutableUsers = false;
    users = {
      root.hashedPasswordFile = config.sops.secrets.root-password-hashed.path;

      will = {
        isNormalUser = true;
        home = "/home/will";
        hashedPasswordFile = config.sops.secrets.will-password-hashed.path;
        shell = pkgs.fish;
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
      ../profiles/home/emacs.nix
      ../profiles/home/fish.nix
      ../profiles/home/git.nix
      ../profiles/home/helix.nix
      ../profiles/home/pkgs/cli.nix
      ../profiles/home/pkgs/gui.nix
      ../profiles/home/programs.nix
      ../profiles/home/starship.nix
      ../profiles/home/swayidle.nix
      ../profiles/home/swaylock.nix
      ../profiles/home/xdg.nix
      ../profiles/home/zathura.nix
    ];

    home = {
      stateVersion = "24.11";
      inherit username;
      homeDirectory = "/home/will";

      sessionVariables = {
        SOPS_AGE_KEY_FILE = ageKeyFile;
      };
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
    services.syncthing.enable = true;

    # Nicely reload system units when changing configs
    systemd.user.startServices = "sd-switch";
  };

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
    iproute2.enable = true; # Needed for mullvad daemon
    wireguard.enable = true;
  };

  modules.services = {
    virt = {
      enable = true;
      user = username;
    };
    docker = {
      enable = true;
      user = username;
    };
    lianli-pwm-rgb-sync.enable = true;
  };

  services = {
    # Enable the gnome-keyrig secrets vault. Will be exposed through DBus to
    # programs willing to store secrets.
    gnome.gnome-keyring.enable = true;

    mullvad-vpn.enable = true;
    tailscale = {
      enable = true;
      useRoutingFeatures = "client";
    };
  };

  programs.fish.enable = true;
  # important for system-wide configuration despite being installed via home-manager

  programs.hyprland.enable = true;
  programs.wireshark.enable = true;

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
    pam.services.swaylock = { };
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [ mullvad-vpn ];

  fonts.packages = with pkgs; [
    dejavu_fonts
    jigmo # Japanese Kanji font set which is the official successor to Hanazono Mincho
    nerd-fonts.noto
    nerd-fonts.symbols-only # used by emacs nerd-icons.el
    nerd-fonts.zed-mono
    noto-fonts-cjk-sans
    noto-fonts-emoji
  ];

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tokyo-night-dark.yaml";
    image = pkgs.fetchurl {
      # https://www.reddit.com/r/WidescreenWallpaper/comments/1dzli4w/untitled_7680x2160/
      url = "https://i.redd.it/1k3jwtm7zlbd1.jpeg";
      hash = "sha256-3GJ6pwrExTGJ5y+X//7iBY2ABDUkcFlxQzot24evceo=";
    };

    fonts = {
      # Explicitly setting this in-case I want to quickly increase size from the
      # defaults.
      sizes = {
        applications = 14; # default: 12
        desktop = 12; # default: 10
        popups = 12; # default: 10
        terminal = 14; # default: 12
      };
      serif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };

      sansSerif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };

      monospace = {
        package = pkgs.nerd-fonts.zed-mono;
        name = "ZedMono NFM Extd";
      };

      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.11";
}
