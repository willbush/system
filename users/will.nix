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

    secrets."work/aws/access_key_id" = { };
    secrets."work/aws/secret_access_key" = { };

    templates."aws-environment".content = ''
      AWS_ACCESS_KEY_ID=${config.sops.placeholder."work/aws/access_key_id"}
      AWS_SECRET_ACCESS_KEY=${config.sops.placeholder."work/aws/secret_access_key"}
      AWS_EC2_METADATA_DISABLED=true
    '';
  };

  systemd.services.nix-daemon.serviceConfig.EnvironmentFile =
    config.sops.templates."aws-environment".path;

  services.ollama = {
    enable = true;
    acceleration = "rocm";
    rocmOverrideGfx = "11.0.0";
    # pin until https://nixpk.gs/pr-tracker.html?pr=375850
    package =
      (import (builtins.fetchTarball {
        sha256 = "sha256:1hh0p0p42yqrm69kqlxwzx30m7i7xqw9m8f224i3bm6wsj4dxm05";
        url = "https://github.com/NixOS/nixpkgs/archive/d0169965cf1ce1cd68e50a63eabff7c8b8959743.tar.gz";
      }) { system = pkgs.system; }).ollama-rocm;
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
      shell.enableFishIntegration = true;
      sessionVariables = {
        SOPS_AGE_KEY_FILE = ageKeyFile;
      };
    };

    # Setup sops for home-manager
    sops = {
      defaultSopsFile = ../secrets/secrets.yaml;
      age.keyFile = "/nix/persist/home/will/.secrets/sops-nix/admin_will.txt";
    };

    gtk.enable = true;

    wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = builtins.readFile ../configs/hypr/hyprland.conf;
    };

    programs.wezterm = {
      enable = true;
      extraConfig = builtins.readFile ../configs/wezterm/wezterm.lua;
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

  modules.unfree.allowList = [
    "slack"
    "spotify"
  ];

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
