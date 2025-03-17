{
  config,
  pkgs,
  ...
}:
let
  ageKeyFile = "/nix/persist/home/will/.secrets/sops-nix/admin_will.txt";
in
{
  imports = [
    ../modules/user.nix
    ../modules/services/docker.nix
    ../modules/services/lianli-pwm-rgb-sync.nix
    ../modules/services/virt.nix
    ../profiles/mixed/theme.nix
    ../profiles/mixed/wayland.nix
    ../profiles/nixos/greetd.nix
    ../profiles/nixos/less.nix
  ];

  user.name = "will";

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
      ../profiles/home/xdg.nix
      ../profiles/home/yazi.nix
      ../profiles/home/zathura.nix
    ];

    home = {
      stateVersion = "24.11";
      username = config.user.name;
      homeDirectory = "/home/will";
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

    programs.wezterm = {
      enable = true;
      extraConfig = builtins.readFile ../configs/wezterm/wezterm.lua;
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
    wireguard.enable = true;
  };

  modules.services = {
    virt.enable = true;
    docker.enable = true;
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
    resolved.enable = true;
  };

  programs.fish.enable = true;
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
  };

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

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.11";
}
