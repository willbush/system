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
          "dialout"
          "docker"
          "libvirtd"
          "networkmanager"
          "tty" # For Arduino ESP32
          "wheel"
          "wireshark"
        ];
      };
    };
  };

  home-manager.users.will = {
    imports = [
      (import ../profiles/emacs.nix {
        inherit inputs;
        inherit pkgs;
        emacsPackage = pkgs.emacs-unstable;
      })
      ../profiles/bat.nix
      ../profiles/dotnet.nix
      (import ../profiles/gpg.nix { inherit pkgs; })
      ../profiles/gui-theme.nix
      ../profiles/pkgs/cli.nix
      ../profiles/pkgs/gui.nix
      ../profiles/programs.nix
      ../profiles/redshift.nix
      (import ./git.nix {
        inherit pkgs;
        inherit config;
      })
      ./krew.nix
      (import ./wallpaper.nix {
        inherit pkgs;
        inherit config;
      })
      ./picom.nix
      ./pipx.nix
      (import ./pkgs/cli.nix {
        inherit pkgs;
        inherit config;
      })
      ./pkgs/gui.nix
      ./rofi.nix
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

      file = {
        ".config".source = ../../config;
        ".config".recursive = true;
        ".xmonad/xmonad.hs".source = ../../xmonad/xmonad.hs;
      };
    };

    services.lorri.enable = true;

    programs = {
      neovim = {
        enable = true;
        extraConfig = builtins.readFile ../../nvim/init.vim;
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
    xserver = {
      dpi = 110;
      enable = true;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackges: [
          haskellPackges.xmonad-contrib
          haskellPackges.xmonad-extras
          haskellPackges.xmonad
        ];
      };

      displayManager = {
        defaultSession = "none+xmonad";
        sddm.enable = true;
      };
    };

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
