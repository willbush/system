{ inputs, config, lib, pkgs, ... }:
let
  inherit (lib) fileContents;
  secretsEnabled = config.modules.secrets.enable;
in
{
  imports = [
    ../../modules/services/clamav.nix
    ../../modules/services/opensnitch
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
          "wheel"
          "networkmanager"
          "docker"
          "libvirtd"
          # For Arduino ESP32
          "tty"
          "dialout"
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
      ../profiles/gpg.nix
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
      stateVersion = "23.05";
      username = "will";
      homeDirectory = "/home/will";

      sessionVariables = {
        EDITOR = "emacsclient --create-frame --alternate-editor emacs";
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
  };

  modules.unfree.allowList = [
    "rider"
    "slack"
    "steam"
    "steam-original"
    "steam-run"
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

    services.opensnitch = {
      enable = false;
      user = "will";
    };
  };

  services = {
    xserver = {
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

  programs.steam = {
    enable = config.networking.hostName == "blazar";
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  security.pki.certificates = config.modules.secrets.pkiCertificates;

  security.pam.krb5.enable = secretsEnabled;
  krb5 = {
    enable = secretsEnabled;
    config = config.modules.secrets.krb5Config;
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [ mullvad-vpn ];

  system.stateVersion = "23.05";
}
