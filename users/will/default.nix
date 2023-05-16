{ inputs, config, lib, pkgs, ... }:
let inherit (lib) fileContents;
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
      root.hashedPassword = fileContents ../../secrets/hashed-password-root.txt;

      will = {
        isNormalUser = true;
        home = "/home/will";
        hashedPassword = fileContents ../../secrets/hashed-password-will.txt;
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
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEE2hQsuOQZ3PvM2DdI0vxpaBFoRQpFhGXZmeRq8Srs6 tau-ceti-2020-11-16"
        ];
      };
    };
  };

  home-manager.users.will = {
    imports = [
      (import ../profiles/emacs.nix {
        inherit inputs;
        inherit pkgs;
        emacsPackage = pkgs.emacsUnstable;
      })
      ../profiles/bat.nix
      ../profiles/dotnet.nix
      ../profiles/gpg.nix
      ../profiles/gui-theme.nix
      ../profiles/pkgs/cli.nix
      ../profiles/pkgs/gui.nix
      ../profiles/programs.nix
      ../profiles/redshift.nix
      ./git.nix
      ./krew.nix
      ./lockscreen.nix
      ./picom.nix
      ./pipx.nix
      ./pkgs/cli.nix
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
      enable = config.networking.hostName != "ton-618";
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
      enable = true;
      settings = import ../../secrets/work-globalprotect-settings.nix;
    };
  };

  programs.zsh.enable = true;

  programs.steam = {
    enable = config.networking.hostName == "betelgeuse";
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  security.pki.certificates = [
    "${builtins.readFile ../../secrets/work-cert.pem}"
  ];

  security.pam.krb5.enable = true;
  krb5 =
    {
      enable = true;
      config = builtins.readFile ../../secrets/krb5.conf;
    };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [ mullvad-vpn ];

  system.stateVersion = "23.05";
}
