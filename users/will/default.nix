{ config, lib, pkgs, ... }:
let inherit (lib) fileContents;
in
{
  imports = [
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
        extraGroups = [ "wheel" "networkmanager" "docker" "libvirtd" ];
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEE2hQsuOQZ3PvM2DdI0vxpaBFoRQpFhGXZmeRq8Srs6 tau-ceti-2020-11-16"
        ];
      };
    };
  };

  home-manager.users.will = {
    imports = [
      (import ../profiles/emacs.nix {
        inherit pkgs;
        emacsPackage = pkgs.emacsGcc;
      })
      ../profiles/bat.nix
      ../profiles/gpg.nix
      ../profiles/gtk.nix
      ../profiles/pkgs/cli.nix
      ../profiles/pkgs/gui.nix
      ../profiles/programs.nix
      ../profiles/redshift.nix
      ./picom.nix
      ./pkgs/cli.nix
      ./pkgs/gui.nix
      ./rofi.nix
      ./xdg.nix
    ];

    home = rec {
      stateVersion = "21.11";
      username = "will";
      homeDirectory = "/home/will";

      sessionVariables = {
        EDITOR = "emacsclient --create-frame --alternate-editor emacs";
        DOTNET_CLI_TELEMETRY_OPTOUT = 1;
      };

      file = {
        ".config".source = ../../config;
        ".config".recursive = true;
        ".xmonad/xmonad.hs".source = ../../xmonad/xmonad.hs;
      };
    };

    services.lorri.enable = true;
  };

  modules.unfree.allowList = [ "discord" "slack" "teams" ];

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
    iproute2.enable = true; # Needed for mullvad daemon
    wireguard.enable = true;
  };

  modules.services.syncthing = {
    enable = true;
    user = "will";
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
        lightdm.enable = true;
      };
    };

    mullvad-vpn.enable = true;
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    mullvad-vpn
  ];

  programs = {
    # needed for gnome / gtk themes
    dconf.enable = true;
    qt5ct.enable = true;
  };

  system.stateVersion = "21.11";
}
