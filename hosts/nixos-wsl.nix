{ lib, pkgs, config, modulesPath, ... }:

with lib;
let
  # Use the default user given by https://github.com/Trundle/NixOS-WSL because
  # bootstrapping my config from theirs involves changing the user name, then
  # the new name will have uid 1001. However, WSL default mount options assume
  # user uid is 1000:
  # https://docs.microsoft.com/en-us/windows/wsl/wsl-config#per-distribution-configuration-options-with-wslconf
  # A user with a different uid will run into permission issues when copying
  # files to WSL from Windows.
  defaultUser = "nixos";
  syschdemd = import ./nixos-wsl/syschdemd.nix { inherit lib pkgs config defaultUser; };
in
{
  imports = [
    ../modules/unfree.nix
    ../profiles/common/fonts.nix
    ../profiles/common/nix-settings.nix
    ../users/will/less.nix
    ./nixos-wsl/build-tarball.nix
  ];

  users.users.${defaultUser} = {
    uid = 1000;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  home-manager.users.nixos = {
    imports = [
      (import ../users/profiles/emacs.nix {
        inherit pkgs;
        emacsPackage = pkgs.emacsGcc;
      })
      ../users/profiles/bat.nix
      ../users/profiles/gpg.nix
      ../users/profiles/pkgs/cli.nix
      ../users/profiles/pkgs/cli.nix
      ../users/profiles/programs.nix
      ../users/will/pkgs/cli.nix
      ../users/will/xdg.nix
    ];

    home = rec {
      stateVersion = "21.11";
      username = defaultUser;
      homeDirectory = "/home/${defaultUser}";

      sessionVariables = {
        EDITOR = "emacsclient --create-frame --alternate-editor emacs";
      };

      file = {
        ".config".source = ../config;
        ".config".recursive = true;
      };
    };
  };

  # WSL is closer to a container than anything else
  boot.isContainer = true;

  environment.etc.hosts.enable = false;
  environment.etc."resolv.conf".enable = false;

  networking.dhcpcd.enable = false;

  users.users.root = {
    shell = "${syschdemd}/bin/syschdemd";
    # Otherwise WSL fails to login as root with "initgroups failed 5"
    extraGroups = [ "root" ];
  };

  security.sudo.wheelNeedsPassword = false;

  systemd = {
    # Don't allow emergency mode, because we don't have a console.
    enableEmergencyMode = false;

    # Disable systemd units that don't make sense on WSL
    services = {
      "serial-getty@ttyS0".enable = false;
      "serial-getty@hvc0".enable = false;
      "getty@tty1".enable = false;
      "autovt@".enable = false;

      firewall.enable = false;
      systemd-resolved.enable = false;
      systemd-udevd.enable = false;
    };
  };
}
