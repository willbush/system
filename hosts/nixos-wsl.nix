{ lib, pkgs, config, modulesPath, ... }:

with lib;
let
  defaultUser = "will";
  syschdemd = import ./syschdemd.nix { inherit lib pkgs config defaultUser; };
in {
  # imports = [ "${modulesPath}/profiles/minimal.nix" ];
  imports = [
    ../profiles/common/nix-settings.nix
  ];

  nixpkgs.config.allowUnfree = true;

  home-manager.users.will = {
    imports = [
      (import ../users/profiles/emacs.nix {
        inherit pkgs;
        emacsPackage = pkgs.emacsGcc;
      })
    ];

    home = rec {
      stateVersion = "21.11";
      username = "will";
      homeDirectory = "/home/will";

      sessionVariables = {
        EDITOR = "emacsclient --create-frame --alternate-editor emacs";
      };

      packages = with pkgs; [ mutagen ];
    };
  };

  # WSL is closer to a container than anything else
  boot.isContainer = true;

  environment.etc.hosts.enable = false;
  environment.etc."resolv.conf".enable = false;

  networking.dhcpcd.enable = false;

  users.users.${defaultUser} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  users.users.root = {
    shell = "${syschdemd}/bin/syschdemd";
    # Otherwise WSL fails to login as root with "initgroups failed 5"
    extraGroups = [ "root" ];
  };

  security.sudo.wheelNeedsPassword = false;

  # Disable systemd units that don't make sense on WSL
  systemd.services."serial-getty@ttyS0".enable = false;
  systemd.services."serial-getty@hvc0".enable = false;
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@".enable = false;

  systemd.services.firewall.enable = false;
  systemd.services.systemd-resolved.enable = false;
  systemd.services.systemd-udevd.enable = false;

  # Don't allow emergency mode, because we don't have a console.
  systemd.enableEmergencyMode = false;
}
