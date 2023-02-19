{ inputs, lib, pkgs, config, modulesPath, ... }:

with lib;
let
  # Use the default user given by https://github.com/Trundle/NixOS-WSL because
  # if bootstrapping my config from theirs involves changing the user name, then
  # the new name will have uid 1001. However, WSL default mount options assume
  # user uid is 1000:
  # https://docs.microsoft.com/en-us/windows/wsl/wsl-config#per-distribution-configuration-options-with-wslconf
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
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ];
  };

  home-manager.users.nixos = {
    imports = [
      (import ../users/profiles/emacs.nix {
        inherit inputs;
        inherit pkgs;
        emacsPackage = pkgs.emacsUnstable;
      })
      ../users/profiles/bat.nix
      ../users/profiles/gpg.nix
      ../users/profiles/pkgs/cli.nix
      ../users/profiles/programs.nix
      ../users/will/git.nix
      ../users/will/pkgs/cli.nix
      ../users/will/xdg.nix
    ];

    home = rec {
      stateVersion = "23.05";
      username = defaultUser;
      homeDirectory = "/home/${defaultUser}";

      sessionVariables = {
        EDITOR = "em";
      };

      file = {
        ".config".source = ../config;
        ".config".recursive = true;
      };
    };

    # Add additional zsh settings for WSL2.
    programs.zsh = {
      localVariables = {
        LIBGL_ALWAYS_INDIRECT = 1;
      };

      shellAliases = {
        emc = ''
          export DISPLAY=$(ip route | awk '/^default/{print $3; exit}'):0.0
          setsid emacsclient --create-frame --alternate-editor emacs
        '';
      };
    };
  };

  # WSL is closer to a container than anything else
  boot.isContainer = true;

  environment.etc.hosts.enable = false;
  # Only disable resolv.conf generation when allowing wsl to generate it
  # environment.etc."resolv.conf".enable = false;

  networking = import ../secrets/work-network-settings.nix;

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

      firewall.enable = true;
      systemd-resolved.enable = false;
      systemd-udevd.enable = false;
    };
  };

  system.stateVersion = "23.05";
}
