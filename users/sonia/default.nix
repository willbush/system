{ config, pkgs, ... }:
{
  imports = [
    ../../modules/services/syncthing.nix
    ../../profiles/fonts.nix
    ../../profiles/nix-settings.nix
  ];

  users = {
    mutableUsers = false;
    users = {
      root.hashedPassword = config.modules.secrets.rootHashedPassword;

      sonia = {
        isNormalUser = true;
        home = "/home/sonia";
        hashedPassword = config.modules.secrets.soniaHashedPassword;
        shell = pkgs.zsh;
        extraGroups = [
          "wheel"
          "networkmanager"
        ];
      };
    };
  };

  home-manager.users.sonia = {
    imports = [
      ../profiles/bat.nix
      ../profiles/emacs.nix
      ../profiles/gpg.nix
      ../profiles/gui-theme.nix
      ../profiles/pkgs/cli.nix
      ../profiles/pkgs/gui.nix
      ../profiles/programs.nix
      ./git.nix
    ];

    home = rec {
      stateVersion = "24.05";
      username = "sonia";
      homeDirectory = "/home/sonia";

      packages = with pkgs; [ chromium firefox ];
    };

    xdg.enable = true;

    programs = {
      neovim.enable = true;
    };
  };

  modules.unfree.allowList = [
  ];

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
  };

  modules = {
    services.syncthing = {
      enable = config.modules.secrets.enable;
      user = "sonia";
    };
  };

  services = {
    xserver = {
      enable = true;

      desktopManager.gnome.enable = true;
      displayManager = {
        gdm = {
          enable = true;
          wayland = false;
        };
        defaultSession = "gnome";
      };
    };
  };
  # gnome desktopManager does 'pulseaudio.enable = mkDefault true'
  hardware.pulseaudio.enable = false;

  programs.less.enable = true;
  programs.zsh.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
