{ config, lib, pkgs, ... }:
let inherit (lib) fileContents;
in {
  imports =
    [ ../../profiles/common/fonts.nix ../../profiles/common/nix-settings.nix ];
  # TODO deal with syncthing hard-coding home paths
  # imports = [ ../../fonts.nix ../../modules/services/syncthing.nix ];

  users = {
    mutableUsers = false;
    users = {
      root.hashedPassword = fileContents ../../secrets/hashed-password-root.txt;

      sonia = {
        uid = 1001;
        isNormalUser = true;
        home = "/home/sonia";
        hashedPassword = fileContents ../../secrets/hashed-password-sonia.txt;
        shell = pkgs.zsh;
        extraGroups = [ "wheel" "networkmanager" ];
      };
    };
  };

  home-manager.users.sonia = import ./home.nix;

  nixpkgs.config.allowUnfree = true;

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
  };

  # TODO
  # modules.services.syncthing.enable = true;

  # TODO
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # services.xserver = {
  #   enable = true;
  #   displayManager.sddm.enable = true;
  #   desktopManager.plasma5.enable = true;
  # };

  services = {
    # Enable CUPS to print documents.
    printing.enable = true;
    printing.drivers = [ pkgs.hll2390dw-cups ];

    xserver = {
      enable = true;
      layout = "us";

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
      desktopManager.xterm.enable = false;
    };
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [ curl fd neovim ripgrep tree wget ];

  programs = {
    ssh.startAgent = true;
    # needed for gnome / gtk themes
    dconf.enable = true;
    qt5ct.enable = true;
    gnupg.agent.enable = true;
    less.enable = true;
  };

  system.stateVersion = "20.09";
}
