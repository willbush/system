{ inputs, config, lib, pkgs, ... }:
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
      (import ../profiles/emacs {
        inherit inputs;
        inherit pkgs;
        emacsPackage = pkgs.emacsNativeComp;
      })
      ../profiles/bat.nix
      ../profiles/dotnet.nix
      ../profiles/gpg.nix
      ../profiles/gui-theme.nix
      ../profiles/pkgs/cli.nix
      ../profiles/pkgs/gui.nix
      ../profiles/programs.nix
      ../profiles/redshift.nix
      ./pkgs/cli.nix
      ./pkgs/gui.nix
      ./git.nix
      ./lockscreen.nix
      # ./picom.nix
      ./rofi.nix
      ./xdg.nix
    ];

    home = rec {
      stateVersion = "22.11";
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
    "android-studio-stable"
    "libsciter" # used by rustdesk
    "rider"
    "slack"
    "teams"
  ];

  networking = {
    firewall.enable = false;
    networkmanager.enable = true;
    iproute2.enable = true; # Needed for mullvad daemon
    wireguard.enable = true;
  };

  modules.services.syncthing = {
    enable = config.networking.hostName != "ton-618";
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
        sddm.enable = true;
      };
    };

    mullvad-vpn.enable = true;
  };

  # request-key expects a configuration file under /etc
  environment.etc."request-key.conf" = {
    text =
      let
        upcall = "${pkgs.cifs-utils}/bin/cifs.upcall";
        keyctl = "${pkgs.keyutils}/bin/keyctl";
      in
      ''
        #OP     TYPE          DESCRIPTION  CALLOUT_INFO  PROGRAM
        # -t is required for DFS share servers...
        create  cifs.spnego   *            *             ${upcall} -t %k
        create  dns_resolver  *            *             ${upcall} %k
      '';
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    mullvad-vpn
    # Doesn't *need* to be in the system profile for this to work, but we
    # want it installed so that e.g. the man pages are available
    cifs-utils
    # This *does* need to be installed in the system profile, as we link to
    # it there in the symlink-requestkey activation script defined above
    keyutils
  ];

  system.stateVersion = "22.11";
}
