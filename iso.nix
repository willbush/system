{ modulesPath, pkgs, ... }:

{
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-graphical-base.nix"
    ./fonts.nix
  ];

  isoImage.edition = "plasma5";

  nix = {
    # Required until nix version 2.4 for nix flakes
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  services = {
    xserver = {
      desktopManager.plasma5.enable = true;

      # Automatically login as nixos.
      displayManager = {
        sddm.enable = true;
        autoLogin = {
          enable = true;
          user = "nixos";
        };
      };
    };
    # Nice to have when running in a virtualized environment with spice.
    spice-vdagentd.enable = true;
  };

  users.users.nixos.shell = pkgs.zsh;

  environment.systemPackages = with pkgs; [ git mkpasswd ripgrep tree wget ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.nixos = { pkgs, ... }: {
      imports = [
        (import ./emacs.nix {
          inherit pkgs;
          emacsPackage = pkgs.emacsGit;
        })
      ];

      home.file = {
        # Include the readme from this repo which might be handy before cloning it.
        "Desktop/readme.org".source = ./readme.org;
      };

      programs = {
        starship.enable = true;
        alacritty.enable = true;

        zsh = {
          enable = true;
          enableCompletion = true;
          enableAutosuggestions = true;
        };
      };
    };
  };

  system.activationScripts.installerDesktop = let

    # Comes from documentation.nix when xserver and nixos.enable are true.
    manualDesktopFile =
      "/run/current-system/sw/share/applications/nixos-manual.desktop";

    homeDir = "/home/nixos/";
    desktopDir = homeDir + "Desktop/";

  in ''
    mkdir -p ${desktopDir}
    chown nixos ${homeDir} ${desktopDir}

    ln -sfT ${manualDesktopFile} ${desktopDir + "nixos-manual.desktop"}
    ln -sfT ${pkgs.alacritty}/share/applications/Alacritty.desktop ${
      desktopDir + "Alacritty.desktop"
    }
    ln -sfT ${pkgs.emacsGit}/share/applications/emacsclient.desktop ${
      desktopDir + "emacsclient.desktop"
    }
  '';
}
