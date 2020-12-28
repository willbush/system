{ pkgs, ... }:

{
  imports = [ ./fonts.nix ];

  isoImage.edition = "plasma5";

  services.xserver = {
    desktopManager.plasma5 = { enable = true; };

    # Automatically login as nixos.
    displayManager = {
      sddm.enable = true;
      autoLogin = {
        enable = true;
        user = "nixos";
      };
    };
  };

  users.users.nixos.shell = pkgs.zsh;

  environment.systemPackages = with pkgs; [ git ripgrep tree wget mkpasswd ];

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
        # Include this repository in the ISO so it doesn't need to be cloned.
        "system".source = ./.;
        "system".recursive = true;
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
    ln -sfT ${pkgs.gparted}/share/applications/gparted.desktop ${
      desktopDir + "gparted.desktop"
    }
    ln -sfT ${pkgs.alacritty}/share/applications/Alacritty.desktop ${
      desktopDir + "Alacritty.desktop"
    }
    ln -sfT ${pkgs.emacsGit}/share/applications/emacsclient.desktop ${
      desktopDir + "emacsclient.desktop"
    }
  '';
}
