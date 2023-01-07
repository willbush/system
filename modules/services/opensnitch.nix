{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkOption types;
  cfg = config.modules.services.opensnitch;
in
{
  options.modules.services.opensnitch = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    user = mkOption { type = types.str; };
  };

  config = mkIf cfg.enable {
    services.opensnitch = {
      enable = true;
      rules = {

        rustup = {
          name = "rustup";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.rustup}/bin/.rustup-wrapped";
          };
        };

        octant = {
          name = "octant";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.octant}/bin/octant";
          };
        };

        clamav = {
          name = "clamav";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.clamav}/bin/freshclam";
          };
        };

        avahi = {
          name = "avahi";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.avahi}/bin/avahi-daemon";
          };
        };

        mullvad = {
          name = "mullvad";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.mullvad}/bin/.mullvad-daemon-wrapped";
          };
        };

        syncthing = {
          name = "syncthing";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.syncthing}/bin/syncthing";
          };
        };

        systemd-timesyncd = {
          name = "systemd-timesyncd";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.systemd}/lib/systemd/systemd-timesyncd";
          };
        };

        dnsutils = {
          name = "dnsutils";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.dnsutils}/bin/*";
          };
        };

        iputils = {
          name = "iputils";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.iputils}/bin/*";
          };
        };

        kubectl = {
          name = "kubectl";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.kubectl}/bin/kubectl";
          };
        };

        networkmanager = {
          name = "networkmanager";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.networkmanager}/bin/NetworkManager";
          };
        };

        slack = {
          name = "slack";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.slack}/lib/slack/slack";
          };
        };

        argocd = {
          name = "argocd";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.argocd}/bin/argocd";
          };
        };

        nscd = {
          name = "nscd";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${lib.getBin pkgs.glibc}/bin/nscd";
          };
        };

        nix = {
          name = "nix";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.nix}/bin/nix";
          };
        };

        openconnect = {
          name = "openconnect";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.openconnect}/bin/openconnect";
          };
        };

        ssh = {
          name = "ssh";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.openssh}/bin/ssh";
          };
        };

        spotify = {
          name = "spotify";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.spotify}/share/spotify/.spotify-wrapped";
          };
        };

        cups-browsed = {
          name = "cups-browsed";
          enabled = true;
          action = "allow";
          duration = "always";
          operator = {
            type = "simple";
            sensitive = false;
            operand = "process.path";
            data = "${pkgs.cups-filters}/bin/cups-browsed";
          };
        };
      };
    };

    home-manager.users."${cfg.user}" = {
      services.opensnitch-ui.enable = true;
    };
  };
}
