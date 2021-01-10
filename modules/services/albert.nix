{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkOption types;

  cfg = config.modules.services.albert;
in {
  options.modules.services.albert = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable Albert desktop launcher.";
    };

    package = mkOption {
      type = types.package;
      default = pkgs.albert;
      defaultText = "pkgs.albert";
      description = "The albert derivation to use.";
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.albert = {
      description = "Albert is a desktop agnostic launcher";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];

      serviceConfig = {
        ExecStart = "${pkgs.albert}/bin/albert";
        RestartSec = 3;
        Restart = "always";
      };
    };

    environment.systemPackages = [ cfg.package ];
  };
}
