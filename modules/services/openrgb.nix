{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.services.openrgb;
  orgb-package = pkgs.openrgb-with-all-plugins;
in
{
  options.modules.services.openrgb = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    services.hardware.openrgb = {
      enable = true;
      package = orgb-package;
      motherboard = "amd";
    };

    systemd.user.services.openrgb-profile-loader = {
      description = "openrgb with primary profile";
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${orgb-package}/bin/openrgb --profile primary";
        Type = "oneshot";
      };
    };
  };
}
