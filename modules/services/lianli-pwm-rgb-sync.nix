{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.services.lianli-pwm-rgb-sync;
  lianli-pwm-rgb-sync = inputs.lianli-pwm-rgb-sync.packages.${pkgs.system}.default;
in
{
  options.modules.services.lianli-pwm-rgb-sync = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    systemd.services.lianli-pwm-rgb-sync = {
      description = "lianli-pwm-rgb-sync service";
      wantedBy = [ "multi-user.target" ];
      before = [ "openrgb.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${lianli-pwm-rgb-sync}/bin/lianli-pwm-rgb-sync";
        AmbientCapabilities = [ "CAP_SYS_RAWIO" ];
      };
    };
  };
}
