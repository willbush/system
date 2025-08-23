{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.services.lianli-pwm-rgb-sync;
in
{
  options.modules.services.lianli-pwm-rgb-sync = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable service to configure Lian Li Uni Fan SL-Infinity on boot.";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.lianli-pwm-rgb-sync = {
      description = "Configure Lian Li Uni Fan SL-Infinity for PWM and RGB Sync";
      wantedBy = [ "multi-user.target" ];
      before = [ "openrgb.service" ];
      # `--vidpid 0cf2:a102`:
      # Target the Lian Li controller by its Vendor ID (0cf2) and Product ID (a102).
      #
      # `--send-feature 0xe0,0x10,0x61,0x01`:
      # Enables motherboard RGB sync ("pass-through" mode).
      # Payload: [Report ID, Register, Command, Value]
      # 0x61: The "Set RGB Sync" command
      # 0x01: Enable RGB Sync
      #
      # `--send-feature 0xe0,0x10,0x62,0xff`:
      # Sets all fan channels to be controlled by PWM from the motherboard.
      # 0x62: Set Fan Mode command
      # 0xff: Set all channels to PWM mode
      script = ''
        ${pkgs.hidapitester}/bin/hidapitester \
          --vidpid 0cf2:a102 \
          --open \
          --send-feature 0xe0,0x10,0x61,0x01 \
          --send-feature 0xe0,0x10,0x62,0xff \
          --close
      '';
      serviceConfig = {
        Type = "oneshot";
      };
    };
  };
}
