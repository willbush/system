{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkOption types;
  cfg = config.modules.services.clamav;
in
{
  options.modules.services.clamav = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.clamav = {
      # enable opensource antivirus
      daemon.enable = true;
      # keep the signatures' database updated
      updater.enable = true;
    };
  };
}
