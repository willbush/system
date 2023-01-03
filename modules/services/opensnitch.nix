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
    services.opensnitch.enable = true;
    home-manager.users."${cfg.user}" = {
      services.opensnitch-ui.enable = true;
    };
  };
}
