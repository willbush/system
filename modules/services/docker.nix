{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf mkOption types;
  cfg = config.modules.services.docker;
in
{
  options.modules.services.docker = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    users.users."${config.user.name}".extraGroups = [ "docker" ];

    virtualisation.docker.enable = true;

    home-manager.users."${config.user.name}" = {
      home.packages = with pkgs; [
        docker
        docker-compose
      ];
    };
  };
}
