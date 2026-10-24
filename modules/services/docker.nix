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

    virtualisation.docker = {
      enable = true;
      # Start on first use via docker.socket. On boot it pulls in
      # network-online.target, which stalls greetd.
      enableOnBoot = false;
    };

    home-manager.users."${config.user.name}" = {
      home.packages = with pkgs; [
        docker
        docker-compose
      ];
    };
  };
}
