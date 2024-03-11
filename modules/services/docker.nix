{ config, lib, pkgs, ... }:
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
    user = mkOption { type = types.str; };
  };

  config = mkIf cfg.enable {
    users.users."${cfg.user}".extraGroups = [ "docker" ];

    virtualisation.docker.enable = true;

    home-manager.users."${cfg.user}" = {
      home.packages = with pkgs; [
        docker
        docker-compose
      ];
      programs.zsh.oh-my-zsh.plugins = [ "docker" "docker-compose" ];
    };
  };
}
