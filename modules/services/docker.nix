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
    user = mkOption { type = types.str; };
  };

  config = mkIf cfg.enable {
    users.users."${cfg.user}".extraGroups = [ "docker" ];

    virtualisation.docker.enable = true;

    home-manager.users."${cfg.user}" = {
      home.packages = with pkgs; [
        docker
        docker-compose
        # lets me use `minikube start --driver=kvm2` which is useful testing
        # rook-ceph
        docker-machine-kvm2
      ];
      programs.zsh.oh-my-zsh.plugins = [
        "docker"
        "docker-compose"
      ];
    };
  };
}
