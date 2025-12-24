{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkIf mkOption types;
  cfg = config.modules.services.virt;
in
{
  options.modules.services.virt = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    users.users."${config.user.name}".extraGroups = [ "libvirtd" ];

    programs.virt-manager.enable = true;
    programs.dconf.enable = true; # virt-manager requires dconf to remember settings

    virtualisation = {
      libvirtd = {
        enable = true;

        onBoot = "ignore";
        onShutdown = "shutdown";

        qemu = {
          runAsRoot = false;
        };
      };
      spiceUSBRedirection.enable = true;
    };
    services.spice-vdagentd.enable = true;
  };
}
