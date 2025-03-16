{
  config,
  lib,
  pkgs,
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
          # Needed for Windows
          swtpm.enable = true;
          ovmf.enable = true;
          ovmf.packages = [ pkgs.OVMFFull.fd ];
        };
      };
      spiceUSBRedirection.enable = true;
    };
    services.spice-vdagentd.enable = true;
  };
}
