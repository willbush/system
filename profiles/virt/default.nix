{ pkgs, ... }:
{
  # Requires user to add:
  # users.users.<username>.extraGroups = [ "libvirtd" "docker" ];

  environment.systemPackages = with pkgs; [
    spice
    spice-gtk
    spice-protocol
    win-virtio
    win-spice
  ];

  programs.virt-manager.enable = true;
  programs.dconf.enable = true; # virt-manager requires dconf to remember settings

  virtualisation = {
    docker.enable = true;
    libvirtd = {
      enable = true;
      qemu = {
        # Needed for Windows
        swtpm.enable = true;
        ovmf.enable = true;
        ovmf.packages = [ pkgs.OVMFFull.fd ];
      };
    };
    spiceUSBRedirection.enable = true;
  };
  services.spice-vdagentd.enable = true;
}
