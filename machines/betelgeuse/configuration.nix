{
  imports = [ ./hardware-configuration.nix ../common-configuration.nix ];

  networking.hostName = "betelgeuse";

  services = {
    fstrim.enable = true;
    xserver.videoDrivers = [ "nvidiaBeta" ];
  };

  # Needed for mount.nfs command (when manually mounting)
  boot.supportedFilesystems = [ "nfs" ];

  fileSystems."/mnt/media" = {
    device = "tau-ceti:/media";
    fsType = "nfs";
    options = [
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=600" # disconnects after 10 minutes (i.e. 600 seconds)
    ];
  };
}
