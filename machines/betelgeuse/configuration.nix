{
  imports = [ ./hardware-configuration.nix ../common-configuration.nix ];

  networking.hostName = "betelgeuse";
  services.xserver.videoDrivers = [ "nvidiaBeta" ];

  services.rpcbind.enable = true;

  boot.supportedFilesystems = [ "nfs" ];

  # fileSystems."/mnt/media" = {
  #   device = "tau-ceti:/media";
  #   fsType = "nfs";
  #   options = [
  #     "nfsvers=4.2"
  #     "x-systemd.automount"
  #     "noauto"
  #     "x-systemd.idle-timeout=600" # disconnects after 10 minutes (i.e. 600 seconds)
  #   ];
  # };
}
