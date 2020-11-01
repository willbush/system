{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "betelgeuse";
  services.xserver.videoDrivers = [ "nvidiaBeta" ];
}
