{
  imports = [
    ./hardware-configuration.nix
    ../common-configuration.nix
  ];

  networking.hostName = "betelgeuse";
  services.xserver.videoDrivers = [ "nvidiaBeta" ];
}
