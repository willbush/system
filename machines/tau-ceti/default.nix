{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "tau-ceti";
  # Enable touchpad support.
  services.xserver.libinput.enable = true;
}
