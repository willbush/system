{
  imports = [ ./hardware-configuration.nix ../common-configuration.nix ];

  networking.hostName = "tau-ceti";

  services = {
    # Enable touchpad support.
    xserver.libinput.enable = true;

    openssh = {
      enable = true;
      passwordAuthentication = false;
      ports = [ 16596 ];
    };
  };
}
