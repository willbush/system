{ config, pkgs, ... }: {
  imports = [ ../users/sonia ];

  networking.hostName = "bellatrix";

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda2";
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
}
