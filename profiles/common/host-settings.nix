{ lib, ... }:
{
  time.timeZone = "America/Chicago";

  sound.enable = true;
  # Enable sound with pipewire.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;

  # Change the default timeout for a service from 90 seconds.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=30s
  '';

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
