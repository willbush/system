{
  time.timeZone = "America/Chicago";

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  networking.useDHCP = false;

  # Change the default timeout for a service from 90 seconds.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=30s
  '';
}
