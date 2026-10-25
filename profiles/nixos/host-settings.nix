{ ... }:
{
  time.timeZone = "America/Chicago";

  # Enable sound with pipewire.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };
  # needed for wayland
  hardware.graphics.enable = true;

  # enable non-root access to the firmware of QMK keyboards.
  hardware.keyboard.qmk.enable = true;

  # Change the default timeout for a service from 90 seconds.
  systemd.settings.Manager = {
    DefaultTimeoutStopSec = "30s";
  };
}
