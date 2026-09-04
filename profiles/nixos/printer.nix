{ pkgs, ... }:
{
  hardware.sane = {
    enable = true;
    extraBackends = [ pkgs.sane-airscan ];
  };

  # Hyprland's setcap wrapper strips LD_LIBRARY_PATH, SANE's loader falls back to this
  environment.sessionVariables.SHLIB_PATH = "/etc/sane-libs";

  services = {
    printing.enable = true;

    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };
}
