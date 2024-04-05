{ pkgs, ... }:
{
  # requires `security.pam.services.swaylock = { };` at the system level or else
  # unlock will not work.
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      indicator-radius = 100;
      indicator-thickness = 7;
      text-color = "eaebf1";
      effect-blur = "7x5";
      effect-vignette = "0.5:0.5";
      ring-color = "363b74";
      key-hl-color = "673888";
      line-color = "00000000";
      inside-color = "05050b";
      separator-color = "00000000";
      grace = 2;
      fade-in = 0.2;
    };
  };
}
