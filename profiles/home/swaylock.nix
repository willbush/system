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
      effect-blur = "7x5";
      effect-vignette = "0.5:0.5";
      # grace = 2; ## TODO something is preventing swaylock from locking.
      fade-in = 0.2;
    };
  };
}
