{
  services = {
    picom = {
      enable = true;
      fade = true;
      vSync = true;
      experimentalBackends = true;
      # the default 'glx' backend lags like crazy for me for some reason.
      backend = "xrender";
      fadeDelta = 1;
      # I only want transparency for a couple of applications.
      opacityRule = [
        "95:class_g *?= 'emacs' && focused"
        "75:class_g *?= 'emacs' && !focused"
        "90:class_g ?= 'alacritty' && focused"
        "75:class_g ?= 'alacritty' && !focused"
      ];
    };

    redshift = {
      enable = true;
      latitude = "33";
      longitude = "-97";
      temperature.day = 6500;
      temperature.night = 3000;
    };
  };
}
