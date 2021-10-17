{
  services.picom = {
    enable = true;
    fade = true;
    vSync = true;
    experimentalBackends = true;
    # the default 'glx' backend lags like crazy for me for some reason.
    backend = "xrender";
    fadeDelta = 3;
    # I only want transparency for a couple of applications.
    opacityRule = [
      "98:class_g *?= 'emacs' && focused"
      "88:class_g *?= 'emacs' && !focused"
      "98:class_g ?= 'alacritty' && focused"
      "88:class_g ?= 'alacritty' && !focused"
    ];
  };
}
