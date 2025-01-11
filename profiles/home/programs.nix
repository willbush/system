{ ... }:
{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    alacritty = {
      enable = true;
      settings = {
        keyboard.bindings = [
          {
            key = "Plus";
            # alacritty thinks I'm pressing shift even though I'm not. Perhaps
            # something to do with QMK firmware.
            # https://github.com/alacritty/alacritty/issues/6496
            mods = "Control | Shift";
            action = "IncreaseFontSize";
          }
          {
            key = "Minus";
            mods = "Control";
            action = "DecreaseFontSize";
          }
        ];
      };
    };
    bat.enable = true;
    bottom.enable = true; # command: btm
    btop.enable = true;
    htop.enable = true;
    imv.enable = true; # command line image viewer intended for use with tiling window managers.
    rbw.enable = true;
    skim.enable = true;
    wofi.enable = true;
    yazi.enable = true;
    zoxide.enable = true;
  };
}
