{ device, ... }: {
  boot = {
    cleanTmpDir = true; # cleans all files in /tmp during boot

    loader = {
      # Timeout (in seconds) until loader boots the default menu item.
      timeout = 2;

      grub = {
        enable = true;
        version = 2;
        inherit device;
        configurationLimit = 50;
        memtest86.enable = true;
      };
    };
  };
}
