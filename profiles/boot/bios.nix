{ device, ... }: {
  boot = {
    tmp.cleanOnBoot = true;

    loader = {
      # Timeout (in seconds) until loader boots the default menu item.
      timeout = 2;

      grub = {
        enable = true;
        version = 2;
        inherit device;
      };
    };
  };
}
