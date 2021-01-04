{
  boot = {
    cleanTmpDir = true; # cleans all files in /tmp during boot

    loader = {
      # Timeout (in seconds) until loader boots the default menu item.
      timeout = 2;
      efi.canTouchEfiVariables = true;
      # Use the systemd-boot EFI boot loader.
      systemd-boot = {
        enable = true;
        configurationLimit = 50;
        memtest86.enable = true;
        # Fixes a security hole in place for the sake of backwards
        # compatibility. See description in:
        # nixpkgs/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
        editor = false;
      };
    };
  };

  modules.unfree.allowList = [ "memtest86-efi" ];
}
